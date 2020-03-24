#' Create a recorder function for  learnr
#'
#' Recorder functions are  called automatically by `{learnr}` in
#' response  to an event. There is a standard set of arguments for
#' any recorder function. `make_record()`  creates a recorder
#' function that can  be used.
#'
#' @param store_fun A call to one of the constructors for a
#' storage mode. Possibilities:
#' - `cat_event()`
#' - `in_google_sheets(key)` where `key` is the identifier
#' for the particular Google Sheet used for
#' storage of submissions.
#'
#'
#' @importFrom utils capture.output
#'
#' @export
make_recorder <- function(store_fun = cat_event()) {
  # Create a unique ID for the  IDs submitted in this  session.
  session_id <- make_session_id()

  # define a function  with the standard set of  arguments
  #  for a learnr  recorder.
  res <- function(tutorial_id,
                  tutorial_version,
                  user_id,
                  event, data) {
    cat("in top-level  recorder\n")

    this_event <-
      data.frame(time = date(), user_id = user_id,
                 session_id = session_id,
                 markr_id = get_ID(),
                 event = event,
                 tutorial_id = tutorial_id,
                 tutorial_version = tutorial_version,
                 chunk_label = ifelse(is.null(data$label), "", data$label),
                 submission = capture.output(data$answer),
                 correct = ifelse(is.null(data$correct), "", data$correct),
                 details = as.character(jsonlite::toJSON(data)),
                 stringsAsFactors = FALSE)

    # Don't store the output of chunks -- it can be arbitrarily long.
    if  ( ! event %in% c("exercise_result")) {
      store_fun(this_event)
    }

    #  Return something to indicate success?
  }

  res # return the function just created
}


#' Constructors for modes for storage of submissions
#'
#' @param key The google sheet ID
#' @export
in_google_sheets  <-  function(key) {
  # Authorize the request
  googledrive::drive_auth(cache = ".secrets")#,  email = email)
  googlesheets4::sheets_auth(token = googledrive::drive_token())
  function(this_event) {
    cat("In google sheets recorder\n")
    suppressMessages(
      googlesheets4::sheets_append(
        this_event,
        key)
    )
  }
}
# Display the event in the console
#' @export
cat_event <- function(key) {
  function(this_event) {
    cat("Submission event from  user", this_event$markr_id,
        "in session", this_event$session_id ,"\n")
    cat("\tchunk label:", this_event$chunk_label,
        ":: correct:", this_event$correct, "\n")
    cat(this_event$submission, "\n\n")
  }
}
