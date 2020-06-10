#' Create a recorder function for learnr
#'
#' This function is used internally and is not intended for authors (or users)
#' of tutorials. Recorder functions are  called automatically by `{learnr}` in
#' response  to an event. There is a standard set of arguments for
#' any recorder function. The function constructs another function which
#' will be the one actually used by `{learnr}`.
#'
#'
#' @param store_fun A call to one of the constructors for a
#' storage mode. Possibilities:
#' - `record_local(filename)`
#' - `record_gs4(key, email, vfun)` where `key` is the identifier
#' for the particular Google Sheet to be used in account `email`. The `vfun` function
#' handles authentication.
#' @param submitr_id The user ID as authenticated during login.
#
#'
#' @importFrom utils capture.output
#'
make_a_recorder <- function(store_fun, submitr_id) {
  # Create a unique ID for the  IDs submitted in this  session.
  session_id <- make_session_id()

  # define a function  with the standard set of  arguments
  #  for a learnr  recorder.
  res <- function(tutorial_id,
                  tutorial_version,
                  user_id,
                  event, data) {
    # Put the elements of data into a data frame with always
    # the same names
    data$video_time <- data$time # give it a better name
    label <- ifelse("label" %in% names(data), data$label, "NA")
    comment <- val1 <- val2 <- "NA"
    val3 <- paste(names(data), collapse = "::") # for development purposes
    comment <- ifelse("reset" %in% names(data), "reset", comment)
    if ("video_url" %in% names(data) ) {
      comment <- "video"
      val1 <- data$video_url
      val2 <- data$video_time
    } else if ("code" %in% names(data)) {
      comment <- "code"
      val1 <- data$code
    } else if ("submission" %in% names(data)){
      comment = "code submission"
    } else if ("sectionId" %in% names(data)) {
      comment <- "section"
      val1 <- data$sectionId
    } else if ("question" %in% names(data)) {
      comment <- "question"
      if ("answer" %in% names(data) && "correct" %in% names(data))
        val1 <- paste(data$answer, ":::", data$correct)
      else val1 <- "reset"
      val2 <- data$question
    } else {
      comment = "handled field"
      val1 <- paste(names(data), collapse = "::")
    }

    if (event == "exercise_result") {
      val2 <- paste(data$checked, ":::", data$feedback)
    }

    this_event <-
      data.frame(time = date(),
                 id = paste(submitr_id, user_id),
                 session_id = session_id,
                 event = event,
                 tutorial = paste(label, tutorial_id, tutorial_version),
                 comment = comment,
                 val1 = val1,
                 val2 = val2,
                 val3 = val3,
                 stringsAsFactors = FALSE)

    ss <- store_fun(this_event)
    #  Return something to indicate success?
    ss
  }

  res # return the function just created
}


#' Constructors for modes for storage of submissions
#'





