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
make_recorder <- function(store_fun = cat_event(),  markr_id ="anonymous") {
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
    data_names <- c("label", "question", "answer", "id",
                    "correct", "code", "index", "reset",
                    "sectionId", "video_url", "video_time")
    data_in_standard_format <- list()
    data_in_standard_format[data_names] <- NA
    actual_names <- intersect(data_names, names(data))
    data_in_standard_format[actual_names] <- data[actual_names]

    this_event <-
      data.frame(time = date(), user_id = user_id,
                 session_id = session_id,
                 markr_id = markr_id,  #get_ID(),
                 event = event,
                 tutorial_id = tutorial_id,
                 tutorial_version = tutorial_version,
                 # chunk_label = ifelse(is.null(data$label), "", data$label),
                 # submission = capture.output(data$answer),
                 # correct = ifelse(is.null(data$correct), "", data$correct),
                 stringsAsFactors = FALSE)

    # Don't store the output of chunks -- it can be arbitrarily long.
    if  ( ! event %in% c("exercise_result")) {
      ss <- store_fun(cbind(this_event, data_in_standard_format))
    }

    #  Return something to indicate success?
    ss
  }

  res # return the function just created
}


#' Constructors for modes for storage of submissions
#'
#' @param key The google sheet ID
#' @param email Character string with the google email address
#' that corresponds to the key.
#'
#' @export
in_google_sheets  <-  function(key, email) {

  initiated <- FALSE # shared among all three functions

  do_initialization <- function() {
    # Authorize the request
    initiated <<- TRUE
    googledrive::drive_auth(cache = ".secrets", use_oob = TRUE, email = email)
    res <- googlesheets4::sheets_auth(token = googledrive::drive_token())
    res
  }
  write <- function(this_event) {

    if (!initiated) tmp <- do_initialization()

    res <- suppressMessages(
      googlesheets4::sheet_append(
        this_event,
        key)
    )
    return(res)
  }
  read_submissions <- function(fname) {
    if (!initiated) do_initialization()
    contents <- googlesheets4::sheets_read(key)
    write.csv(contents, file = fname, row.names=FALSE)
  }

  list(write = write, read_submissions = read_submissions)
}

# Write to a local  file
#' @export
in_local_file <- function(key, email = "") {
  # email is a dummy argument so that all  the recorders have the
  # same calling  interface
  append_to_file <- TRUE # this is the permanent value

  write <- function(this_event) {
    if (!file.exists(key)) {
      # create a new file
      # And some logic to put in the column names the
      # first time  the created function  is called.
      append_to_file <- FALSE #  this is a temporary copy
    }
    write.table(this_event, file = key, sep = ",",
                append = append_to_file, quote = TRUE,
                qmethod  = "escape",
                col.names = !append_to_file, row.names = FALSE)
  }
  read_submissions <- function(fname) { # return a local file name
    file.copy(key, fname, overwrite =  TRUE)
  }

  list(write = write, read_submissions = read_submissions)
}

# Display the event in the console
#' @export
cat_event <- function(key, email) {
  write <- function(this_event) {
    cat("Submission event from  user", this_event$markr_id,
        "in session", this_event$session_id ,"\n")
    cat("\tchunk label:", this_event$chunk_label,
        ":: correct:", this_event$correct, "\n")
    cat(this_event$submission, "\n\n")
  }
  read_submissions <-  function(fname) {
    cat("'Reading' the store. Since this is cat(), we have  nothing.",
        file = fname)
  }

  list(write = write, read_submissions = read_submissions)
}
