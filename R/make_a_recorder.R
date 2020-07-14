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
  # Create a unique ID for the each user session
  session_id <- make_session_id()

  # define a function  with the standard set of  arguments
  #  for a learnr  recorder.
  format_event <- function(tutorial_id, tutorial_version, user_id, event, data) {
    Everything <- list(event, data)
    save(Everything, file = "~/Downloads/Everything.rda")

    event_type <- learnr_event_type(data)
    if (event_type %in% c("unchecked-code", "trash")) return(NULL)
    this_event <- data.frame(time = format(Sys.time(), "%a %b %d %X %Y %z"),
                             id = paste(submitr_id, user_id),
                             session_id = session_id,
                             event = event_type,
                             tutorial = paste(data$label, event_type, tutorial_id, tutorial_version),
                             stringsAsFactors = FALSE)
    # Other fields are
    #    prompt, answer, correct, feedback
    if (event_type %in% c("essay", "multiple-choice")) {
      this_event$prompt <- data$question
      this_event$answer <- data$answer
      if (event_type == "essay") {
      this_event$correct <- FALSE
      this_event$feedback <- paste(as.character(nchar(data$answer)), "chars")
      } else {
        this_event$correct <- data$correct
        this_event$feedback <- "none"
      }
    } else if (event_type == "unchecked-code") {
      # THIS SHOULDN'T BE RECORDED
      stop("Attempt to record unchecked code")
      this_event$prompt <- "None"
      this_event$answer <- data$code
      this_event$correct <- FALSE
      if (!is.null(data$error_message)) this_event$feedback <- data$error_message
      else this_event$feedback <- data$time_elapsed
    } else if (event_type == "checked-code") {
      this_event$prompt <- "None"
      this_event$answer <- data$code
      is_correct <- ifelse(is.null(data$feedback), FALSE, data$feedback$correct)
      this_event$correct <- is_correct
      if (!is.null(data$error_message)) this_event$feedback <- data$error_message
      else this_event$feedback <- data$feedback$message
    } else if (event_type == "video") {
      this_event$prompt <- data$video_url
      this_event$answer <- data$time
      this_event$correct <- FALSE
      this_event$feedback <- "watching"
    }
    ss <- store_fun(this_event[1,]) # [1,] just in case a field is a vector
    ss

  }

  format_event # return the function just created
}

learnr_event_type <- function(data) {
  if ("video_url" %in% names(data)) return("video")
  if ("code" %in% names(data)) {
    cat("Is feedback present?", "feedback" %in% names(data), "\n")
    if (!is.null(data$feedback)) return("checked-code")
    else return("unchecked-code")
  }
  if ("question" %in% names(data) && !("reset" %in% names(data))) {
    if (grepl("Essay[0-9]+$", data$label) ||
        grepl(" $", data$question)) return("essay")
    else return("multiple-choice")
  }

  "trash"
}




