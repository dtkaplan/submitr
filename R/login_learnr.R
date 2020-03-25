#' Custom learnr question for logging in to submittr
#'
#' You  don't need to call these yourself, but they
#' have to be  exported so that learnr can have access  to them.
#'
#' @param question First argument
#' @param value Second argument
#' @param  ...  Additional arguments
#' @importFrom  learnr question_ui_initialize
#' @importFrom learnr question_is_valid
#' @importFrom learnr question_is_correct

#' @export
question_ui_initialize.learnr_userid <- function(question, value, ...) {
  shiny::textInput(
    question$ids$answer,
    label = "loginID",
    placeholder = question$options$placeholder,
    value = value
  )
}

#' @importFrom learnr  mark_as
#'
#' @export
question_is_valid.learnr_userid <- function(question, value, ...) {
  # No strong reason for this. 4 is the  shortest valid
  # userID::password string.
  (! is.null(value)) &&  nchar(value)  >= 4
}

#' @export
question_is_correct.learnr_userid <- function(question, value, ...) {
  store_ID("..anonymous..")
  fields <- unlist(strsplit(value, "::", fixed  = TRUE))
  if (length(fields) != 2)
    return(mark_as(FALSE, "Use format user_id::password"))

  ID <- which(fields[1] == question$options$passwd_df$id)
  if (length(ID)  !=  1) return(mark_as(FALSE, "Invalid user ID"))
  if (fields[2]  != question$options$passwd_df$password[ID]) {
    return(
      mark_as(FALSE,
              paste("Invalid  password for user", question$options$passwd_df$id[ID]))
    )
  }

  store_ID(fields[1]) # store the user ID
  mark_as(TRUE, NULL)

}

#' Create a learnr question suitable for entering a user ID and password.
#'
#' @param passwd_df A data  frame with columns "id" and  "password"
#' @param placeholder A character  string to be  shown in the text-entry field
#' @export
login_learnr <- function(passwd_df,
                         placeholder = "Enter ID::password here.") {
  learnr::question(
    text = "ID",
    learnr::answer("bogus", correct = TRUE,  message = "Will never see this"),
    type = "learnr_userid",
    correct = "ID validated",
    incorrect = "Invalid ID. Without a valid ID, your answers will NOT be recorded. Try again.",
    allow_retry = TRUE,
    random_answer_order = FALSE,
    submit_button = "Submit login credentials",
    options =
      list(
        placeholder = placeholder,
        passwd_df = passwd_df
      )

  )
}
# question_ui_completed.learnr_userid <- question_ui_completed.default
