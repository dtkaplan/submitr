#' Authorize submissions  to the  database
#'
#' Creates a text box group tutorial quiz question.
#'
#' @param credentials One of these choices:
#' 1. Data frame of user IDs/passwords;
#' 2. the name of a CSV file containing that information;and
#' 3. key to google sheet storing user ID/password pairs
#' @param recorder The function used to record events. By
#' default  this is `cat_event()`, which merely prints the  event to the console.
#' See also `in_google_sheets(storage_sheet_key)`.
#'
#' @param placeholder Character string giving instructions to user.
#'
#' @param shared A logical flag (default TRUE) applicable only when the
#' credentials file is stored as a universally readable google spreadsheet.
#'
#' @param return_df A logical flag (default FALSE) specifying that the password
#' file should be returned.
#'
#' @importFrom googlesheets4 sheets_auth sheets_append sheets_read
#' @importFrom googledrive drive_auth drive_token
#' @importFrom shiny textInput
#'
#' @export
#' @examples
#' \dontrun{
#' authorize_submissions(
#'    credentials =
#'         data.frame(id  = c("anne", "betty"), passwords = c("cat", "dog"))
#'    recorder = in_google_sheets("234238d8322s2342"),
#'    placeholder = "Type ID and  password here.",
#'    )
#' }

authorize_submissions <- function(
  credentials,
  recorder = cat_event(),
  placeholder = "Enter ID here in format user_name::password",
  shared = TRUE,
  return_df = FALSE)
  {

  #  set up the event handler
  options(tutorial.event_recorder =
            make_recorder(recorder))

  passwd_df <- NULL


  if (is.data.frame(credentials)) {
    passwd_df = credentials
  } else if (file.exists(credentials)) {
    passwd_df = read.csv(credentials)
  } else {
    if (shared) {
      googledrive::drive_deauth() # use an open credentials sheet
      passwd_df <- sheets_read(credentials)
    }
    # read the passwords from a Google sheet identified by <credentials>
    if (file.exists(".secrets")) {
    googledrive::drive_auth(cache = ".secrets")#,  email = email)
    googlesheets4::sheets_auth(token = googledrive::drive_token())
    } else {
      googledrive::drive_deauth() # use an open credentials file.
    }
  }

  # Get the credentials file if it hasn't been gotten already
  # by some other means.
  if (is.null(passwd_df)) {
    suppressMessages(passwd_df <-  sheets_read(credentials))
  }

  if (!all(names(passwd_df) %in% c("id", "password")))
    stop("password data frame must have columns 'id' and 'password'")


  if (return_df)  return(passwd_df)

  # Create  the field for entering User ID and password
  # Trying to turn  off storage of previous answers ...
  res <- login_learnr(passwd_df = passwd_df,
               placeholder =  placeholder)


  return(res)
}



