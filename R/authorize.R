#' Authorize submissions  to the  database
#'
#' Using an authorization token stored under a `.secrets` directory,
#' authorize access to the account's google sheets. The account is specified
#' by the `email` argument.
#'
#' @param credentials One of these choices:
#' 1. Data frame of user IDs/passwords;
#' 2. the name of a CSV file containing that information;and
#' 3. key to google sheet storing user ID/password pairs
#'
#' @param email The google account email address corresponding to the credentials.
#' This is applicable only when google sheets is being used. Set  `email` to the zero-length string,
#' that is `""`, when the
#' credentials file is stored as a universally readable google spreadsheet.
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
#'    email = 'myaddress@google.com'
#'    )
#' }

authorize_submissions <- function(credentials, email = '')
  {
  #  set up the event handler
  # options(tutorial.event_recorder =
  #           make_recorder(recorder))

  passwd_df <- NULL

  if (is.data.frame(credentials)) {
    passwd_df = credentials
  } else if (file.exists(credentials)) {
    passwd_df = read.csv(credentials)
  } else if (nchar(email) == 0) {
      # use an open credentials sheet
      googledrive::drive_deauth()
      passwd_df <- suppressMessages(sheets_read(credentials))
  } else if (file.exists(".secrets")) {
    # read the passwords from a Google sheet identified by <credentials>
    if (is.null(googledrive::drive_token())) {
      googledrive::drive_auth(
        cache = ".secrets", use_oob = TRUE,  email = email)
      googlesheets4::sheets_auth(token = googledrive::drive_token())
    }
    passwd_df <- suppressMessages(sheets_read(credentials))
  } else {
    stop("Credentials not found.")
  }

  if (!all(names(passwd_df) %in% c("id", "password")))
    stop("password data frame must have columns 'id' and 'password'")

  return(passwd_df)
}



