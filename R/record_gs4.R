#' Methods for storing in Google Sheets using a cached credential
#'
#' @export
#' @param key The google sheet ID
#' @param email Character string with the google email address
#' that corresponds to the key.
#' @param auth_fun function to perform validation
#'
#' @export
record_gs4  <-  function(key, email, auth_fun = submitr:::auth_gs4) {
  initialized <- FALSE # shared among all three functions

  do_initialization <- function() {
    # Authorize the request
    initialized <<- TRUE
    auth_fun(email, "q", key)

  }
  write <- function(this_event) {
    if (!initialized) do_initialization()
    res <- suppressMessages(
      googlesheets4::sheet_append(key, this_event)
    )
    return(res)
  }
  read_submissions <- function(fname) {
    if (!initialized) do_initialization()
    contents <- googlesheets4::range_read(key)
    write.csv(contents, file = fname, row.names=FALSE)
  }
  list(write = write, read_submissions = read_submissions)
}

# default authorizing using cached credential
auth_gs4 <- function(email, letter, key) {
  googledrive::drive_auth(cache = ".secrets", use_oob = TRUE, email = email)
  googlesheets4::gs4_auth(token = googledrive::drive_token())
}
