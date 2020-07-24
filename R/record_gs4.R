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
  local_env <- new.env()
  local_env$sofar <- data.frame(stringsAsFactors = FALSE)
  local_env$for_hash <- data.frame(stringsAsFactors = FALSE)
  # a data frame to store submissions within
  # the app itself, for later sending to
  # an external database

  do_initialization <- function() {
    # Authorize the request
    initialized <<- TRUE
    auth_fun(email, "q", key)

  }
  write <- function(this_event) {
    if (!initialized) do_initialization()
    # Cache the events so fewer requests are made to Google
    tmp <- local_env$sofar
    for_hash <- local_env$for_hash
    tmp <- rbind(tmp, this_event)
    for_hash <- rbind(for_hash, this_event)
    assign("sofar", tmp, envir = local_env)
    assign("for_hash", for_hash, envir = local_env)
    if (nrow(tmp) >= 5) {
      suppressMessages(
         googlesheets4::sheet_append(key, tmp)
      )
      local_env$sofar <- data.frame(stringsAsFactors = FALSE)
    }
    # res <- suppressMessages(
    #   googlesheets4::sheet_append(key, this_event)
    # )

    res <- this_event
    return(res)
  }
  # create a hash code summarizing all the events
  get_events <- function() local_env$for_hash

  flush <- function() {
    tmp <- local_env$sofar
    if (nrow(tmp) > 0) {
      suppressMessages(
        googlesheets4::sheet_append(key, tmp)
      )
      local_env$sofar <- data.frame(stringsAsFactors = FALSE)
    }
  }
  read_submissions <- function(fname) {
    if (!initialized) do_initialization()
    contents <- googlesheets4::range_read(key)
    write.csv(contents,
              file = fname, row.names=FALSE)
  }
  list(write = write, read_submissions = read_submissions,
       flush = flush, get_events = get_events)
}

# default authorizing using cached credential
auth_gs4 <- function(email, letter, key) {
  googledrive::drive_auth(cache = ".secrets", use_oob = TRUE, email = email)
  googlesheets4::gs4_auth(token = googledrive::drive_token())
}
