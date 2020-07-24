#' Methods for storing via a hash generation a la {learnrhash}
#'
#' @export
record_hash  <-  function() {
  initialized <- FALSE # shared among all three functions
  local_env <- new.env()
  local_env$sofar <- data.frame(stringsAsFactors = FALSE)
  # a data frame to store submissions within
  # the app itself, for later sending to
  # an external database

  do_initialization <- function() {
    # nothing needed
  }
  write <- function(this_event) {
    tmp <- local_env$sofar
    tmp <- rbind(tmp, this_event)
    assign("sofar", tmp, envir = local_env)

    this_event
  }
  # get the data frame holding the results
  get_events <- function() local_env$sofar

  flush <- function() {
    # nothing needed
  }
  read_submissions <- function(){}
  list(write = write, read_submissions = read_submissions,
       flush = flush, get_events = get_events)
}
