#' Record events in a local file
#'
#' For recording events in a local file, specified by a file name.
#'
#' @param key Name of the local file. Either an absolute path name or
#' a path relative to the directory with the tutorial .Rmd file
#' @param email Not used for this recorder.
#'
#' @export
record_local <- function(key, email = "") {
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
