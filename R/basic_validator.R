#' A simple validity function for use with local storage
#'
#' This simple validity function is adequate for basic authoring and testing needs.
#' It reads login credentials from a file that can be read without authentication.
#' Storage is "local," meaning that the store is a CSV file typically in the same directory
#' as the tutorial .Rmd file. This can work on a shiny server that has persistent file storage
#' (which means, not shinyapps.io), or on the author's desktop.  It can also work on
#' individual user machines, but each user/machine will have its own, independent storage.
#' @export
make_basic_validator <- function(psfile=NULL, instructor_secret=NULL) {
  if (is.null(instructor_secret))
    stop("Must provide an instructor secret, e.g., 'hello'")
  if (is.null(psfile)) # just for illustration
    psfile <- system.file("credentials-example.csv", package = "submitr")
    # Read the login credentials from a file
  # Here, very simple: read from the fixed file in the package.
  # You could easily change this to use an address provided by the `psfile` argument.
  user_credentials <-
    read.csv(psfile, stringsAsFactors = FALSE
    )
  res <- function(user, mode, secret = "default") {
    # Get rid of leading/trailing white space
    user <- trimws(user)
    mode <- trimws(mode)
    secret <- trimws(secret)
    if (mode == "q") {
      return(TRUE) # no authorization needed to write an event
    } else if (mode == "i") {
      return(user == "instructor" && secret == instructor_secret)
    } else if (mode == "u") {
      return(user %in% user_credentials$id)
    } else if (mode == "p") {
      cat("Validator in p mode with secret:", secret, "\n")
      ind <-  which(user == user_credentials$id)
      return(user_credentials$password[ind] == secret)
    }

    FALSE
  }

  res
}
