#' Store user and  database information
#'
#' These are internal functions
#' @param str the ID that the user logged into the tutorial with

submitr_env  <-  new.env()

store_ID <- function(str) {
  if(is.null(str)) str <- ".initializing."
  submitr_env$user_ID <- str
}

get_ID  <- function() {
  if (is.null(submitr_env$user_ID))  ".initializing"
  else submitr_env$user_ID
}

