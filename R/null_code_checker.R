#' Placeholder for a `{learnr}` code-checking function
#'
#' Used for demonstration and development purposes. Doesn't do anything.
#' @export
null_code_checker <- function(...) {
  goo <- list(...)

  list(message = "No genuine code checking is being done.",
       correct = runif(1) > 0.5,
       location = "append")
}
