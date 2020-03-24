#' Create a unique session ID
#'
#' Not completely unique, but pretty close!
#'
#' @importFrom stats runif
#' @export

make_session_id <- function() {
  set.seed(as.numeric(trunc(Sys.time(),  units="secs")))
  paste0(round(10 * runif(10)), collapse = "")
}

