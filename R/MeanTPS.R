#' Mean time per state
#'
#' @description
#'     returns a matrix where entrance {i,j} is the expected time spent in state
#'     j given the initial state is i.
#' @param pi Initial Distribution
#' @param S Subintensity matrix
#'
#' @return Matrix of expected times.
#' @export
#'
#' @examples
MeanTPS <- function(pi, S){
  solve(-S)
}
