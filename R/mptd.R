#' Moment of phase-type distribution
#'
#' @param pi Initial distribution
#' @param S Subintensity matrix
#' @param order Order of moment wanted (1 for mean)
#'
#' @return The calculated moment
#' @export
mptd <- function(pi, S, order){
  sapply(order, function(y){
    factorial(y) * pi %*% (expm::`%^%`(solve(-S),y)) %*% rep(1,length(pi))
  })
}
