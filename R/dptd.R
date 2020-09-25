#' Density function for Phase-Type distribution
#'
#' @param u Vector of values
#' @param pi Initial distribution
#' @param S Subintensitymatrix
#'
#' @return Returns the density.
#' @export
#'
#' @examples curve(dptd(x, 1, -2))
dptd <- function(u, pi, S) {
  sapply(u, function(x){
    if(length(pi) == 1){
      -pi * exp(S*x)*S
    }else{
      -pi %*% expm::expm(S *  x) %*% S %*% rep(1, length(pi))
      }
  })
}
