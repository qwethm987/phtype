#' CDF of Phase-Type distribution
#'
#' @param u Vector of quantiles
#' @param pi Initial distribution
#' @param S Subintensity matrix
#'
#' @return Returns the probability
#' @export
#'
#' @examples pptd(0.5, 1, -2)
pptd <- function(u, pi, S) {
  sapply(u, function(x){
    if(length(pi) == 1){
      1 -pi * exp(S*x)
    }else{
      1-pi %*% expm::expm(S *  x) %*% rep(1, length(pi))
    }
  })
}
