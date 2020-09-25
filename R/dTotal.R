#' Density function of T_Total.
#'
#' @param n number of individuals
#' @param u the quantile
#'
#' @return the density in u
#' @export
#'
#' @examples
#' n = c(2, 5, 10, 20, 50, 100)
#' col = rainbow(length(n))
#' for(i in 1:length(n)){
#'   curve(sapply(x, function(y){
#'     dTotal(n = n[i], y)}), 0, 14, col = col[i], add = i!=1)
#' }
dTotal <- function(n, u) {
  pi = c(1, rep(0,n-2))
  lambdas = sapply(2:n, function(x){ (x-1)/2 })
  if(n == 2){
    S = matrix(-(lambdas[1]))
  } else {
    S = matrix(append(sapply(lambdas[-(n-1)], function(x){
      c(-x, x, rep(0, n-2))}), -lambdas[n-1]),
      nrow = n-1, byrow = T)
  }
  dptd(u, pi, S)
}
