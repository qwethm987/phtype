#' Density function of T_MRCA.
#'
#' @param n How many individuals
#' @param u The quantile.
#'
#' @return The density in u
#' @export
#'
#' @examples
#' n = c(2,5,10,100)
#' col = rainbow(length(n))
#' for(i in 1:length(n)){
#'   curve(dMRCA(n = n[i], x), 0, 4, col = col[i], add = i!=1, ylim= c(0,0.9))
#' }
#' legend(2.5,1,paste("n = ",n, sep = ""), col=col, lty=1)
dMRCA <- function(n, u) {
  pi = c(1, rep(0,n-2))
  lambdas = sapply(2:n, function(x){choose(x,2)})
  if(n == 2){
    S = matrix(-(lambdas[1]))
  } else {
    S = matrix(append(sapply(lambdas[-(n-1)], function(x){
      c(-x, x, rep(0, n-2))}), -lambdas[n-1]),
      nrow = n-1, byrow = T)
  }
  dptd(u, pi, S)
}
