#' Variance of total brachlength from most recent common ancestor of n individuals
#'
#' @param n Amount of individuals
#'
#' @return the variance value of the phase-type.
#' @export
#'
#' @examples
#' VarTotal(4:5)
VarTotal <- function(n){
  sapply(n, function(y){
    pi = c(1, rep(0,y-2)) #n-2, since T_1 er abs. state, and first pos. is known.
    lambdas = sapply(2:y, function(x){(x-1)/2})
    if(y == 2){
      S = matrix(-(lambdas[1]))
    } else {
      S = matrix(append(sapply(lambdas[-(y-1)], function(x){
        c(-x, x, rep(0, y-2))}), -lambdas[y-1]),
        nrow = y-1, byrow = T)
    }
    mptd(pi, S, 2) - mptd(pi, S, 1)^2
  })
}
