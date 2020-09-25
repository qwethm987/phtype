#' Mean of time from most recent common ancestor of n individuals
#'
#' @param n Amount of individuals
#'
#' @return the mean value of the phase-type.
#' @export
#'
#' @examples
#' plot(2:20, sapply(2:20, MeanMRCA), type = "l", ylim = c(0,7))
MeanMRCA <- function(n){
  sapply(n, function(y){
    pi = c(1, rep(0,y-2))#n-2, since T_1 er abs. state, and first pos. is known.
    lambdas = sapply(2:y, function(x){choose(x,2)})
    if(y == 2){
      S = matrix(-(lambdas[1]))
    } else {
      S = matrix(append(sapply(lambdas[-(y-1)], function(x){
        c(-x, x, rep(0, y-2))}), -lambdas[y-1]),
        nrow = y-1, byrow = T)
    }
    mptd(pi, S, 1)
  })
}
