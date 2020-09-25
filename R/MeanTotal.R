#' Mean of T_total of n individuals
#'
#' @param n amount of individuals
#'
#' @return the mean
#' @export
#'
#' @examples
#' plot(2:20, sapply(2:20, MeanTotal), type = "l", ylim = c(0,7))
MeanTotal <- function(n){
  sapply(n, function(y){
    pi = c(1, rep(0,y-2))#n-2, since T_1 er abs. state, and first pos. is known.
    lambdas = sapply(2:y, function(x){ (x-1)/2 })
    if(y == 2){
      S = matrix(-(lambdas[1]))
    } else {
      S = matrix(append(sapply(lambdas[-(y-1)], function(x){
        c(-x, x, rep(0, y-2))}), -lambdas[n-1]),
        nrow = n-1, byrow = T)
    }
    mptd(pi, S, 1)
  })
}
