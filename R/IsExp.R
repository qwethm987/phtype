#' Is Exponential
#'
#' @description
#'    Checks if the phase type can be written as an exponential r.v.
#'
#' @param S Initial Distribution
#'
#' @return Either FALSE or a rate.
#' @export
#'
#' @examples
#' IsExp(matrix(c(-2, 0, 1, -1), nrow = 2))
#' IsExp(-2)
#' IsExp(matrix(c(-2, 0, 1, -3), nrow = 2))
IsExp <- function(S){
  lambda = unique(-S %*% rep(1,NROW(S)))
  if(length(lambda) == 1){
    lambda[1]
  }else{
    FALSE
  }
}
