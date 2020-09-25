#' Check if phase-type is irreducible
#'
#' @param pi Innitial distribution
#' @param S Subintensity matrix
#'
#' @return TRUE or FALSE
#' @export
ired <- function(pi, S){
  if(length(pi) == 1){
    pi*exp(S*1)>0
  }else{
    all(pi %*% expm::expm(S * 1) > 0)
  }
}
