#' Transformation using rewards
#'
#' @param r vector of rewards
#' @param pi Initial distribution
#' @param S Subintensity matrix
#'
#' @return A list containing the following components:
#' \itemize{
#'   \item \code{pi} - New Initial distribution
#'   \item \code{S} - New subintensity matrix
#'   \item \code{prob} - Probability of entering states with positive reward before absorption.
#' }
#'
#' @export
#'
#' @examples
#' S = matrix(c(-6, 6, 0, 0,
#'               0, -3, 1, 2,
#'               0, 0, -1, 0,
#'               0, 0, 0, -1), ncol = 4, byrow = TRUE)
#' p = c(1, 0, 0, 0)

#' reward(c(4, 2, 0, 1), p, S) #Counting singles
#' reward(c(0, 1, 2, 0) , p, S) #Counting doubles
#' reward(c(0, 0, 0, 1) , p, S) #Counting triples
#'
#' temp = reward(c(0,1,2,0) , p, S)
#' hist(rptd(1000, temp$pi, temp$S), breaks = 75, prob = TRUE)
#' curve(dptd(x,temp$pi, temp$S), add = TRUE)
reward <- function(r, pi, S) {
  #If all rewards positive:
  if(all(r>0)){
    pi_out = pi
    if(length(r) == 1){
      S_out = S*r
    }else{
      S_out = S %*% solve(diag(r))
    }
  }else{

    #Which rewards are positive?
    C = which(r>0)

    #Extract the corresponding lambdas.
    V = diag(S)[C]

    #Extract the corresponding pi's
    p = pi[C]
    p0 = pi[-C]

    #Build Q
    Q = diag(length(S[,1])) - solve(diag(diag(S)))%*%S
    Q1 = Q[C,C]
    Q2 = Q[C, -C]
    Q3 = Q[-C, C]
    Q4 = Q[-C, -C]

    #Build pi_out:
    pi_out = p + p0 %*% solve(diag(NROW(p0)) - Q4) %*% Q3

    #Build P
    P = Q1 + Q2 %*% solve(diag(NROW(Q4)) - Q4) %*% Q3

    #Build two tempoary matrixes meant for building S_out
    if(length(V) == 1){
      S_out = V / r[C] * (1 - P)
    }else{
      D = diag(V) / r[C] * (1-diag(P))
      G = sapply(1:length(V), function(x){
        V/r[C]
      })
      #Build S_out
      S_out = -G * P - diag(diag(-G * P)) + diag(V) / r[C] * (1-diag(P))
    }}

  return(list("pi" = pi_out/sum(pi_out), #the new starting distr.. Made to sum to one.
              "S" = S_out, #The new intensity matrix
              "prob" = sum(pi_out))) #The probability of hitting S-plus.
}
