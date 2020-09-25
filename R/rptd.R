#' Simulation from phase-type distribution
#'
#' @param n number of observations
#' @param pi Initial distribution
#' @param S Subintensity matrix
#'
#' @return vector of absorption times
#' @export
#'
#' @examples
#' m = 5
#' pi = c(1, rep(0, m-2))
#' lambdas = sapply(2:m, function(x){choose(x,2)/x})
#' S = matrix(append(sapply(lambdas[-(m-1)], function(x){
#'     c(-x, x, rep(0, m-2))}), -lambdas[m-1]),
#'    nrow = m-1, byrow = TRUE)
#' hist(rptd(10^3, pi, S), breaks = 20, freq = FALSE)
#' curve(dptd(x, pi, S), add = TRUE)
rptd <- function(n, pi, S){
  #First I wish to create a transitionmatrix from my subintensity matrix.
  temp = S
  temp = (temp - diag(diag(temp)))
  temp = temp/(-diag(S))
  #Makes it (p+1)x(p+1) dimensional.
  temp = cbind(temp, rowSums(S)/diag(S))
  P = rbind(temp, c(rep(0, length(pi)), 1))

  #Now isolate the rates with which we leave each state.
  rates = -diag(S)

  #Initialize vector of results:
  times = rep(0, n)

  for(i in 1:n){
    #First choose which state to start in
    start = sample(1:length(pi), 1, prob = pi)

    #Initialize starting state as a vector with zeroes and a 1 in the correct
    #index indicating the state.
    state = rep(0, length(pi) + 1)
    state[start] = 1

    #Start the time as an exponentially distributed r.v. with the correct rate.
    time = stats::rexp(1,rates[start])

    while(T){
      #While we are not at the absorbing state:
      #Jump to new state:
      newstate = sample(1:(length(pi) + 1), size = 1,  prob = state %*% P)

      #If new state is absorbing state we break:
      if(newstate == length(pi) + 1){break}

      #Otherwise update state-vector
      state = rep(0, length(pi) + 1)
      state[newstate] = 1

      #And simulate how long until next jump.
      time = time + stats::rexp(1,rates[which(state == 1)])
    }
    times[i] = time
  }
  times
}
