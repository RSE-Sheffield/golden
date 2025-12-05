
#' Empty hazard function example
#'
#' @param property This dictates the length of the vector returned
#' @return A vector of length. equal to param property, filled with 0.5
empty_hazard_fn <- function(property) {
    return (rep(0.5, length(property)))
}
# 0.7 return works out to approximately 50% chance
fifty_fifty_hazard <- function(a) {
    return (rep(0.7, length(a)))
}
#' Empty hazard transition function example
#'
#' @param a This value is directly returned
#' @return The param a
empty_transition_fn <- function(a) {
    return (a)
}
empty_reduction_fn <- empty_transition_fn
#' All transitioning agents, transition from 0 to 1
#' @param a Vector which provides the length of the output vector
#' @return A vector of 1's
transition_to_1_fn <- function(a) {
    return (rep(1, length(a)))
}
#' Empty trajectory function example
#'
#' @param age Integer expected
#' @return The age + 1
empty_trajectory_fn <- function(age) {
    return (age + 1)
}
plus_1_fn <- empty_trajectory_fn

#' Bad general function, returns a vector of 1 less length than input
bad_len_fn1 <- function(a) {
    return (head(a, -1))
}
#' Bad general function, returns a vector of 1 more length than input
bad_len_fn2 <- function(a) {
    return (c(a, 0))
}

#' Create a sample data.table with two columns a and b, both zero init
sample_pop2 <- function(N) {
      a <- rep(0, N)
      b <- rep(0, N)
      dt <- data.table(a = a, b = b)
      
      return(dt)
}
#' Create a sample data.table with two columns a, b and c, all zero init
sample_pop3 <- function(N) {
      a <- rep(0, N)
      b <- rep(0, N)
      c <- rep(0, N)
      dt <- data.table(a = a, b = b, c = c)
      
      return(dt)
}
reduce_fn <- function(x) {
  return (sum(x))
}
filter_fn <- function(x) {
  return (x == -1)
}