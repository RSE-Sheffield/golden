
#' Empty hazard function example
#'
#' @param property This dictates the length of the vector returned
#' @return A vector of length. equal to param property, filled with 0.5
empty_hazard_fn <- function(property) {
    return (rep(0.5, length(property)))
}
#' Empty hazard transition function example
#'
#' @param a This value is directly returned
#' @return The param a
empty_transition_fn <- function(a) {
    return (a)
}
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

#' Create a sample data.table with two columns a and b, both zero init
sample_pop2 <- function(N) {
      a <- rep(0, N)
      b <- rep(0, N)
      dt <- data.table(a = a, b = b)
      
      return(dt)
}