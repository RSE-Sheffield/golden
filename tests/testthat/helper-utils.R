
#' Empty hazard function example
#'
#' @param property This dictates the length of the vector returned
#' @return A vector of length. equal to param property, filled with 0.5
empty_hazard_fn <- function(property) {
    return (rep(0.5, length(property)))
}
#' Empty hazard transition function example
#'
#' @param chance Unused
#' @param death This value is directly returned
#' @return The param death
empty_transition_fn <- function(death) {
    return (death)
}
#' Empty trajectory function example
#'
#' @param age Integer expected
#' @return The age + 1
empty_trajectory_fn <- function(age) {
    return (age + 1)
}