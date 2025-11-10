library(testthat)
library(eldoradosim)
library(data.table)

#' Create a sample data.table with two columns a and b, both zero init
sample_pop <- function(N) {
      a <- rep(0, N)
      b <- rep(0, N)
      dt <- data.table(a = a, b = b)
      
      return(dt)
}

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

plus_two_fn <- function(a) {
    return (a + 2)
}
all_step_fn <- function(a, step) {
    return (rep(step, length(a)))
}

get_parms <- function() {
    return(
        parms <- list(
          hazards = list(
            list(
              fn = empty_hazard_fn,
              parms=c("a"),
              transition_fn=empty_transition_fn,
              transition_state="a",
              transition_parms=c("a")
            )
          ),
          trajectories = list(
            list(
              fn = plus_two_fn,
              property="b",
              parms=c("b")
            )
          ),
          steps = 1
        )
    )
}

test_that("Trajectory function is reflected in results", {
    N <- 100
    initPop <- sample_pop(N)
    parms <- get_parms()
    # 1 step b->b
    parms$steps = 1
    step1 = run_simulation(initPop, parms)
    expect_equal(step1$b, rep(2, N))
    # 4 steps b->b
    parms$steps = 4
    step4 = run_simulation(initPop, parms)
    expect_equal(step4$b, rep(8, N))
    
    parms$trajectories <- list(list(
              fn = all_step_fn,
              property="b",
              parms=c("b", "~STEP")
            ))
    # 1 step STEP->b
    parms$steps = 1
    step1 = run_simulation(initPop, parms)
    expect_equal(step1$b, rep(0, N))
    # 4 step STEP->b
    parms$steps = 4
    step4 = run_simulation(initPop, parms)
    expect_equal(step4$b, rep(3, N))
    
    # c->b?
    
    # second traj?
    
    # dependent traj?
    
})