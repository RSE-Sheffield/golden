library(testthat)
library(eldoradosim)
library(data.table)

#
# Tests in this file cover the functionality of hazards and transitions
# Hazards and transitions are interlinked, so cannot be tested independent of one-another
# The tests mostly ensure that the below features are functional: 
# - multiple hazard parameters
# - multiple hazards
# - dependent hazards/order
# - multiple transition parameters
# - multiple transitions
# - dependent transitions/order
# - special (e.g.~CAPS) parameters
#


transition_to_2 <- function(a) {
    return (rep(2, length(a)))
}

#' odd indices 0, even indices 1 (e.g. [0,1,0,1,0,1...])
#' predictable nature makes it useful for testing hazards and transitions
yes_no_hazard <- function(a) {
    ret <- rep(0.0, length(a))
    ret[seq(2, length(ret), by = 2)] <- 1.0
    return (ret)
}

get_parms <- function() {
    return(
        parms <- new_parameters(
          hazards = list(
            new_hazard(
              yes_no_hazard,
              c("a"),
              list(
                new_transition(transition_to_2, c("a"), "a")
              )
            )
          ),
          trajectories = list(
            new_trajectory(empty_trajectory_fn, c("b"), "b")
          ),
          steps = 1
        )
    )
}

test_that("Single Hazard fn/param, single transition fn/param", {
    N = 100
    initPop <- sample_pop2(N)
    parms <- get_parms()
    ## All odd indices "a" parameter transitions from 0 to 2, even remain 0
    ret_test <- rep(0.0, N)
    ret_test[seq(2, N, by = 2)] <- 2
    
    parms$steps = 1
    step1 = run_simulation(initPop, parms)
    expect_equal(step1$a, ret_test)
    
    parms$steps = 4
    step4 = run_simulation(initPop, parms)
    expect_equal(step4$a, ret_test)
})

test_that("Single Hazard fn/param, multiple transition fn/param", {
    N = 100
    initPop <- sample_pop2(N)
    parms <- get_parms()
    ## All odd indices "a" parameter transitions from 0 to 2, even remain 0
    ret_test <- rep(0.0, N)
    ret_test[seq(2, N, by = 2)] <- 2
    
    parms$steps = 1
    step1 = run_simulation(initPop, parms)
    expect_equal(step1$a, ret_test)
    
    parms$steps = 4
    step4 = run_simulation(initPop, parms)
    expect_equal(step4$a, ret_test)
})
