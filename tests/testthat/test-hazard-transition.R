library(testthat)
library(eldoradosim)

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


transition_add_2 <- function(a) {
    return (a + 2)
}
transition_add_3 <- function(a) {
    return (a + 3)
}

#' odd indices 0, even indices 1 (e.g. [0,1,0,1,0,1...])
#' predictable nature makes it useful for testing hazards and transitions
#' Hazards don't return direct chance, it's passed through 1-exp(-hazard)
#' As such we set 1000, which is effectively 99.99% chance
#' Small potential for this to cause test failures occasionally, so probably best to seed these tests
yes_no_hazard <- function(a) {
    ret <- rep(0.0, length(a))
    ret[seq(2, length(ret), by = 2)] <- 1000.0
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
                new_transition(transition_add_2, c("a"), "a")
              )
            )
          ),
          trajectories = list(
            new_trajectory(empty_trajectory_fn, c("b"), "b")
          ),
          steps = 1,
          random_seed = 12
        )
    )
}
get_parms2 <- function() {
    # Dual transition
    return(
        parms <- new_parameters(
          hazards = list(
            new_hazard(
              yes_no_hazard,
              c("a"),
              list(
                new_transition(transition_add_2, c("a"), "a"),
                new_transition(transition_add_3, c("b"), "b")
              )
            )
          ),
          trajectories = list(
            new_trajectory(empty_trajectory_fn, c("c"), "c")
          ),
          steps = 1,
          random_seed = 12
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
    expect_equal(step1$pop$a, ret_test)
    
    parms$steps = 4
    step4 = run_simulation(initPop, parms)
    expect_equal(step4$pop$a, ret_test * 4)
    
    # Validate that returned data tables are type data.table
    expect_true(data.table::is.data.table(step1$pop))
    expect_true(data.table::is.data.table(step4$pop))
})

test_that("Single Hazard fn/param, multiple transition fn/param", {
    N = 100
    initPop <- sample_pop3(N)
    parms <- get_parms2()
    ## Add second transition
    ## All odd indices "a" parameter transitions from 0 to 2, even remain 0
    ret_test1 <- rep(0.0, N)
    ret_test1[seq(2, N, by = 2)] <- 2
    ## All odd indices "b" parameter transitions from 0 to 3, even remain 0
    ret_test2 <- rep(0.0, N)
    ret_test2[seq(2, N, by = 2)] <- 3
    
    parms$steps = 1
    step1 = run_simulation(initPop, parms)
    expect_equal(step1$pop$a, ret_test1)
    expect_equal(step1$pop$b, ret_test2)
    
    parms$steps = 4
    step4 = run_simulation(initPop, parms)
    expect_equal(step4$pop$a, ret_test1 * 4)
    expect_equal(step4$pop$b, ret_test2 * 4)
    
    # Multiple transitions of same hazard share RNG
    parms$hazards[[1]]$fn = fifty_fifty_hazard
    parms$random_seed <- 12
    
    parms$steps = 1
    step1 = run_simulation(initPop, parms)
    expect_equal(step1$pop$a * 1.5, step1$pop$b)
    
    parms$steps = 4
    step4 = run_simulation(initPop, parms)
    expect_equal(step4$pop$a * 1.5, step4$pop$b)
    
    parms$steps = 7
    step7 = run_simulation(initPop, parms)
    expect_equal(step7$pop$a * 1.5, step7$pop$b)
})