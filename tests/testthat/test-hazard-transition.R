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

test_that("Hazard$first works", {
    N = 100
    initPop <- sample_pop2(N)
    parms <- get_parms()
    ## All odd indices "a" parameter transitions from 0 to 2, even remain 0
    ret_test <- rep(0.0, N)
    ret_test[seq(2, N, by = 2)] <- 2
    
    # Hazard on no steps
    parms$hazards[[1]]$first <- 2
    parms$steps = 1
    step1 = run_simulation(initPop, parms)
    expect_equal(step1$pop$a, rep(0.0, N))
    
    # Hazard on steps 2,3,4 (x3) (skip 1)
    parms$hazards[[1]]$first <- 2
    parms$steps = 4
    step4 = run_simulation(initPop, parms)
    expect_equal(step4$pop$a, ret_test * 3)
    
    # Hazard on steps 3,4,5 (x3) (skip 1,2)
    parms$hazards[[1]]$first <- 3
    parms$steps = 5
    step4 = run_simulation(initPop, parms)
    expect_equal(step4$pop$a, ret_test * 3)
})
test_that("Hazard$last works", {
    N = 100
    initPop <- sample_pop2(N)
    parms <- get_parms()
    ## All odd indices "a" parameter transitions from 0 to 2, even remain 0
    ret_test <- rep(0.0, N)
    ret_test[seq(2, N, by = 2)] <- 2
    
    # Hazard on steps 1
    parms$hazards[[1]]$last <- 3
    parms$steps = 1
    step1 = run_simulation(initPop, parms)
    expect_equal(step1$pop$a, ret_test)
    
    # Hazard on steps 1,2,3 (x3) (skip 4)
    parms$hazards[[1]]$last <- 3
    parms$steps = 4
    step4 = run_simulation(initPop, parms)
    expect_equal(step4$pop$a, ret_test * 3)
    
    # Hazard on steps 1,2,3 (x3) (skip 4-40)
    parms$hazards[[1]]$last <- 3
    parms$steps = 40
    step4 = run_simulation(initPop, parms)
    expect_equal(step4$pop$a, ret_test * 3)
})
test_that("Hazard$freq works", {
    N = 100
    initPop <- sample_pop2(N)
    parms <- get_parms()
    ## All odd indices "a" parameter transitions from 0 to 2, even remain 0
    ret_test <- rep(0.0, N)
    ret_test[seq(2, N, by = 2)] <- 2
    
    parms$hazards[[1]]$freq <- 2
    parms$steps = 1
    step1 = run_simulation(initPop, parms)
    expect_equal(step1$pop$a, ret_test)
    
    parms$hazards[[1]]$freq <- 2
    parms$steps = 4
    step4 = run_simulation(initPop, parms)
    expect_equal(step4$pop$a, ret_test * 2)
    
    parms$hazards[[1]]$freq <- 3
    parms$steps = 1
    step1 = run_simulation(initPop, parms)
    expect_equal(step1$pop$a, ret_test)
    
    parms$hazards[[1]]$freq <- 3
    parms$steps = 4
    step4 = run_simulation(initPop, parms)
    expect_equal(step4$pop$a, ret_test * 2)
    
    parms$hazards[[1]]$freq <- 3
    parms$steps = 6
    step4 = run_simulation(initPop, parms)
    expect_equal(step4$pop$a, ret_test * 2)
})

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

test_that("Hazard that returns Inf is treated as 100% chance", {
    inf_hazard <- function(a) {
        ret <- rep(Inf, length(a))
        return (ret)
    }
    # Can't test the difference between 99.999% and 100% with RNG
    # Mostly looking that it still produces correct output
    N = 10000
    initPop <- sample_pop3(N)
    parms <- get_parms2()
    parms$hazards[[1]]$fn <- inf_hazard
    ## Add second transition
    ## All  "a" parameter transitions from 0 to 2
    ret_test1 <- rep(2, N)
    ## All odd indices "b" parameter transitions from 0 to 3
    ret_test2 <- rep(3, N)
    
    parms$steps = 1
    step1 = run_simulation(initPop, parms)
    expect_equal(step1$pop$a, ret_test1)
    expect_equal(step1$pop$b, ret_test2)
    
    parms$steps = 4
    step4 = run_simulation(initPop, parms)
    expect_equal(step4$pop$a, ret_test1 * 4)
    expect_equal(step4$pop$b, ret_test2 * 4)
})

test_that("No arg hazard works correctly", {
    no_arg_hazard <- function() {
        return (1000.0) # Close to 100%, without being Inf
    }
    no_arg_hazard2 <- function() {
        return (0.0) # Hazard never passes
    }
    # Can't test the difference between 99.999% and 100% with RNG
    # Mostly looking that it still produces correct output
    N = 10000
    initPop <- sample_pop3(N)
    parms <- get_parms2()
    parms$hazards[[1]] <- new_hazard(
              no_arg_hazard,
              c(),
              list(
                new_transition(transition_add_2, c("a"), "a"),
                new_transition(transition_add_3, c("b"), "b")
              )
            )
    ## Add second transition
    ## All  "a" parameter transitions from 0 to 2
    ret_test1 <- rep(2, N)
    ## All odd indices "b" parameter transitions from 0 to 3
    ret_test2 <- rep(3, N)
    
    parms$steps = 1
    step1 = run_simulation(initPop, parms)
    expect_equal(step1$pop$a, ret_test1)
    expect_equal(step1$pop$b, ret_test2)
    
    parms$steps = 4
    step4 = run_simulation(initPop, parms)
    expect_equal(step4$pop$a, ret_test1 * 4)
    expect_equal(step4$pop$b, ret_test2 * 4)
    
    # All hazards return false
    parms$hazards[[1]]$fn <- no_arg_hazard2
    
    parms$steps = 1
    step1 = run_simulation(initPop, parms)
    expect_equal(step1$pop$a, initPop$a)
    expect_equal(step1$pop$b, initPop$b)
    
    parms$steps = 4
    step4 = run_simulation(initPop, parms)
    expect_equal(step4$pop$a, initPop$a)
    expect_equal(step4$pop$b, initPop$b)
})
test_that("No arg transition works correctly", {
    no_arg_transition <- function() {
        return (12.0)
    }
    #
    N = 10000
    initPop <- sample_pop3(N)
    parms <- get_parms2()
    parms$hazards[[1]]$transitions[[2]] <- new_transition(no_arg_transition, c(), "b")
    ## Add second transition
    ## All odd indices "a" parameter transitions from 0 to 2, even remain 0
    ret_test1 <- rep(0.0, N)
    ret_test1[seq(2, N, by = 2)] <- 2.0
    ## All odd indices "b" parameter transitions from 0 to 12, even remain 0
    ret_test2 <- rep(0.0, N)
    ret_test2[seq(2, N, by = 2)] <- 12.0
    
    parms$steps = 1
    step1 = run_simulation(initPop, parms)
    expect_equal(step1$pop$a, ret_test1)
    expect_equal(step1$pop$b, ret_test2)
    
    parms$steps = 4
    step4 = run_simulation(initPop, parms)
    expect_equal(step4$pop$a, ret_test1 * 4)
    expect_equal(step4$pop$b, ret_test2)
})

test_that("Hazard & Transition function cannot return wrong length", {
    N <- 100
    initPop <- sample_pop2(N)
    parms <- get_parms()
    parms$debug = TRUE
    
    # Default runs safely
    expect_no_error(run_simulation(initPop, parms))
    
    # Update with a bad hazard
    parms$hazards[[1]]$fn <- bad_len_fn1
    # Running will now produce an error
    expect_error(run_simulation(initPop, parms), "return had wrong length")
    
    # Update with a bad hazard
    parms$hazards[[1]]$fn <- bad_len_fn2
    # Running will now produce an error
    expect_error(run_simulation(initPop, parms), "return had wrong length")
    
    # Reset parms
    parms <- get_parms()
    parms$debug = TRUE
    
    # Update with a bad transition
    parms$hazards[[1]]$transitions[[1]]$fn <- bad_len_fn1
    # Running will now produce an error
    expect_error(run_simulation(initPop, parms), "return had wrong length")
    
    # Update with a bad hazard
    parms$hazards[[1]]$transitions[[1]]$fn <- bad_len_fn2
    # Running will now produce an error
    expect_error(run_simulation(initPop, parms), "return had wrong length")
})
