library(testthat)
library(eldoradosim)
library(data.table)


plus_two_fn <- function(a) {
    return (a + 2)
}

all_step_fn <- function(a, step) {
    return (rep(step, length(a)))
}

add_fn <- function(a, b) {
    return (a + b)
}

get_parms <- function() {
    return(
        parms <- new_parameters(
          hazards = list(
            new_hazard(
              empty_hazard_fn,
              c("a"),
              list(
                new_transition(empty_transition_fn, c("a"), "a")
              )
            )
          ),
          trajectories = list(
            new_trajectory(plus_two_fn, c("b"), "b")
          ),
          steps = 1
        )
    )
}

test_that("Trajectory function is reflected in results", {
    N <- 100
    initPop <- sample_pop2(N)
    parms <- get_parms()
    ## First: b->b
    # 1 step
    parms$steps = 1
    step1 = run_simulation(initPop, parms)
    expect_equal(step1$b, rep(2, N))
    # 4 steps
    parms$steps = 4
    step4 = run_simulation(initPop, parms)
    expect_equal(step4$b, rep(8, N))
    
    ## Second: ~STEP->b
    parms$trajectories <- list(new_trajectory(
              fn = all_step_fn,
              property="b",
              parms=c("b", "~STEP")
            ))
    # 1 step
    parms$steps = 1
    step1 = run_simulation(initPop, parms)
    expect_equal(step1$b, rep(0, N))
    # 4 step
    parms$steps = 4
    step4 = run_simulation(initPop, parms)
    expect_equal(step4$b, rep(3, N))
    
    ## Third: a+b->b
    parms$trajectories <- list(new_trajectory(
              fn = add_fn,
              property="b",
              parms=c("b", "a")
            ))
    initPop$a = rep(3, N)
    # 1 step
    parms$steps = 1
    step1 = run_simulation(initPop, parms)
    expect_equal(step1$b, rep(3, N))
    # 4 step
    parms$steps = 4
    step4 = run_simulation(initPop, parms)
    expect_equal(step4$b, rep(12, N))
    
    ## Fourth: Two trajectories (combine second and third above tests)
    initPop <- sample_pop3(N)
    initPop$a = rep(4, N)
    parms$trajectories <- list(
        new_trajectory(
              fn = all_step_fn,
              property="c",
              parms=c("c", "~STEP")
            ),
        new_trajectory(
              fn = add_fn,
              property="b",
              parms=c("b", "a")
            )
    )
    # 1 step
    parms$steps = 1
    step1 = run_simulation(initPop, parms)
    expect_equal(step1$a, rep(4, N))
    expect_equal(step1$b, rep(4, N))
    expect_equal(step1$c, rep(0, N))
    # 4 step
    parms$steps = 4
    step4 = run_simulation(initPop, parms)
    expect_equal(step4$a, rep(4, N))
    expect_equal(step4$b, rep(16, N))
    expect_equal(step4$c, rep(3, N))
    
    
    ## Fifth: Repeat the previous test, however the trajectories are dependent
    ## They should execute in the defined order
    initPop <- sample_pop2(N)
    parms$trajectories <- list()
    # 1 step
    parms$steps = 1
    expect_no_error(run_simulation(initPop, parms))
    
    
})