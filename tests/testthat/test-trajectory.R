library(testthat)
library(eldoradosim)
library(data.table)


plus_two_fn <- function(a) {
    return (a + 2)
}

all_step_fn <- function(a, step) {
    return (rep(step, length(a)))
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
    # 1 step b->b
    parms$steps = 1
    step1 = run_simulation(initPop, parms)
    expect_equal(step1$b, rep(2, N))
    # 4 steps b->b
    parms$steps = 4
    step4 = run_simulation(initPop, parms)
    expect_equal(step4$b, rep(8, N))
    
    parms$trajectories <- list(new_trajectory(
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
    
    # Multiple/zero trajectories
    
})