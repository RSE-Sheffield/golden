library(testthat)
library(eldoradosim)

#
# Tests in this file cover the functionality of history collection during run_simulation()
# This includes the use of filters to capture information about sub-populations
#

test_that("History function collects expected data", {
    # Number of steps to run for
    STEPS = 10
    # Population size to test
    N = 100
    initPop <- list(
        "a" = 1:N
        )
    parms <- new_parameters(
      # No Hazards, trajectory will be used to modify pop
      hazards = list(),
      # Trajectory merely increases all a variables by 1
      trajectories = list(new_trajectory(plus_1_fn, c("a"), "a")),
      steps = STEPS,
      history = new_history(
        columns = list(new_column("sum_a", sum, c("a"))),
        frequency = 1
        )
    )
    # Execute the simulation
    ret <- run_simulation(initPop, parms)
    # Validate that returned data tables are type data.table
    expect_true(data.table::is.data.table(ret$pop))
    expect_true(data.table::is.data.table(ret$history))
    # Validate results match what we calculate
    init_sum_a = sum(1:N)
    # Test each step's result matches what we calculate
    for (i in ret$history$sum_a) {
      # Calculate the impact of this step's trajectory fn
      init_sum_a = init_sum_a + N
      expect_equal(init_sum_a, i)
    }
    # Test that ~STEP column has been generated correctly
    t = 1
    for (i in ret$history[["~STEP"]]) {
      expect_equal(t, i)
      t = t + 1
    }
})
test_that("History function collects expected data with non-1 frequency", {
    # Number of steps to run for
    STEPS = 10
    # Population size to test
    N = 100
    # Frequency of history collection
    FREQ = 3
    initPop <- list(
        "a" = 1:N
        )
    parms <- new_parameters(
      # No Hazards, trajectory will be used to modify pop
      hazards = list(),
      # Trajectory merely increases all a variables by 1
      trajectories = list(new_trajectory(plus_1_fn, c("a"), "a")),
      steps = STEPS,
      history = new_history(
        columns = list(new_column("sum_a", sum, c("a"))),
        frequency = FREQ
        )
    )
    # Execute the simulation
    ret <- run_simulation(initPop, parms)
    # Validate results match what we calculate
    init_sum_a = sum(1:N)
    # First history is always collected after initial step
    init_sum_a = init_sum_a + N
    # Test each step's result matches what we calculate
    for (i in ret$history$sum_a) {
      expect_equal(init_sum_a, i)
      # Subsequent histories follow FREQ steps
      init_sum_a = init_sum_a + N * FREQ
    }
    # Test that ~STEP column has been generated correctly
    t = 1
    for (i in ret$history[["~STEP"]]) {
      expect_equal(t, i)
      t = t + 3
    }
})
test_that("Filtered history function collects expected data", {
    # Sum all odd numbers in a vector
    sum_odd <- function(v) {
      t = 0
      for (i in v) {
         if (i %% 2 == 1) {
            t = t + i
         }
      }
      return (t)
    }
    # Return a vector where all odd values in input are set True
    filter_odd <- function(v) {
      return (v %% 2 == 1)
    }
    # Number of steps to run for
    STEPS = 10
    # Population size to test
    N = 100
    initPop <- list(
        "a" = 1:N
        )
    parms <- new_parameters(
      # No Hazards, trajectory will be used to modify pop
      hazards = list(),
      # Trajectory merely increases all a variables by 1
      trajectories = list(new_trajectory(plus_1_fn, c("a"), "a")),
      steps = STEPS,
      history = new_history(
        columns = list(new_column("sum_a", sum, c("a"),
          filter_fn = filter_odd,
          filter_args = c("a"))),
        frequency = 1
        )
    )
    # Execute the simulation
    ret <- run_simulation(initPop, parms)
    # Validate results match what we calculate
    test_a = 1:N
    # Test each step's result matches what we calculate
    for (i in ret$history$sum_a) {
      # Calculate the impact of this step's trajectory fn and filter
      test_a = test_a + 1
      # Test the history contains the expected value
      expect_equal(sum_odd(test_a), i)
    }
})

test_that("Column & column filter functions cannot return wrong length", {
    N <- 100
    initPop <- sample_pop2(N)
    parms <- new_parameters(
      hazards = list(),
      trajectories = list(),
      steps = 1,
      history = new_history(new_column("sum_a", sum, c("a")))
    )
    parms$debug = TRUE
    
    # Default runs safely
    expect_no_error(run_simulation(initPop, parms))
    
    # Update with a bad column fn
    parms$history$columns[[1]]$fn <- bad_len_fn1
    # Running will now produce an error
    check_parameters(parms, initPop)
    expect_error(run_simulation(initPop, parms), "return had wrong length")
    
    # Update with a bad column fn
    parms$history$columns[[1]]$fn <- bad_len_fn2
    # Running will now produce an error
    expect_error(run_simulation(initPop, parms), "return had wrong length")
    
    # Reset parms, check it works again
    parms <- new_parameters(
      hazards = list(),
      trajectories = list(),
      steps = 1,
      history = new_history(new_column("sum_a", sum, c("a"), filter_fn, c("a")))
    )
    parms$debug = TRUE
    expect_no_error(run_simulation(initPop, parms))
    
    # Update with a bad transition
    parms$history$columns[[1]]$filter_fn <- bad_len_fn1
    # Running will now produce an error
    expect_error(run_simulation(initPop, parms), "return had wrong length")
    
    # Update with a bad hazard
    parms$history$columns[[1]]$filter_fn <- bad_len_fn2
    # Running will now produce an error
    expect_error(run_simulation(initPop, parms), "return had wrong length")
})