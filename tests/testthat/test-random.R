library(testthat)
library(eldoradosim)

#
# Tests in this file cover
# - Do different non-zero random seeds, lead to different hazard results
# - Does fixed random seed, maintain hazard results across multiple runs
# - Does RNG set 0, change hazard results across multiple runs

get_parms <- function() {
    return(
        parms <- new_parameters(
          hazards = list(
            new_hazard(
              empty_hazard_fn, # Always returns 0.5
              c("a"),
              list(
                new_transition(transition_to_1_fn, c("a"), "a")
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

test_that("Different random, different hazard results", {
    N <- 100
    initPop <- sample_pop2(N) # All init vars are 0
    parms <- get_parms()
    parms$random_seed = 12
    rng_12 = run_simulation(initPop, parms)
    parms$random_seed = 13
    rng_13 = run_simulation(initPop, parms)
    expect_false(identical(rng_12, rng_13))
})
test_that("Same random, same hazard results", {
    N <- 100
    initPop <- sample_pop2(N) # All init vars are 0
    parms <- get_parms()
    parms$random_seed = 12
    rng_12a = run_simulation(initPop, parms)
    parms$random_seed = 12
    rng_12b = run_simulation(initPop, parms)
    expect_identical(rng_12a, rng_12b)
})
test_that("Random==0, different random, different hazard results", {
    N <- 100
    initPop <- sample_pop2(N) # All init vars are 0
    parms <- get_parms()
    parms$random_seed = 0
    set.seed(12)
    rng_12 = run_simulation(initPop, parms)
    set.seed(13)
    rng_13 = run_simulation(initPop, parms)
    expect_false(identical(rng_12, rng_13))
})
test_that("Random==0, same random, same hazard results", {
    N <- 100
    initPop <- sample_pop2(N) # All init vars are 0
    parms <- get_parms()
    parms$random_seed = 0
    set.seed(12)
    rng_12a = run_simulation(initPop, parms)
    set.seed(12)
    rng_12b = run_simulation(initPop, parms)
    expect_identical(rng_12a, rng_12b)
})