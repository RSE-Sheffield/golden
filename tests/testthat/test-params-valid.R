library(testthat)
library(eldoradosim)
library(data.table)

#' Create a sample data.table
#'
#' @param N Number of individuals in the cohort
#' @param max_age Maximum possible age for random initialization (default 100)
#' @return A data.table with columns:
#'   \item{age}{Initial age of each individual, random integer between 0 and max_age}
#'   \item{death}{Death step indicator, initialized to -1}
sample_pop <- function(N, max_age = 100) {
      # Randomly initialize age between 0 and max_age
      age <- sample(0:max_age, N, replace = TRUE)
      
      # Initialize death column to -1
      death <- rep(-1L, N)
      
      # Create data.table
      dt <- data.table(age = age, death = death)
      
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
#' Empty trajectory function example
#'
#' @param age Integer expected
#' @return The age + 1
empty_trajectory_fn <- function(age) {
    return (age + 1)
}

get_parms <- function() {
    return(
        parms <- list(
          hazards = list(
            new_hazard(
              empty_hazard_fn,
              c("age"),
              list(
                new_transition(empty_transition_fn, c("death"), "death")
              )
            )
          ),
          trajectories = list(
            new_trajectory(empty_trajectory_fn, c("age"), "age")
          ),
          steps = 1
        )
    )
}

test_that("Missing parameters trigger errors", {
    initPop <- sample_pop(100)
    # Hazard subfields
    required_fields <- c(
        "fn",
        "parms",
        "transitions"
    )
    for (field in required_fields) {
        parms <- get_parms()
        parms$hazards[[1]][[field]] <- NULL
        expect_error(run_simulation(initPop, parms),
            paste("'", field, "'", sep=""),
            info = paste("Missing", field, "should cause an error"))
    }
    # Transition subfields
    required_fields <- c(
        "fn",
        "parms",
        "state"
    )
    for (field in required_fields) {
        parms <- get_parms()
        parms$hazards[[1]]$transitions[[1]][[field]] <- NULL
        expect_error(run_simulation(initPop, parms),
            paste("'", field, "'", sep=""),
            info = paste("Missing", field, "should cause an error"))
    }
    # Trajectory subfields
    required_fields <- c(
        "fn",
        "parms",
        "property"
    )
    for (field in required_fields) {
        parms <- get_parms()
        parms$trajectories[[1]][[field]] <- NULL
        expect_error(run_simulation(initPop, parms),
            paste("'", field, "'", sep=""),
            info = paste("Missing", field, "should cause an error"))
    }
    # parms subfields
    required_fields <- c(
        "hazards",
        "trajectories",
        "steps"
    )
    for (field in required_fields) {
        parms <- get_parms()
        parms[[field]] <- NULL
        expect_error(run_simulation(initPop, parms),
            paste("'", field, "'", sep=""),
            info = paste("Missing", field, "should cause an error"))
    }
    # parms subfields empty
    required_fields <- c(
        "hazards",
        "trajectories"
    )
    for (field in required_fields) {
        parms <- get_parms()
        parms[[field]] <- list()
        expect_error(run_simulation(initPop, parms),
            "empty",
            info = paste("Missing", field, "should cause an error"))
    }
})