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
    return (0.5, rep(length(property)))
}
#' Empty hazard transition function example
#'
#' @param chance Unused
#' @param death This value is directly returned
#' @return The param death
empty_transition_fn <- function(chance, death) {
    return (death)
}

# Todo: repeat for all mandatory parameters
test_that("Hazard missing fn", {
    # Define a simple simulation, but forget to specify the hazard fn
    initPop <- sample_pop(100)
    parms <- list(
      hazards = list(
        list(
          #fn = empty_hazard_fn, # Disabled for this test
          parms=c("age"),
          empty_transition_fn=empty_hazard_fn,
          transition_state="death",
          transition_parms=c("~RESULT", "death")
        )
      ),
      steps = 1
    )    
    expect_error(run_simulation(initPop, parms), "'fn'")
})