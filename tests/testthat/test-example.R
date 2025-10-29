library(testthat)
library(eldoradosim)
library(data.table)
library(SciViews)

test_that("id_odd() Odd Numbers", {
    expect_equal(is_odd(3), TRUE)
    expect_equal(is_odd(25), TRUE)
})

test_that("id_odd() Even Numbers", {
    expect_equal(is_odd(4), FALSE)
    expect_equal(is_odd(-24), FALSE)
})

test_that("calling run_simulation()", {
    # Mock population
    initpop <- data.table(
      id = 1:10,
      age = c(23, 45, 31, 52, 27, 36, 41, 29, 60, 34),
      is_male = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE),
      bmi = c(21.5, 27.8, 22.3, 30.2, 19.6, 24.7, 26.1, 23.8, 28.9, 25.4)
    )
    # Mock parameters
    parms <- list(
      hazards = list(list(fn = ln, parms=c("age"), freq = 1),
                     list(fn = lg, parms=c("bmi"), freq = 5)),
      steps = 100L,
      random_seed = 12L
    )
    print(initpop$age)
    run_simulation(initpop, parms)
    print(initpop$age)
})