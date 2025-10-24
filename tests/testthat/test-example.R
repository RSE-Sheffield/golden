library(testthat)
library(eldoradosim)

test_that("id_odd() Odd Numbers", {
    expect_equal(is_odd(3), TRUE)
    expect_equal(is_odd(25), TRUE)
})

test_that("id_odd() Even Numbers", {
    expect_equal(is_odd(4), FALSE)
    expect_equal(is_odd(-24), FALSE)
})