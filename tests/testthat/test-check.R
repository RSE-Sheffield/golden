library(testthat)
library(eldoradosim)
library(data.table)

#
# Tests in this file cover
# - check_trajectory()
# - check_hazard()
# - check_transition()
# - check_parameters()
# For the purposes of
# - Missing attributes
# - Attributes of the incorrect type
# - args not found within the initial population
# - length of args, does not match the number of args required by fn()
# - args that begin with "~" do not need to be in the initial population
#

test_that("Trajectory with missing attribute triggers stop()", {
    # Trajectory subfields
    required_fields <- c(
        "fn",
        "args",
        "property"
    )
    # No error by default
    trj <- new_trajectory(empty_trajectory_fn, c("age"), "age")
    expect_no_error(check_trajectory(trj))
    # Test removal of each subfield 1 by 1
    for (field in required_fields) {
        trj <- new_trajectory(empty_trajectory_fn, c("age"), "age")
        trj[[field]] <- NULL
        expect_error(check_trajectory(trj),
            paste("required fields:", field),
            info = paste("Missing", field, "should cause an error"))
    }
})
test_that("Trajectory attribute of incorrect type triggers stop()", {
    # Trajectory subfields
    required_fields <- c(
        "fn",
        "args",
        "property"
    )
    # No error by default
    trj <- new_trajectory(empty_trajectory_fn, c("age"), "age")
    expect_no_error(check_trajectory(trj))
    # Test replacing each field with an unexpected numeric 1 by 1
    for (field in required_fields) {
        trj <- new_trajectory(empty_trajectory_fn, c("age"), "age")
        trj[[field]] <- 12
        expect_error(check_trajectory(trj),
            paste("trajectory\\$", field, sep=""),
            info = paste(field, " of incorrect type should cause an error"))
    }
    # Special case: lists with different item types
    trj <- new_trajectory(empty_trajectory_fn, c("age"), "age")
    trj$args <- list("age", 12)
    expect_error(check_trajectory(trj),
            "'trajectory\\$args' must only contain strings",
            info = paste("trajectory$args elements must be strings"))
})
test_that("Trajectory arg not found in initial pop table triggers stop()", {
    dt <- data.table(a = integer(),
                     b = integer(),
                     c = integer())
    trj_pass <- new_trajectory(empty_trajectory_fn, c("a"), "a")
    # No error by default when table contains column "a"
    expect_no_error(check_trajectory(trj_pass, dt))
    # Error when table does not contain column "d"
    trj_fail <- new_trajectory(empty_trajectory_fn, c("d"), "d")
    expect_error(check_trajectory(trj_fail, dt),
        "columns required by trajectory\\$args: d",
        info = "Column not found in initial pop should cause an error")
})
test_that("Special trajectory arg not found in initial pop table does not trigger stop()", {
    dt <- data.table(a = integer(),
                     b = integer(),
                     c = integer())
    trj_pass <- new_trajectory(empty_trajectory_fn, c("a"), "a")
    # No error by default when table contains column "a"
    expect_no_error(check_trajectory(trj_pass, dt))
    # No error when param is special
    trj_pass2 <- new_trajectory(empty_trajectory_fn, c("~STEP"), "a")
    expect_no_error(check_trajectory(trj_pass2, dt))
})
test_that("Trajectory args length does not match fn args triggers stop()", {
    dt <- data.table(a = integer(),
                     b = integer(),
                     c = integer())
    trj <- new_trajectory(empty_trajectory_fn, c("a"), "a")
    # No error by default when table contains column "a"
    expect_no_error(check_trajectory(trj))
    # Error when params is wrong length
    trj$args <- c("a", "~STEP")
    expect_error(check_trajectory(trj),
        "does not match number of arguments required",
        info = "params cant be used if they don't match fn")
})
test_that("Trajectory property not found in initial pop table triggers stop()", {
    dt <- data.table(a = integer(),
                     b = integer(),
                     c = integer())
    trj <- new_trajectory(empty_trajectory_fn, c("a"), "a")
    # No error by default when table contains column "a"
    expect_no_error(check_trajectory(trj, dt))
    # Error when property doesn't exist in dt
    trj$property <- "d"
    expect_error(check_trajectory(trj, dt),
        "initial population columns do not contain trajectory\\$property",
        info = "property d does not exist in dt")
})

test_that("Hazard with missing attribute triggers stop()", {
    # Hazard subfields
    required_fields <- c(
        "fn",
        "args",
        "transitions",
        "freq",
        "first",
        "last"
    )
    trn <- list(new_transition(empty_transition_fn, c("age"), "age"))
    # No error by default
    haz <- new_hazard(empty_hazard_fn, c("age"), trn)
    expect_no_error(check_hazard(haz))
    # Test removing each subfield 1 by 1
    for (field in required_fields) {
        haz <- new_hazard(empty_hazard_fn, c("age"), trn)
        haz[[field]] <- NULL
        expect_error(check_hazard(haz),
            paste("Hazard missing required fields:", field),
            info = paste("Missing", field, "should cause an error"))
    }
})
test_that("Hazard attribute of incorrect type triggers stop()", {
   # Hazard subfields
    required_fields <- c(
        "fn",
        "args",
        "transitions",
        "freq",
        "first",
        "last"
    )
    trn <- list(new_transition(empty_transition_fn, c("age"), "age"))
    # No error by default
    haz <- new_hazard(empty_hazard_fn, c("age"), trn)
    expect_no_error(check_hazard(haz))
    # Replace individual fields with 12.5
    # No field expects a floating point number
    for (field in required_fields) {
        haz <- new_hazard(empty_hazard_fn, c("age"), trn)
        haz[[field]] <- 12.5
        expect_error(check_hazard(haz),
            paste("hazard\\$", field, sep=""),
            info = paste("Missing", field, "should cause an error"))
    }
    # Special case: lists with different item types
    haz <- new_hazard(empty_hazard_fn, c("age"), trn)
    haz$args <- list("age", 12)
    expect_error(check_hazard(haz),
            "'hazard\\$args' must only contain strings",
            info = paste("hazard$args elements must be strings"))
})
test_that("Hazard arg not found in initial pop table triggers stop()", {
    dt <- data.table(a = integer(),
                     b = integer(),
                     c = integer())
    trn <- list(new_transition(empty_transition_fn, c("a"), "a"))
    # No error by default when table contains column "a"
    haz_pass <- new_hazard(empty_hazard_fn, c("a"), trn)
    expect_no_error(check_hazard(haz_pass, dt))
    # Error when table does not contain column "d"
    haz_fail <- new_hazard(empty_hazard_fn, c("d"), trn)
    expect_error(check_hazard(haz_fail, dt),
        "columns required by hazard\\$args: d",
        info = "Column not found in initial pop should cause an error")
})
test_that("Special hazard arg not found in initial pop table does not trigger stop()", {
    dt <- data.table(a = integer(),
                     b = integer(),
                     c = integer())
    trn <- list(new_transition(empty_transition_fn, c("a"), "a"))
    haz_pass <- new_hazard(empty_hazard_fn, c("a"), trn)
    # No error by default when table contains column "a"
    expect_no_error(check_hazard(haz_pass, dt))
    # No error when param is special
    haz_pass2 <- new_hazard(empty_hazard_fn, c("~STEP"), trn)
    expect_no_error(check_hazard(haz_pass2, dt))
})
test_that("Hazard args length does not match fn args triggers stop()", {
    dt <- data.table(a = integer(),
                     b = integer(),
                     c = integer())
    trn <- list(new_transition(empty_transition_fn, c("a"), "a"))
    haz <- new_hazard(empty_hazard_fn, c("a"), trn)
    # No error by default when table contains column "a"
    expect_no_error(check_hazard(haz))
    # Error when params is wrong length
    haz$args <- c("a", "~STEP")
    expect_error(check_hazard(haz),
        "does not match number of arguments required",
        info = "params cant be used if they don't match fn")
})

test_that("Transition with missing attribute triggers stop()", {
    # Transition subfields
    required_fields <- c(
        "fn",
        "args",
        "state"
    )
    # No error by default
    trn <- new_transition(empty_transition_fn, c("age"), "age")
    expect_no_error(check_transition(trn))
    # Test removal of each subfield 1 by 1
    for (field in required_fields) {
        trn <- new_transition(empty_transition_fn, c("age"), "age")
        trn[[field]] <- NULL
        expect_error(check_transition(trn),
            paste("required fields:", field),
            info = paste("Missing", field, "should cause an error"))
    }
})
test_that("Transition attribute of incorrect type triggers stop()", {
    # Trajectory subfields
    required_fields <- c(
        "fn",
        "args",
        "state"
    )
    # No error by default
    trn <- new_transition(empty_transition_fn, c("age"), "age")
    expect_no_error(check_transition(trn))
    # Test replacing each field with an unexpected numeric 1 by 1
    for (field in required_fields) {
        trn <- new_transition(empty_transition_fn, c("age"), "age")
        trn[[field]] <- 12
        expect_error(check_transition(trn),
            paste("transition\\$", field, sep=""),
            info = paste(field, " of incorrect type should cause an error"))
    }    
    # Special case: lists with different item types
    trn <- new_transition(empty_transition_fn, c("age"), "age")
    trn$args <- list("age", 12)
    expect_error(check_transition(trn),
            "'transition\\$args' must only contain strings",
            info = paste("transition$args elements must be strings"))
})
test_that("Transition arg not found in initial pop table triggers stop()", {
    dt <- data.table(a = integer(),
                     b = integer(),
                     c = integer())
    trn_pass <- new_transition(empty_transition_fn, c("a"), "a")
    # No error by default when table contains column "a"
    expect_no_error(check_transition(trn_pass, dt))
    # Error when table does not contain column "d"
    trn_fail <- new_transition(empty_trajectory_fn, c("d"), "d")
    expect_error(check_transition(trn_fail, dt),
        "columns required by transition\\$args: d",
        info = "Column not found in initial pop should cause an error")
})
test_that("Special transition arg not found in initial pop table does not trigger stop()", {
    dt <- data.table(a = integer(),
                     b = integer(),
                     c = integer())
    trn_pass <- new_transition(empty_transition_fn, c("a"), "a")
    # No error by default when table contains column "a"
    expect_no_error(check_transition(trn_pass, dt))
    # No error when param is special
    trn_pass2 <- new_transition(empty_transition_fn, c("~STEP"), "a")
    expect_no_error(check_transition(trn_pass2, dt))
})
test_that("Transition args length does not match fn args triggers stop()", {
    dt <- data.table(a = integer(),
                     b = integer(),
                     c = integer())
    trn <- new_transition(empty_transition_fn, c("a"), "a")
    # No error by default when table contains column "a"
    expect_no_error(check_transition(trn))
    # Error when params is wrong length
    trn$args <- c("a", "~STEP")
    expect_error(check_transition(trn),
        "does not match number of arguments required",
        info = "params cant be used if they don't match fn")
})
test_that("Transition state not found in initial pop table triggers stop()", {
    dt <- data.table(a = integer(),
                     b = integer(),
                     c = integer())
    trn <- new_transition(empty_transition_fn, c("a"), "a")
    # No error by default when table contains column "a"
    expect_no_error(check_transition(trn, dt))
    # Error when state doesn't exist in dt
    trn$state <- "d"
    expect_error(check_transition(trn, dt),
        "initial population columns do not contain transition\\$state",
        info = "property d does not exist in dt")
})


test_that("Parameters with missing attribute triggers stop()", {
    # Trajectory subfields
    required_fields <- c(
        "hazards",
        "trajectories",
        "steps",
        "random_seed",
        "debug"
    )
    # No error by default
    prm <- new_parameters(list(), list(), 12, 12, FALSE)
    expect_no_error(check_parameters(prm))
    # Test removal of each subfield 1 by 1
    for (field in required_fields) {
        prm <- new_parameters(list(), list(), 12, 12, FALSE)
        prm[[field]] <- NULL
        expect_error(check_parameters(prm),
            paste("required fields:", field),
            info = paste("Missing", field, "should cause an error"))
    }
})
test_that("Parameters attribute of incorrect type triggers stop()", {
    # Trajectory subfields
    required_fields <- c(
        "hazards",
        "trajectories",
        "steps",
        "random_seed",
        "debug"
    )
    # No error by default
    prm <- new_parameters(list(), list(), 12, 12, FALSE)
    expect_no_error(check_parameters(prm))
    # Test replacing each field with an unexpected string 1 by 1
    for (field in required_fields) {
        prm <- new_parameters(list(), list(), 12, 12, FALSE)
        prm[[field]] <- "foo"
        expect_error(check_parameters(prm),
            paste("parameters\\$", field, sep=""),
            info = paste(field, " of incorrect type should cause an error"))
    }
    # Special case, steps/random number must be whole numbers
    prm <- new_parameters(list(), list(), 12, 12, FALSE)
    prm$steps = 12.5
    expect_error(check_parameters(prm),
        "'parameters\\$steps' must be a whole number",
        info = paste(field, " of incorrect type should cause an error"))
    prm <- new_parameters(list(), list(), 12, 12, FALSE)
    prm$random_seed = 12.5
    expect_error(check_parameters(prm),
        "'parameters\\$random_seed' must be a whole number",
        info = paste(field, " of incorrect type should cause an error"))
})