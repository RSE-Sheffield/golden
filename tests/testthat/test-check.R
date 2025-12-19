library(testthat)
library(golden)

#
# Tests in this file cover
# - check_trajectory()
# - check_hazard()
# - check_transition()
# - check_parameters()
# - check_history()
# - check_columns()
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
    # Special case: Multiple properties provided
    trj <- new_trajectory(empty_trajectory_fn, c("age"), c("age", "foo"))
    expect_no_error(check_trajectory(trj))
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
    # Error when args is wrong length
    trj$args <- c("a", "~STEP")
    expect_error(check_trajectory(trj),
        "does not match number of arguments required",
        info = "args cant be used if they don't match fn")
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
test_that("Multivariate trajectory property not found in initial pop table triggers stop()", {
    dt <- data.table(a = integer(),
                     b = integer(),
                     c = integer())
    trj <- new_trajectory(empty_trajectory_fn, c("a"), c("a", "b"))
    # No error by default when table contains column "a"
    expect_no_error(check_trajectory(trj, dt))
    # Error when property doesn't exist in dt
    trj$property <- c("a", "d")
    expect_error(check_trajectory(trj, dt),
        "initial population columns do not contain trajectory\\$property",
        info = "property d does not exist in dt")
    trj$property <- c("d", "a")
    expect_error(check_trajectory(trj, dt),
        "initial population columns do not contain trajectory\\$property",
        info = "property d does not exist in dt")
})
test_that("Args can be empty string", {
    # No error by default
    trj <- new_trajectory(empty_trajectory_fn, c(), "age")
    expect_no_error(check_trajectory(trj))
})
test_that("Multivariate trajectory check does not crash", {
    dt <- data.table(a = integer(),
                     b = integer(),
                     c = integer())
    trj_pass <- new_trajectory(empty_trajectory_fn, c("a"), c("a", "b"))
    # No error by default when table contains column "a"
    expect_no_error(check_trajectory(trj_pass))
    expect_no_error(check_trajectory(trj_pass, dt))
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
            paste("golden_hazard missing required fields:", field),
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
    haz <- new_hazard(empty_hazard_fn, c("age"), trn)
    haz$transitions <- list(new_transition(empty_transition_fn, c("age"), "age"), 12)
    expect_error(check_hazard(haz),
            "'hazard\\$transitions' must be S3 objects of class 'golden_transition'",
            info = paste("hazard$transitions elements must be transition objects"))
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
    trn <- list(new_transition(empty_transition_fn, c("a"), "a"))
    haz <- new_hazard(empty_hazard_fn, c("a"), trn)
    # No error by default when table contains column "a"
    expect_no_error(check_hazard(haz))
    # Error when args is wrong length
    haz$args <- c("a", "~STEP")
    expect_error(check_hazard(haz),
        "does not match number of arguments required",
        info = "args cant be used if they don't match fn")
})
test_that("Hazard passed transition, as opposed to list of,is valid", {
    trn <- new_transition(empty_transition_fn, c("age"), "age")
    haz <- new_hazard(empty_hazard_fn, c("age"), trn)
    # It has been replaced by a list containing only the original item
    expect_equal(length(haz$transitions), 1)
    expect_equal(haz$transitions[[1]], trn)
    # No error by default
    expect_no_error(check_hazard(haz))
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
    # Error when args is wrong length
    trn$args <- c("a", "~STEP")
    expect_error(check_transition(trn),
        "does not match number of arguments required",
        info = "args cant be used if they don't match fn")
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
    # Parameters subfields
    required_fields <- c(
        "hazards",
        "trajectories",
        "steps",
        "random_seed",
        "debug",
        "print_timing"
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
        "history",
        "random_seed",
        "debug",
        "print_timing"
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
test_that("Parameters passed hazard, as opposed to list of,is valid", {
    trn <- new_transition(empty_transition_fn, c("age"), "age")
    haz <- new_hazard(empty_hazard_fn, c("age"), trn)
    prm <- new_parameters(haz, list(), 12, 12, FALSE)
    # It has been replaced by a list containing only the original item
    expect_equal(length(prm$hazards), 1)
    expect_equal(prm$hazards[[1]], haz)
    # No error by default
    expect_no_error(check_parameters(prm))
})
test_that("Parameters passed trajectory, as opposed to list of,is valid", {
    trj <- new_trajectory(empty_trajectory_fn, c("age"), "age")
    prm <- new_parameters(list(), trj, 12, 12, FALSE)
    # It has been replaced by a list containing only the original item
    expect_equal(length(prm$trajectories), 1)
    expect_equal(prm$trajectories[[1]], trj)
    # No error by default
    expect_no_error(check_parameters(prm))
})
test_that("Parameters passed hazard, as opposed to list of,is valid", {
    # This also tests that new_parameters having list() passed to trajectories is acceptable
    trn <- new_transition(empty_transition_fn, c("age"), "age")
    haz <- new_hazard(empty_hazard_fn, c("age"), trn)
    prm <- new_parameters(haz, list(), 12, 12, FALSE)
    # It has been replaced by a list containing only the original item
    expect_equal(length(prm$hazards), 1)
    expect_equal(prm$hazards[[1]], haz)
    # No error by default
    expect_no_error(check_parameters(prm))
})
test_that("Parameters passed trajectory, as opposed to list of,is valid", {
    # This also tests that new_parameters having list() passed to hazards is acceptable
    trj <- new_trajectory(empty_trajectory_fn, c("age"), "age")
    prm <- new_parameters(list(), trj, 12, 12, FALSE)
    # It has been replaced by a list containing only the original item
    expect_equal(length(prm$trajectories), 1)
    expect_equal(prm$trajectories[[1]], trj)
    # No error by default
    expect_no_error(check_parameters(prm))
})


test_that("History with missing attribute triggers stop()", {
    # History subfields
    required_fields <- c(
        "columns",
        "frequency"
    )
    # No error by default
    hist <- new_history(list(new_column("test", empty_reduction_fn, c("a"))), 1)
    expect_no_error(check_history(hist))
    # Test removal of each subfield 1 by 1
    for (field in required_fields) {
        hist <- new_history(list(new_column("test", empty_reduction_fn, c("a"))), 1)
        hist[[field]] <- NULL
        expect_error(check_history(hist),
            paste("required fields:", field),
            info = paste("Missing", field, "should cause an error"))
    }
})
test_that("History attribute of incorrect type triggers stop()", {
    # History subfields
    required_fields <- c(
        "columns",
        "frequency"
    )
    # No error by default
    hist <- new_history(list(new_column("test", empty_reduction_fn, c("a"))), 1)
    expect_no_error(check_history(hist))
    # Test replacing each field with an unexpected logical 1 by 1
    for (field in required_fields) {
    hist <- new_history(list(new_column("test", empty_reduction_fn, c("a"))), 1)
        hist[[field]] <- TRUE
        expect_error(check_history(hist),
            paste("history\\$", field, sep=""),
            info = paste(field, " of incorrect type should cause an error"))
    }
    # Special case: lists with different item types
    hist <- new_history(list(new_column("test", empty_reduction_fn, c("a"))), 1)
    hist$columns <- list(new_column("test", empty_reduction_fn, c("a")), 12)
    expect_error(check_history(hist),
            "'history\\$columns' must be S3 objects of class 'golden_history_column'",
            info = paste("history$columns elements must be history_column objects"))
    # Special case, frequency must be positive whole number
    hist <- new_history(list(new_column("test", empty_reduction_fn, c("a"))), 1)
    hist$frequency = 12.5
    expect_error(check_history(hist),
        "'history\\$frequency' must be a positive integer",
        info = paste(field, " of incorrect type should cause an error"))
    hist$frequency = 0
    expect_error(check_history(hist),
        "'history\\$frequency' must be a positive integer",
        info = paste(field, " of incorrect type should cause an error"))
    hist$frequency = -12
    expect_error(check_history(hist),
        "'history\\$frequency' must be a positive integer",
        info = paste(field, " of incorrect type should cause an error"))
})
test_that("Parameters passed column, as opposed to list of,is valid", {
    clm <- new_column("test", empty_reduction_fn, c("a"))
    hist <- new_history(clm, 1)
    # It has been replaced by a list containing only the original item
    expect_equal(length(hist$columns), 1)
    expect_equal(hist$columns[[1]], clm)
    # No error by default
    expect_no_error(check_history(hist))
})
test_that("History cannot contain two columns with the same name", {
    # No error by default
    col1 <- new_column("test", empty_reduction_fn, c("a"))
    col2 <- new_column("test", empty_trajectory_fn, c("a"))
    expect_error(new_history(list(col1, col2)),
        "Each element of history\\$columns must have a unique name")
})

test_that("Column with missing attribute triggers stop()", {
    # Column subfields
    required_fields <- c(
        "name",
        "args",
        "fn"
    )
    # No error by default
    clm <- new_column("test", empty_reduction_fn, c("a"))
    expect_no_error(check_column(clm))
    # Test removing each subfield 1 by 1
    for (field in required_fields) {
        clm <- new_column("test", empty_reduction_fn, c("a"))
        clm[[field]] <- NULL
        expect_error(check_column(clm),
            paste("golden_history_column missing required fields:", field),
            info = paste("Missing", field, "should cause an error"))
    }
})
test_that("Column filter_args passed without filter_fn triggers stop()", {
    # No error by default
    clm <- new_column("test", empty_reduction_fn, c("a"), empty_trajectory_fn, c("a"))
    expect_no_error(check_column(clm))
    # Remove filter_fn
    clm$filter_fn <- NULL
    expect_error(check_column(clm),
        "'column\\$filter_args' provided without 'column\\$filter_fn'",
        info = "Column should not contains filter_args without filter_fn")
})
test_that("Column attribute of incorrect type triggers stop()", {
   # Column subfields
    required_fields <- c(
        "name",
        "fn",
        "args",
        "filter_fn",
        "filter_args"
    )
    # No error by default
    clm <- new_column("test", empty_reduction_fn, c("a"), empty_trajectory_fn, c("a"))
    expect_no_error(check_column(clm))
    # Replace individual fields with 12.5
    # No field expects a number
    for (field in required_fields) {
        clm <- new_column("test", empty_reduction_fn, c("a"), empty_trajectory_fn, c("a"))
        clm[[field]] <- 12.5
        expect_error(check_column(clm),
            paste("column\\$", field, sep=""),
            info = paste(field, "of incorrect type should cause an error"))
    }
    # Special case: lists with different item types
    # args
    clm <- new_column("test", empty_reduction_fn, c("a"))
    clm$args <- list("a", 12)
    expect_error(check_column(clm),
            "'column\\$args' must only contain strings",
            info = paste("column$args elements must be strings"))
    # filter_args
    clm <- new_column("test", empty_reduction_fn, c("a"), empty_trajectory_fn, c("a"))
    clm$filter_args <- list("a", 12)
    expect_error(check_column(clm),
            "'column\\$filter_args' must only contain strings",
            info = paste("column$filter_args elements must be strings"))
})
test_that("Column arg not found in initial pop table triggers stop()", {
    dt <- data.table(a = integer(),
                     b = integer(),
                     c = integer())
    # No error by default when table contains column "a"
    clm <- new_column("test", empty_reduction_fn, c("a"))
    expect_no_error(check_column(clm, dt))
    # Error when table does not contain column "d"
    clm_fail <- new_column("test", empty_reduction_fn, c("d"))
    expect_error(check_column(clm_fail, dt),
        "columns required by column\\$args: d",
        info = "Column not found in initial pop should cause an error")
})
test_that("Column filter_arg not found in initial pop table triggers stop()", {
    dt <- data.table(a = integer(),
                     b = integer(),
                     c = integer())
    # No error by default when table contains column "a"
    clm <- new_column("test", empty_reduction_fn, c("a"), empty_trajectory_fn, c("a"))
    expect_no_error(check_column(clm, dt))
    # Error when table does not contain column "d"
    clm_fail <- new_column("test", empty_reduction_fn, c("a"), empty_trajectory_fn, c("d"))
    expect_error(check_column(clm_fail, dt),
        "columns required by column\\$filter_args: d",
        info = "Column not found in initial pop should cause an error")
})
test_that("Special column arg not found in initial pop table does not trigger stop()", {
    dt <- data.table(a = integer(),
                     b = integer(),
                     c = integer())
    clm_pass <- new_column("test", empty_reduction_fn, c("a"))
    # No error by default when table contains column "a"
    expect_no_error(check_column(clm_pass, dt))
    # No error when param is special
    clm_pass2 <- new_column("test", empty_reduction_fn, c("~STEP"))
    expect_no_error(check_column(clm_pass2, dt))
})
test_that("Special column filter arg not found in initial pop table does not trigger stop()", {
    dt <- data.table(a = integer(),
                     b = integer(),
                     c = integer())
    clm_pass <- new_column("test", empty_reduction_fn, c("a"), empty_trajectory_fn, c("a"))
    # No error by default when table contains column "a"
    expect_no_error(check_column(clm_pass, dt))
    # No error when param is special
    clm_pass2 <- new_column("test", empty_reduction_fn, c("a"), empty_trajectory_fn, c("~STEP"))
    expect_no_error(check_column(clm_pass2, dt))
})
test_that("Column args length does not match fn args triggers stop()", {
    clm <- new_column("test", empty_reduction_fn, c("a"))
    # No error by default when table contains column "a"
    expect_no_error(check_column(clm))
    # Error when args is wrong length
    clm$args <- c("a", "~STEP")
    expect_error(check_column(clm),
        "does not match number of arguments required",
        info = "args cant be used if they don't match fn")
})
test_that("Column filter args length does not match fn args triggers stop()", {
    clm <- new_column("test", empty_reduction_fn, c("a"), empty_trajectory_fn, c("a"))
    # No error by default when table contains column "a"
    expect_no_error(check_column(clm))
    # Error when filter args is wrong length
    clm$filter_args <- c("a", "~STEP")
    expect_error(check_column(clm),
        "does not match number of arguments required",
        info = "filter args cant be used if they don't match fn")
})
test_that("Column cannot be named '~STEP'", {
    # No error by default
    clm <- new_column("test", empty_reduction_fn, c("a"))
    expect_no_error(check_column(clm))
    # If column name is set to ~STEP an error will be returned
    clm$name <- "~STEP"
    expect_error(check_column(clm),
        "cannot be '~STEP' this column will be automatically generated as part of the returned history data table")
})
test_that("Column filter_args without filter_fn is invalid", {
    # No error by default
    clm <- new_column("test", empty_reduction_fn, c("a"), empty_trajectory_fn, c("a"))
    expect_no_error(check_column(clm))
    # If column name is set to ~STEP an error will be returned
    clm$filter_fn <- NULL
    expect_error(check_column(clm),
        "'column\\$filter_args' provided without 'column\\$filter_fn'")
})

test_that("No args fn is valid/invalid", {
    no_arg_fn <- function() { return (12) }
    # Valid
    expect_no_error(check_trajectory(new_trajectory(no_arg_fn, c(), "age")))
    expect_no_error(check_transition(new_transition(no_arg_fn, c(), "age")))
    trn <- new_transition(empty_transition_fn, c("age"), "age")
    expect_no_error(check_hazard(new_hazard(no_arg_fn, c(), trn)))
    # Invalid
    clm <- 
    expect_error(new_column("test", no_arg_fn, c()),
        "'column\\$args' must not be empty")
})

