#' Validate an hazard object
#'
#' @param hazard An S3 object of class "eldoradosim_hazard"
#' @param initPop (Optional) data.table to check columns required by functions exist
check_hazard <- function(hazard, initPop = NULL) {
  if (!inherits(hazard, "eldoradosim_hazard")) {
    stop("Object is not of class 'eldoradosim_hazard'")
  }

  # Are the expected fields present
  required_fields <- c("fn", "args", "transitions", "freq", "first", "last")
  missing_fields <- setdiff(required_fields, names(hazard))
  if (length(missing_fields)) {
    stop("Hazard missing required fields: ", paste(missing_fields, collapse = ", "))
  }

  # ---- fn ----
  if (!is.function(hazard$fn)) {
    stop("'hazard$fn' must be a function")
  }

  # ---- args ----
  # Attempt to convert lists to character vectors
  if (is.list(hazard$args)) {
    all_strings <- all(vapply(hazard$args, function(e) is.character(e) && length(e) == 1, logical(1)))
    if (!all_strings) {
      stop("'hazard$args' must only contain strings")
    }
    hazard$args <- unlist(hazard$args, use.names = FALSE)
  }
  if (!is.character(hazard$args)) {
    stop("'hazard$args' must be a character vector")
  }
  if (any(is.na(hazard$args)) || any(hazard$args == "")) {
    stop("'hazard$args' must not contain NA or empty strings")
  }
  # Check named columns exist
  if (!is.null(initPop)) {
    # Ignore special args (they begin "~")
    clean_args <- hazard$args[!grepl("^~", hazard$args)]
    # Which args aren't present among the names of initPop
    missing_columns <- clean_args[!clean_args %in% names(initPop)]
    if (length(missing_columns)) {
      stop("initPop missing columns required by hazard$args: ", paste(missing_columns, collapse = ", "))
    }
  }
  # Check number of params matches what function requires
  # Greater than, because of default arg potential
  if(length(hazard$args) > length(formals(hazard$fn))) {
    stop("length of hazard$args, does not match number of arguments required by hazard$fn: ",
      paste(length(hazard$args), ">", length(formals(hazard$fn))))
  }

  # ---- transitions ----
  # Check every element is a 'eldoradosim_transition' S3 object
  if (!is.list(hazard$transitions)) {
    stop("'hazard$transitions' must be a list")
  }
  if (length(hazard$transitions) > 0) {
    ok <- vapply(hazard$transitions, function(x) inherits(x, "eldoradosim_transition"), logical(1))
    if (!all(ok)) {
      stop(
        "All elements of 'hazard$transitions' must be S3 objects of class 'eldoradosim_transition'. ",
        "Invalid elements at positions: ",
        paste(which(!ok), collapse = ", ")
      )
    }
  }
  # Nested column check
  if (!is.null(initPop)) {
    for (trn in hazard$transitions) {
      check_transition(trn, initPop)
    }
  }
  
  # ---- freq ----
  if (!(is.numeric(hazard$freq) &&
        length(hazard$freq) == 1L &&
        hazard$freq == as.integer(hazard$freq))) {
    stop("'hazard$freq' must be a whole number")
  }
  
  # ---- first ----
  if (!(is.numeric(hazard$first) &&
        length(hazard$first) == 1L &&
        hazard$first == as.integer(hazard$first))) {
    stop("'hazard$first' must be a whole number")
  }
  
  # ---- last ----
  if (!(is.numeric(hazard$last) &&
        length(hazard$last) == 1L &&
        hazard$last == as.integer(hazard$last))) {
    stop("'hazard$last' must be a whole number")
  }

  return (NULL)
}

#' Create a new hazard object
#'
#' @param fn Function which calculates the hazard likelihood
#' @param args Character vector of parameter names expected by fn
#' @param transitions List of transition objects to be applied where the hazard is successful
#' @param freq (Optional) The frequency of hazard execution
#' @param first (Optional) First step the hazard should be enabled
#' @param last (Optional) Last step the hazard should be enabled
#' @return An object of class "eldoradosim_hazard"
new_hazard <- function(fn, args, transitions, freq = 1, first = 0, last = 2147483647) {
  # Initialise new hazard (S3 class)
  hazard <- list(
    fn = fn,
    args = args,
    transitions = transitions,
    freq = freq,
    first = first,
    last = last
  )  
  # Assign S3 class
  class(hazard) <- "eldoradosim_hazard"
  # Check Hazard has correct members of correct types
  check_hazard(hazard)
  # Return hazard  
  return(hazard)
}

#' Validate an trajectory object
#'
#' @param trajectory An S3 object of class "eldoradosim_trajectory"
#' @param initPop (Optional) data.table to check columns required by functions exist
check_trajectory <- function(trajectory, initPop = NULL) {
  if (!inherits(trajectory, "eldoradosim_trajectory")) {
    stop("Object is not of class 'eldoradosim_trajectory'")
  }

  # Are the expected fields present
  required_fields <- c("fn", "args", "property")
  missing_fields <- setdiff(required_fields, names(trajectory))
  if (length(missing_fields)) {
    stop("trajectory missing required fields: ", paste(missing_fields, collapse = ", "))
  }

  # ---- fn ----
  if (!is.function(trajectory$fn)) {
    stop("'trajectory$fn' must be a function")
  }

  # ---- args ----
  # Attempt to convert lists to character vectors
  if (is.list(trajectory$args)) {
      all_strings <- all(vapply(trajectory$args, function(e) is.character(e) && length(e) == 1, logical(1)))
      if (!all_strings) {
        stop("'trajectory$args' must only contain strings")
      }
    trajectory$args <- unlist(trajectory$args, use.names = FALSE)
  }
  if (!is.character(trajectory$args)) {
    stop("'trajectory$args' must be a character vector")
  }
  # Check named columns exist
  if (!is.null(initPop)) {
    # Ignore special args (they begin "~")
    clean_args <- trajectory$args[!grepl("^~", trajectory$args)]
    # Which args aren't present among the names of initPop
    missing_columns <- clean_args[!clean_args %in% names(initPop)]
    if (length(missing_columns)) {
      stop("initPop missing columns required by trajectory$args: ", paste(missing_columns, collapse = ", "))
    }
  }
  # Check number of params matches what function requires
  # Greater than, because of default arg potential
  if(length(trajectory$args) > length(formals(trajectory$fn))) {
    stop("length of trajectory$args, does not match number of arguments required by trajectory$fn: ",
      paste(length(trajectory$args), ">", length(formals(trajectory$fn))))
  }

  # ---- property ----
  if (!is.character(trajectory$property) || length(trajectory$property) != 1L) {
    stop("'trajectory$property' must be a string (character vector length 1)")
  }
  if (!is.null(initPop)) {
      # property exists as a column
      if (!trajectory$property %in% names(initPop)) {
        stop("initial population columns do not contain trajectory$property: ", trajectory$property)
      }
  }

  return (NULL)
}

#' Create a new trajectory object
#'
#' @param fn Function defining the trajectory functions
#' @param args Character vector of parameter names expected by fn
#' @param property Name of the column where the result of the trajectory function is to be stored
#' @return An object of class "eldoradosim_trajectory"
new_trajectory <- function(fn, args, property) {
  # Initialise new trajectory (S3 class)
  trajectory <- list(
    fn = fn,
    args = args,
    property = property
  )
  # Assign S3 class
  class(trajectory) <- "eldoradosim_trajectory"
  # Check Trajectory has correct members of correct types
  check_trajectory(trajectory)
  # Return trajectory
  return(trajectory)
}

#' Validate an transition object
#'
#' @param transition An S3 object of class "eldoradosim_transition"
#' @param initPop (Optional) data.table to check columns required by functions exist
check_transition <- function(transition, initPop = NULL) {
  if (!inherits(transition, "eldoradosim_transition")) {
    stop("Object is not of class 'eldoradosim_transition'")
  }

  # Are the expected fields present
  required_fields <- c("fn", "args", "state")
  missing_fields <- setdiff(required_fields, names(transition))
  if (length(missing_fields)) {
    stop("Transition missing required fields: ", paste(missing_fields, collapse = ", "))
  }

  # ---- fn ----
  if (!is.function(transition$fn)) {
    stop("'transition$fn' must be a function")
  }

  # ---- args ----
  # Attempt to convert lists to character vectors
  if (is.list(transition$args)) {
    all_strings <- all(vapply(transition$args, function(e) is.character(e) && length(e) == 1, logical(1)))
    if (!all_strings) {
      stop("'transition$args' must only contain strings")
    }
    transition$args <- unlist(transition$args, use.names = FALSE)
  }
  if (!is.character(transition$args)) {
    stop("'transition$args' must be a character vector")
  }
  if (any(is.na(transition$args)) || any(transition$args == "")) {
    stop("'transition$args' must not contain NA or empty strings")
  }
  # Check named columns exist
  if (!is.null(initPop)) {
    # Ignore special args (they begin "~")
    clean_args <- transition$args[!grepl("^~", transition$args)]
    # Which args aren't present among the names of initPop
    missing_columns <- clean_args[!clean_args %in% names(initPop)]
    if (length(missing_columns)) {
      stop("initPop missing columns required by transition$args: ", paste(missing_columns, collapse = ", "))
    }
  }
  # Check number of params matches what function requires
  # Greater than, because of default arg potential
  if(length(transition$args) > length(formals(transition$fn))) {
    stop("length of transition$args, does not match number of arguments required by transition$fn: ",
      paste(length(transition$args), ">", length(formals(transition$fn))))
  }

  # ---- state ----
  if (!is.character(transition$state) || length(transition$state) != 1L) {
    stop("'transition$state' must be a string (character vector length 1)")
  }
  # state exists as a column
  if (!is.null(initPop)) {
      if (!transition$state %in% names(initPop)) {
        stop("initial population columns do not contain transition$state: ", transition$state)
      }
  }

  return (NULL)
}

#' Create a new transition object
#'
#' @param fn Function defining the transition functions
#' @param args Character vector of parameter names expected by fn
#' @param state Name of the column where the result of the transition function is to be stored
#' @return An object of class "eldoradosim_transition"
new_transition <- function(fn, args, state) {
  # Initialise new transition (S3 class)
  transition <- list(
    fn = fn,
    args = args,
    state = state
  )
  # Assign S3 class
  class(transition) <- "eldoradosim_transition"
  # Check transition has correct members of correct types
  check_transition(transition)
  # Return transition
  return(transition)
}

#' Validate the configuration passed to run_simulation()
#'
#' @param parameters An eldoradosim_parameters S3 object to be validated
#' @param initPop data.frame which contains the columns required by parameters
check_parameters <- function(parameters, initPop = NULL) {
  if (!inherits(parameters, "eldoradosim_parameters")) {
    stop("Object is not of class 'eldoradosim_parameters'")
  }

  # Are the expected fields present
  required_fields <- c("hazards", "trajectories", "steps", "random_seed", "debug")
  missing_fields <- setdiff(required_fields, names(parameters))
  if (length(missing_fields)) {
    stop("eldoradosim_parameters missing required fields: ", paste(missing_fields, collapse = ", "))
  }
  
  # ---- hazards & nested transitions ----
  # Check every element is a 'hazard' S3 object
  if (!is.list(parameters$hazards)) {
    stop("'parameters$hazards' must be a list")
  }
  if (length(parameters$hazards) > 0) {
    ok <- vapply(parameters$hazards, function(x) inherits(x, "eldoradosim_hazard"), logical(1))
    if (!all(ok)) {
      stop(
        "All elements of 'parameters$hazards' must be S3 objects of class 'eldoradosim_hazard'. ",
        "Invalid elements at positions: ",
        paste(which(!ok), collapse = ", ")
      )
    }
  }
  if (!is.null(initPop)) {
    for (hz in parameters$hazards) {
      check_hazard(hz, initPop)
    }
  }
  
  # ---- trajectories ----
  if (!is.list(parameters$trajectories)) {
    stop("'parameters$trajectories' must be a list")
  }
  if (length(parameters$trajectories) > 0) {
    ok <- vapply(parameters$trajectories, function(x) inherits(x, "eldoradosim_trajectory"), logical(1))
    if (!all(ok)) {
      stop(
        "All elements of 'parameters$trajectories' must be S3 objects of class 'eldoradosim_trajectory'. ",
        "Invalid elements at positions: ",
        paste(which(!ok), collapse = ", ")
      )
    }
  }
  if (!is.null(initPop)) {
    for (trj in parameters$trajectories) {
      check_trajectory(trj, initPop)
    }
  } 
  
  # ---- steps ----
  if (!(is.numeric(parameters$steps) &&
        length(parameters$steps) == 1L &&
        parameters$steps == as.integer(parameters$steps))) {
    stop("'parameters$steps' must be a whole number")
  }
  # ---- history ----
  if (!is.null(parameters$history)) {
    if (!inherits(parameters$history, "eldoradosim_history")) {
      stop("parameters$history is not of class 'eldoradosim_history'")
    }
  } 
  if (!is.null(initPop)) {
    check_history(parameters$history, initPop)
  }
  
  # ---- random_seed ----
  if (!(is.numeric(parameters$random_seed) &&
        length(parameters$random_seed) == 1L &&
        parameters$random_seed == as.integer(parameters$random_seed))) {
    stop("'parameters$random_seed' must be a whole number")
  }
  
  # ---- debug ----
  if (!(is.logical(parameters$debug))) {
    stop("'parameters$debug' must be either TRUE or FALSE")
  }

}

#' Create a new eldoradosim_parameters
#'
#' @param hazards List of eldoradosim_hazard S3 objects
#' @param trajectories List of eldoradosim_trajectory S3 objects
#' @param steps Number of steps to run
#' @param random_seed Seed to be used for random generation. If set 0, current time will be used.
#' @param debug (TRUE/FALSE) flag indicating whether validation checks are enabled. These catch NaN, but reduce performance
#' @param history eldoradosim_history S3 object representing the columns of data to be agregated during simulation
#' @return An object of class "eldoradosim_parameters"
new_parameters <- function(hazards, trajectories, steps, random_seed = 0, debug = TRUE, history = NULL) {
  # Initialise new parameters (S3 class)
  parameters <- list(
    hazards = hazards,
    trajectories = trajectories,
    steps = steps,
    history = history,
    random_seed = random_seed,
    debug = debug
  )
  # Assign S3 class
  class(parameters) <- "eldoradosim_parameters"
  # Check parameters has correct members of correct types
  check_parameters(parameters)
  # Return parameters
  return(parameters)
}


#' Validate an history object
#'
#' @param history An S3 object of class "eldoradosim_history"
#' @param initPop (Optional) data.table to check columns required by functions exist
check_history <- function(history, initPop = NULL) {
  if (!inherits(history, "eldoradosim_history")) {
    stop("Object is not of class 'eldoradosim_history'")
  }

  # Are the expected fields present
  required_fields <- c("columns", "frequency")
  missing_fields <- setdiff(required_fields, names(history))
  if (length(missing_fields)) {
    stop("eldoradosim_history missing required fields: ", paste(missing_fields, collapse = ", "))
  }
  
  # ---- columns & nested transitions ----
  # Check every element is a 'columns' S3 object
  if (!is.list(history$columns)) {
    stop("'history$columns' must be a list")
  }
  if (length(history$columns) > 0) {
    ok <- vapply(history$columns, function(x) inherits(x, "eldoradosim_history_column"), logical(1))
    if (!all(ok)) {
      stop(
        "All elements of 'history$columns' must be S3 objects of class 'eldoradosim_history_column'. ",
        "Invalid elements at positions: ",
        paste(which(!ok), collapse = ", ")
      )
    }
  }
  if (!is.null(initPop)) {
    for (cl in history$columns) {
      check_column(cl, initPop)
    }
  }
    
  # ---- frequency ----
  if (!(is.numeric(history$frequency) &&
        length(history$frequency) == 1L &&
        history$frequency == as.integer(history$frequency))) {
    stop("'history$frequency' must be a positive integer")
  } else if(history$frequency <= 0) {
    stop("'history$frequency' must be a positive integer")
  }
}

#' Create a new eldoradosim_history
#'
#' @param columns List of eldoradosim_history_column
#' @param frequency The number of simulation steps per history collection.
#' @return An object of class "eldoradosim_history"
new_history <- function(columns, frequency = 1) {
  # Initialise new parameters (S3 class)
  history <- list(
    columns = columns,
    frequency = frequency
  )
  # Assign S3 class
  class(history) <- "eldoradosim_history"
  # Check history has correct members of correct types
  check_history(history)
  # Return history
  return(history)
}

#' Validate an history column object
#'
#' @param column An S3 object of class "eldoradosim_history_column"
#' @param initPop (Optional) data.table to check columns required by functions exist
check_column <- function(column, initPop = NULL) {
  if (!inherits(column, "eldoradosim_history_column")) {
    stop("Object is not of class 'eldoradosim_history_column'")
  }

  # Are the expected fields present
  required_fields <- c("name", "fn", "args")
  missing_fields <- setdiff(required_fields, names(column))
  if (length(missing_fields)) {
    stop("Column missing required fields: ", paste(missing_fields, collapse = ", "))
  }

  # ---- name ----
  if (!(is.character(column$name) && length(column$name) == 1)) {
    stop("'column$name' must be a string")
  }
  # ---- fn ----
  if (!is.function(column$fn)) {
    stop("'column$fn' must be a function")
  }

  # ---- args ----
  # Attempt to convert lists to character vectors
  if (is.list(column$args)) {
      all_strings <- all(vapply(column$args, function(e) is.character(e) && length(e) == 1, logical(1)))
      if (!all_strings) {
        stop("'column$args' must only contain strings")
      }
    column$args <- unlist(column$args, use.names = FALSE)
  }
  if (!is.character(column$args)) {
    stop("'column$args' must be a character vector")
  }
  # Check named columns exist
  if (!is.null(initPop)) {
    # Ignore special args (they begin "~")
    clean_args <- column$args[!grepl("^~", column$args)]
    # Which args aren't present among the names of initPop
    missing_columns <- clean_args[!clean_args %in% names(initPop)]
    if (length(missing_columns)) {
      stop("initPop missing columns required by column$args: ", paste(missing_columns, collapse = ", "))
    }
  }
  # Check number of args matches what function requires
  # Greater than, because of default arg potential
  if(length(column$args) > length(formals(column$fn))) {
    stop("length of column$args, does not match number of arguments required by column$fn: ",
      paste(length(column$args), ">", length(formals(column$fn))))
  }

  # ---- filter_fn ----
  if (!is.null(column$filter_fn)) {
      if (!is.function(column$filter_fn)) {
        stop("'column$filter_fn' must be a function")
      }

      # ---- filter_args ----
      # Attempt to convert lists to character vectors
      if (is.list(column$filter_args)) {
          all_strings <- all(vapply(column$filter_args, function(e) is.character(e) && length(e) == 1, logical(1)))
          if (!all_strings) {
            stop("'column$filter_args' must only contain strings")
          }
        column$filter_args <- unlist(column$filter_args, use.names = FALSE)
      }
      if (!is.character(column$filter_args)) {
        stop("'column$filter_args' must be a character vector")
      }
      # Check named columns exist
      if (!is.null(initPop)) {
        # Ignore special args (they begin "~")
        clean_args <- column$filter_args[!grepl("^~", column$filter_args)]
        # Which args aren't present among the names of initPop
        missing_columns <- clean_args[!clean_args %in% names(initPop)]
        if (length(missing_columns)) {
          stop("initPop missing columns required by column$filter_args: ", paste(missing_columns, collapse = ", "))
        }
      }
      # Check number of args matches what function requires
      # Greater than, because of default arg potential
      if(length(column$filter_args) > length(formals(column$filter_fn))) {
        stop("length of column$args, does not match number of arguments required by column$filter_fn: ",
          paste(length(column$filter_args), ">", length(formals(column$filter_fn))))
      }
  } else if (!is.null(column$filter_args)) {
      stop("'column$filter_args' provided without 'column$filter_fn'")
  }
}

#' Create a new eldoradosim_history_column
#'
#' @param name Name of the column in the output data-table
#' @param fn Reduction function, which converts the input columns to a single value
#' @param args Names of columns and special variables to be passed to fn
#' @param filter_fn (Optional) Filter function, which returns a bool vector denoting which rows should be reduced
#' @param filter_args (Optional) Names of columns and special variables to be passed to filter_fn. Required if filter_fn is used.
#' @return An object of class "eldoradosim_history_column"
new_column <- function(name, fn, args, filter_fn = NULL, filter_args = NULL) {
  # Initialise new parameters (S3 class)
  column <- list(
    name = name,
    fn = fn,
    args = args,
    filter_fn = filter_fn,
    filter_args = filter_args
  )
  # Assign S3 class
  class(column) <- "eldoradosim_history_column"
  # Check column has correct members of correct types
  check_column(column)
  # Return column
  return(column)
}