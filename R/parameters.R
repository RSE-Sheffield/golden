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
#' @param hazards eldoradosim_hazard S3 object(s)
#' @param trajectories eldoradosim_trajectory S3 object(s)
#' @param steps Number of steps to run
#' @param random_seed Seed to be used for random generation. If set 0, current R random state will be used.
#' @param debug (TRUE/FALSE) flag indicating whether validation checks are enabled. These catch NaN, but reduce performance
#' @param history eldoradosim_history S3 object representing the columns of data to be aggregated during simulation
#' @return An object of class "eldoradosim_parameters"
new_parameters <- function(hazards, trajectories, steps, random_seed = 0, debug = TRUE, history = NULL) {
  # If hazards is not already a list, upgrade it
  if (inherits(hazards, "eldoradosim_hazard")) {
    hazards <- list(hazards)
  }
  # If trajectories is not already a list, upgrade it
  if (inherits(trajectories, "eldoradosim_trajectory")) {
    trajectories <- list(trajectories)
  }
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
