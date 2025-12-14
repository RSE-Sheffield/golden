#' Validate the configuration passed to run_simulation()
#'
#' @param parameters An eldoradosim_parameters S3 object to be validated
#' @param initPop data.frame which contains the columns required by parameters
check_parameters <- function(parameters, initPop = NULL) {
  validate_S3(parameters, "Object", "eldoradosim_parameters")

  # Are the expected fields present
  required_fields <- c("hazards", "trajectories", "steps", "random_seed", "debug")
  validate_fields_present(parameters, "eldoradosim_parameters", required_fields)
  
  # ---- hazards & nested transitions ----
  # Check every element is a 'hazard' S3 object
  validate_S3_list(parameters$hazards, "parameters$hazards", "eldoradosim_hazard")
  if (!is.null(initPop)) {
    for (hz in parameters$hazards) {
      check_hazard(hz, initPop)
    }
    # Init missing hazard and transition names
    for (i in seq_len(length(parameters$hazards))) {
      if (is.null(parameters$hazards[[i]]$name)) {
        parameters$hazards[[i]]$name = paste0("parameters$hazards[[", i, "]]")
      }
      for (j in seq_len(length(parameters$hazards[[i]]$transitions))) {
        if (is.null(parameters$hazards[[i]]$transitions[[j]]$name)) {
          parameters$hazards[[i]]$transitions[[j]]$name = paste0("parameters$hazards[[", i, "]]$transitions[[", j, "]]")
        }
      }
    }
  }
  
  # ---- trajectories ----
  validate_S3_list(parameters$trajectories, "parameters$trajectories", "eldoradosim_trajectory")
  if (!is.null(initPop)) {
    for (trj in parameters$trajectories) {
      check_trajectory(trj, initPop)
    }
    # Init missing trajectory names
    for (i in seq_len(length(parameters$trajectories))) {
      if (is.null(parameters$trajectories[[i]]$name)) {
        parameters$trajectories[[i]]$name = paste0("parameters$trajectories[[", i, "]]")
      }
    }
  } 
  
  # ---- steps ----
  validate_whole_number(parameters$steps, "parameters$steps")
  # ---- history ----  
  if (!is.null(parameters$history)) {  
    validate_S3(parameters$history, "parameters$history", "eldoradosim_history")
  }
  if (!is.null(initPop)) {
    check_history(parameters$history, initPop)
  }
  
  # ---- random_seed ----
  validate_whole_number(parameters$random_seed, "parameters$random_seed")
  
  # ---- debug ----
  validate_logical(parameters$debug, "parameters$debug")
  
  return (parameters)
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
  # This will also resolve null names
  parameters <- check_parameters(parameters)
  # Return parameters
  return(parameters)
}
