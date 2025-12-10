#' Validate the configuration passed to run_simulation()
#'
#' @param parameters An golden_parameters S3 object to be validated
#' @param initPop data.frame which contains the columns required by parameters
check_parameters <- function(parameters, initPop = NULL) {
  # initpop must be derived from data.frame (e.g. data.table)
  if (!is.null(initPop)) {
    if (!inherits(initPop, "data.frame")) {
      stop("initPop type must inherit from class data.frame (e.g. data.table)")
    }
  }

  .validate_S3(parameters, "Object", "golden_parameters")

  # Are the expected fields present
  required_fields <- c("hazards", "trajectories", "steps", "random_seed", "debug", "print_timing")
  .validate_fields_present(parameters, "golden_parameters", required_fields)
  
  # ---- hazards & nested transitions ----
  # Check every element is a 'hazard' S3 object
  .validate_S3_list(parameters$hazards, "parameters$hazards", "golden_hazard")
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
  .validate_S3_list(parameters$trajectories, "parameters$trajectories", "golden_trajectory")
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
  .validate_whole_number(parameters$steps, "parameters$steps")
  # ---- history ----  
  if (!is.null(parameters$history)) {  
    .validate_S3(parameters$history, "parameters$history", "golden_history")
    if (!is.null(initPop)) {
      check_history(parameters$history, initPop)
    }
  }
  
  # ---- random_seed ----
  .validate_whole_number(parameters$random_seed, "parameters$random_seed")
  
  # ---- debug ----
  .validate_logical(parameters$debug, "parameters$debug")
  
  # ---- print_timing ----
  .validate_logical(parameters$print_timing, "parameters$print_timing")
  
  return (parameters)
}

#' Create a new golden_parameters
#'
#' @param hazards golden_hazard S3 object(s)
#' @param trajectories golden_trajectory S3 object(s)
#' @param steps Number of steps to run
#' @param random_seed Seed to be used for random generation. If set 0, current R random state will be used.
#' @param debug (TRUE/FALSE) flag indicating whether validation checks are enabled. These catch NaN, but reduce performance
#' @param print_timing (TRUE/FALSE) flag indicating whether a per-function timing report should be printed after the simulation, this will always be suppressed for fast (<= 1 second) simulations.
#' @param history golden_history S3 object representing the columns of data to be aggregated during simulation
#' @return An object of class "golden_parameters"
new_parameters <- function(hazards = list(), trajectories = list(), steps, random_seed = 0, debug = TRUE, print_timing = TRUE, history = NULL) {
  # If hazards is not already a list, upgrade it
  if (inherits(hazards, "golden_hazard")) {
    hazards <- list(hazards)
  }
  # If trajectories is not already a list, upgrade it
  if (inherits(trajectories, "golden_trajectory")) {
    trajectories <- list(trajectories)
  }
  # Initialise new parameters (S3 class)
  parameters <- list(
    hazards = hazards,
    trajectories = trajectories,
    steps = steps,
    history = history,
    random_seed = random_seed,
    debug = debug,
    print_timing = print_timing
  )
  # Assign S3 class
  class(parameters) <- "golden_parameters"
  # Check parameters has correct members of correct types
  # This will also resolve null names
  parameters <- check_parameters(parameters)
  # Return parameters
  return(parameters)
}

#' Print the contents of a golden_parameters type S3 object
#'
#' @param x The object to be printed
#' @param ... Not used. Included for S3 method compatibility.
#' @param indent (Optional) The level the printing is indented, useful if nested within another S3 object
print.golden_parameters <- function(x, ..., indent = 0) {
  ind0 <- paste0(rep.int(" ", indent), collapse = "")
  ind2 <- paste0(rep.int(" ", indent + 2L), collapse = "")
  cat(ind0, "<golden_parameters>\n", sep = "")
  if (!is.null(x$hazards)) {  
    cat(ind2, "hazards: [\n", sep = "")
    for (t in x$hazards) {
        print(t, indent = indent + 4)
    }
    cat(ind2, "]\n", sep = "")
  }
  if (!is.null(x$trajectories)) {
    cat(ind2, "trajectories: [\n", sep = "")
    for (t in x$trajectories) {
        print(t, indent = indent + 4)
    }
    cat(ind2, "]\n", sep = "")
  }
  if (!is.null(x$history)) {
    cat(ind2, "history:\n", sep = "")
    print(x$history, indent=indent + 4)
  }
  cat(ind2, "steps: ", x$steps, "\n", sep = "")
  cat(ind2, "random_seed: ", x$random_seed, "\n", sep = "")
  cat(ind2, "print_timing: ", x$print_timing, "\n", sep = "")
  cat(ind2, "debug: ", x$debug, "\n", sep = "")
  invisible(x)
}
