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
  if(length(hazard$args) > length(formals(args(hazard$fn)))) {
    stop("length of hazard$args, does not match number of arguments required by hazard$fn: ",
      paste(length(hazard$args), ">", length(formals(args(hazard$fn)))))
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
#' @param transitions Transition object(s) to be applied where the hazard is successful
#' @param freq (Optional) The frequency of hazard execution
#' @param first (Optional) First step the hazard should be enabled
#' @param last (Optional) Last step the hazard should be enabled
#' @return An object of class "eldoradosim_hazard"
new_hazard <- function(fn, args, transitions, freq = 1, first = 0, last = 2147483647) {
  # If transitions is not already a list, upgrade it
  if (inherits(transitions, "eldoradosim_transition")) {
    transitions <- list(transitions)
  }
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
