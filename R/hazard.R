#' Validate an hazard object
#'
#' @param hazard An S3 object of class "eldoradosim_hazard"
#' @param initPop (Optional) data.table to check columns required by functions exist
check_hazard <- function(hazard, initPop = NULL) {
  validate_S3(hazard, "Object", "eldoradosim_hazard")

  # Are the expected fields present
  required_fields <- c("fn", "args", "transitions", "freq", "first", "last")
  validate_fields_present(hazard, "eldoradosim_hazard", required_fields)

  # ---- fn ----
  if (!is.function(hazard$fn)) {
    stop("'hazard$fn' must be a function")
  }

  # ---- args ----
  # Attempt to convert lists to character vectors
  hazard$args <- validate_convert_char_vector(hazard$args, "hazard$args")
  # Check named columns exist
  if (!is.null(initPop)) {
    validate_columns_exist(hazard$args, "hazard$args", initPop)
  }
  # Check number of params matches what function requires
  validate_function_args(hazard$args, "hazard$args", hazard$fn)

  # ---- transitions ----
  # Check every element is a 'eldoradosim_transition' S3 object
  validate_S3_list(hazard$transitions, "hazard$transitions", "eldoradosim_transition")
  # Nested column check
  if (!is.null(initPop)) {
    for (trn in hazard$transitions) {
      check_transition(trn, initPop)
    }
  }
  
  # ---- freq ----
  validate_whole_number(hazard$freq, "hazard$freq")
  
  # ---- first ----
  validate_whole_number(hazard$first, "hazard$first")
  
  # ---- last ----
  validate_whole_number(hazard$last, "hazard$last")
  
  # ---- name ----
  if (!(is.null(hazard$name) || (is.character(hazard$name) && length(hazard$name) == 1))) {
    stop("'hazard$name' must be a string")
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
#' @param name (Optional) Name used in error messages and similar. Defaults to an automatic name
#' @return An object of class "eldoradosim_hazard"
new_hazard <- function(fn, args, transitions, freq = 1, first = 0, last = 2147483647, name = NULL) {
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
    last = last,
    name = get_name(deparse(substitute(fn)), name) # sub required otherwise "fn" is found
  )  
  # Assign S3 class
  class(hazard) <- "eldoradosim_hazard"
  # Check Hazard has correct members of correct types
  check_hazard(hazard)
  # Return hazard  
  return(hazard)
}
