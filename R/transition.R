#' Validate an transition object
#'
#' @param transition An S3 object of class "eldoradosim_transition"
#' @param initPop (Optional) data.table to check columns required by functions exist
check_transition <- function(transition, initPop = NULL) {
  validate_S3(transition, "Object", "eldoradosim_transition")

  # Are the expected fields present
  required_fields <- c("fn", "args", "state")
  validate_fields_present(transition, "eldoradosim_transition", required_fields)

  # ---- fn ----
  if (!is.function(transition$fn)) {
    stop("'transition$fn' must be a function")
  }

  # ---- args ----
  # Attempt to convert lists to character vectors
  transition$args <- validate_convert_char_vector(transition$args, "transition$args")
  # Check named columns exist
  if (!is.null(initPop)) {
    validate_columns_exist(transition$args, "transition$args", initPop)
  }
  # Check number of params matches what function requires
  validate_function_args(transition$args, "transition$args", transition$fn)

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
  
  # ---- name ----
  if (!(is.null(transition$name) || (is.character(transition$name) && length(transition$name) == 1))) {
    stop("'transition$name' must be a string")
  }

  return (NULL)
}

#' Create a new transition object
#'
#' @param fn Function defining the transition functions
#' @param args Character vector of parameter names expected by fn
#' @param state Name of the column where the result of the transition function is to be stored
#' @param name (Optional) Name used in error messages and similar. Defaults to an automatic name
#' @return An object of class "eldoradosim_transition"
new_transition <- function(fn, args, state, name = NULL) {
  # Initialise new transition (S3 class)
  transition <- list(
    fn = fn,
    args = args,
    state = state,
    name = get_name(deparse(substitute(fn)), name) # sub required otherwise "fn" is found
  )
  # Assign S3 class
  class(transition) <- "eldoradosim_transition"
  # Check transition has correct members of correct types
  check_transition(transition)
  # Return transition
  return(transition)
}
