#' Validate an transition object
#'
#' @param transition An S3 object of class "golden_transition"
#' @param initPop (Optional) data.table to check columns required by functions exist
check_transition <- function(transition, initPop = NULL) {
  validate_S3(transition, "Object", "golden_transition")

  # Are the expected fields present
  required_fields <- c("fn", "args", "state")
  validate_fields_present(transition, "golden_transition", required_fields)

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
  if (!is.character(transition$state) || length(transition$state) == 0L) {
    stop("'transition$state' must be a character vector of 1 or more strings")
  }
  if (!is.null(initPop)) {
      # state exists as a column
      if (any(!(transition$state %in% names(initPop)))) {
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
#' @param state Name(s) of the column(s) where the result of the transition function is to be stored
#' @param name (Optional) Name used in error messages and similar. Defaults to an automatic name
#' @return An object of class "golden_transition"
new_transition <- function(fn, args, state, name = NULL) {
  # Initialise new transition (S3 class)
  transition <- list(
    fn = fn,
    args = args,
    state = state,
    name = get_name(deparse(substitute(fn)), name) # sub required otherwise "fn" is found
  )
  # Assign S3 class
  class(transition) <- "golden_transition"
  # Check transition has correct members of correct types
  check_transition(transition)
  # Return transition
  return(transition)
}

str.golden_transition <- function(x, ..., indent = 0L) {
  ind0 <- paste0(rep.int(" ", indent), collapse = "")
  ind2 <- paste0(rep.int(" ", indent + 2L), collapse = "")
  cat(ind0, "<golden_transition>\n", sep = "")
  cat(ind2, "fn (name): ", x$name, "\n", sep = "")
  cat(ind2, "args: [", paste(x$args, collapse = ", "), "]\n", sep = "")
  cat(ind2, "state: ", x$state, "\n", sep = "")
}
print.golden_transition <- function(x, ...) {
  str(x)
  invisible(x)
}
