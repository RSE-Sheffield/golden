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
  if(length(transition$args) > length(formals(args(transition$fn)))) {
    stop("length of transition$args, does not match number of arguments required by transition$fn: ",
      paste(length(transition$args), ">", length(formals(args(transition$fn)))))
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
