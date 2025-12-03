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
  if(length(trajectory$args) > length(formals(args(trajectory$fn)))) {
    stop("length of trajectory$args, does not match number of arguments required by trajectory$fn: ",
      paste(length(trajectory$args), ">", length(formals(args(trajectory$fn)))))
  }

  # ---- property ----
  if (!is.character(trajectory$property) || length(trajectory$property) == 0L) {
    stop("'trajectory$property' must be a character vector of 1 or more strings")
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
#' @param fn Function defining the trajectory function
#' @param args Character vector of parameter names expected by fn
#' @param property Name(s) of the column(s) where the result(s) of the trajectory function is to be stored
#' @return An object of class "eldoradosim_trajectory"
#' @note If a list if passed to property, fn must return a list of equal length
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
