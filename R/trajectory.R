#' Validate an trajectory object
#'
#' @param trajectory An S3 object of class "eldoradosim_trajectory"
#' @param initPop (Optional) data.table to check columns required by functions exist
check_trajectory <- function(trajectory, initPop = NULL) {
  validate_S3(trajectory, "Object", "eldoradosim_trajectory")

  # Are the expected fields present
  required_fields <- c("fn", "args", "property")
  validate_fields_present(trajectory, "eldoradosim_trajectory", required_fields)

  # ---- fn ----
  if (!is.function(trajectory$fn)) {
    stop("'trajectory$fn' must be a function")
  }

  # ---- args ----
  # Attempt to convert lists to character vectors
  trajectory$args <- validate_convert_char_vector(trajectory$args, "trajectory$args")
  # Check named columns exist
  if (!is.null(initPop)) {
    validate_columns_exist(trajectory$args, "trajectory$args", initPop)
  }
  # Check number of params matches what function requires
  validate_function_args(trajectory$args, "trajectory$args", trajectory$fn)

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
