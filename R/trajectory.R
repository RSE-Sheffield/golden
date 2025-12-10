#' Validate an trajectory object
#'
#' @param trajectory An S3 object of class "golden_trajectory"
#' @param initPop (Optional) data.table to check columns required by functions exist
check_trajectory <- function(trajectory, initPop = NULL) {
  .validate_S3(trajectory, "Object", "golden_trajectory")

  # Are the expected fields present
  required_fields <- c("fn", "args", "property")
  .validate_fields_present(trajectory, "golden_trajectory", required_fields)

  # ---- fn ----
  if (!is.function(trajectory$fn)) {
    stop("'trajectory$fn' must be a function")
  }

  # ---- args ----
  # Attempt to convert lists to character vectors
  trajectory$args <- .validate_convert_char_vector(trajectory$args, "trajectory$args")
  # Check named columns exist
  if (!is.null(initPop)) {
    .validate_columns_exist(trajectory$args, "trajectory$args", initPop)
  }
  # Check number of params matches what function requires
  .validate_function_args(trajectory$args, "trajectory$args", trajectory$fn)

  # ---- property ----
  if (!is.character(trajectory$property) || length(trajectory$property) == 0L) {
    stop("'trajectory$property' must be a character vector of 1 or more strings")
  }
  if (!is.null(initPop)) {
      # property exists as a column
      if (any(!(trajectory$property %in% names(initPop)))) {
        stop("initial population columns do not contain trajectory$property: ", trajectory$property)
      }
  }
  
  # ---- name ----
  if (!(is.null(trajectory$name) || (is.character(trajectory$name) && length(trajectory$name) == 1))) {
    stop("'trajectory$name' must be a string")
  }

  return (NULL)
}

#' Create a new trajectory object
#'
#' @param fn Function defining the trajectory function
#' @param args Character vector of parameter names expected by fn
#' @param property Name(s) of the column(s) where the result(s) of the trajectory function is to be stored
#' @param name (Optional) Name used in error messages and similar. Defaults to an automatic name
#' @return An object of class "golden_trajectory"
#' @note If a list if passed to property, fn must return a list of equal length
new_trajectory <- function(fn, args, property, name = NULL) {
  # Initialise new trajectory (S3 class)
  trajectory <- list(
    fn = fn,
    args = args,
    property = property,
    name = .get_name(deparse(substitute(fn)), name) # sub required otherwise "fn" is found
  )
  # Assign S3 class
  class(trajectory) <- "golden_trajectory"
  # Check Trajectory has correct members of correct types
  check_trajectory(trajectory)
  # Return trajectory
  return(trajectory)
}

#' Print the contents of a golden_trajectory type S3 object
#'
#' @param x The object to be printed
#' @param ... Not used. Included for S3 method compatibility.
#' @param indent (Optional) The level the printing is indented, useful if nested within another S3 object
print.golden_trajectory <- function(x, ..., indent = 0L) {
  ind0 <- paste0(rep.int(" ", indent), collapse = "")
  ind2 <- paste0(rep.int(" ", indent + 2L), collapse = "")
  cat(ind0, "<golden_trajectory>\n", sep = "")
  cat(ind2, "fn (name): ", x$name, "\n", sep = "")
  cat(ind2, "args: [", paste(x$args, collapse = ", "), "]\n", sep = "")
  if (length(x$property) == 1) {
    cat(ind2, "property: ", x$property, "\n", sep = "")
  } else {
    cat(ind2, "property: [", paste(x$property, collapse = ", "), "]\n", sep = "")
  }
  invisible(x)
}
