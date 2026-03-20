#' Validate an trajectory object
#' If validation fails, an exception will be raised.
#'
#' @param trajectory An S3 object of class "golden_trajectory"
#' @param initPop (Optional) data.table to check columns required by functions exist
#' @return No return value, called for side effects.
#'
#' @examples
#' library(data.table)
#' dt <- data.table(b = rep(0, 100))
#' # Define a trajectory function, which adds 2 to all members of the input vector
#' test_trajectory <- function(a) {
#'     return (a + 2)
#' }
#' # Create an S3 golden_trajectory
#' trj <- new_trajectory(test_trajectory, c("b"), "b")
#' # check_trajectory() will not throw an exception
#' # as trj is a valid S3 golden_trajectory
#' # and dt contains column "b"
#' check_trajectory(trj, dt)
check_trajectory <- function(trajectory, initPop = NULL) {
  .validate_S3(trajectory, "Object", "golden_trajectory")

  # Are the expected fields present
  required_fields <- c("fn", "args", "states")
  .validate_fields_present(trajectory, "golden_trajectory", required_fields)

  # Are there any unexpected fields
  required_fields <- append(required_fields, c("name"))
  .validate_wrong_fields(trajectory, "golden_trajectory", required_fields)

  # ---- fn ----
  if (!is.function(trajectory$fn)) {
    stop("'trajectory$fn' must be a function")
  }

  # ---- args ----
  # Attempt to convert lists to character vectors
  trajectory$args <- .validate_convert_char_vector(
    trajectory$args,
    "trajectory$args"
  )
  # Check named columns exist
  if (!is.null(initPop)) {
    .validate_columns_exist(trajectory$args, "trajectory$args", initPop)
  }
  # Check number of params matches what function requires
  .validate_function_args(trajectory$args, "trajectory$args", trajectory$fn)

  # ---- states ----
  if (!is.character(trajectory$states) || length(trajectory$states) == 0L) {
    stop("'trajectory$states' must be a character vector of 1 or more strings")
  }
  if (!is.null(initPop)) {
    # states exist as a column
    if (any(!(trajectory$states %in% names(initPop)))) {
      stop(
        "initial population columns do not contain trajectory$states: ",
        trajectory$states
      )
    }
  }

  # ---- name ----
  if (
    !(is.null(trajectory$name) ||
      (is.character(trajectory$name) && length(trajectory$name) == 1))
  ) {
    stop("'trajectory$name' must be a string")
  }

  return(NULL)
}

#' Create a new trajectory object
#'
#' @param fn Function defining the trajectory function
#' @param args Character vector of parameter names expected by fn
#' @param states Name(s) of the column(s) where the result(s) of the trajectory function is to be stored
#' @param name (Optional) Name used in error messages and similar. Defaults to an automatic name
#' @return An object of class "golden_trajectory"
#' @note If a list if passed to states, fn must return a list of equal length
#'
#' @examples
#' # Define a trajectory function, which adds 2 to all members of the input vector
#' test_trajectory <- function(a) {
#'     return (a + 2)
#' }
#' # Create an S3 golden_trajectory
#' trj <- new_trajectory(test_trajectory, c("b"), "b")
new_trajectory <- function(fn, args, states, name = NULL) {
  # Initialise new trajectory (S3 class)
  trajectory <- list(
    fn = fn,
    args = args,
    states = states,
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
#' @return No return value, called for side effects.
#'
#' @examples
#' # Define a trajectory function, which adds 2 to all members of the input vector
#' test_trajectory <- function(a) {
#'     return (a + 2)
#' }
#' # Create an S3 golden_trajectory
#' trj <- new_trajectory(test_trajectory, c("b"), "b")
#' print(trj)
print.golden_trajectory <- function(x, ..., indent = 0L) {
  ind0 <- paste0(rep.int(" ", indent), collapse = "")
  ind2 <- paste0(rep.int(" ", indent + 2L), collapse = "")
  cat(ind0, "<golden_trajectory>\n", sep = "")
  cat(ind2, "fn (name): ", x$name, "\n", sep = "")
  cat(ind2, "args: [", paste(x$args, collapse = ", "), "]\n", sep = "")
  if (length(x$states) == 1) {
    cat(ind2, "states: ", x$states, "\n", sep = "")
  } else {
    cat(
      ind2,
      "states: [",
      paste(x$states, collapse = ", "),
      "]\n",
      sep = ""
    )
  }
  invisible(x)
}
