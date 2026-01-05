#' Validate an history column object
#'
#' @param column An S3 object of class "golden_history_column"
#' @param initPop (Optional) data.table to check columns required by functions exist
check_column <- function(column, initPop = NULL) {
  .validate_S3(column, "Object", "golden_history_column")

  # Are the expected fields present
  required_fields <- c("name", "fn", "args")
  .validate_fields_present(column, "golden_history_column", required_fields)
  
  # Are there any unexpected fields
  required_fields <- append(required_fields, c("filter_fn", "filter_args"))
  .validate_wrong_fields(column, "golden_history_column", required_fields)

  # ---- name ----
  if (!(is.character(column$name) && length(column$name) == 1)) {
    stop("'column$name' must be a string")
  } else if (column$name == "~STEP") {
    # Check that column name is not reserved
    stop("'column$name' cannot be '~STEP' this column will be automatically generated as part of the returned history data table.")
  }
  # ---- fn ----
  if (!is.function(column$fn)) {
    stop("'column$fn' must be a function")
  }

  # ---- args ----
  # Attempt to convert lists to character vectors
  if (is.null(column$args)) {
    stop("'column$args' must not be empty")
  }
  column$args <- .validate_convert_char_vector(column$args, "column$args")
  # Check named columns exist
  if (!is.null(initPop)) {
    .validate_columns_exist(column$args, "column$args", initPop)
  }
  # Check number of args matches what function requires
  .validate_function_args(column$args, "column$args", column$fn)

  # ---- filter_fn ----
  if (!is.null(column$filter_fn)) {
      if (!is.function(column$filter_fn)) {
        stop("'column$filter_fn' must be a function")
      }

      # ---- filter_args ----
      # Attempt to convert lists to character vectors
      column$filter_args <- .validate_convert_char_vector(column$filter_args, "column$filter_args")
      # Check named columns exist
      if (!is.null(initPop)) {
        .validate_columns_exist(column$filter_args, "column$filter_args", initPop)
      }
      # Check number of args matches what function requires
      .validate_function_args(column$filter_args, "column$filter_args", column$filter_fn)
  } else if (!is.null(column$filter_args)) {
      stop("'column$filter_args' provided without 'column$filter_fn'")
  }
}

#' Create a new golden_history_column
#'
#' @param name Name of the column in the output data-table
#' @param fn Reduction function, which converts the input columns to a single value
#' @param args Names of columns and special variables to be passed to fn
#' @param filter_fn (Optional) Filter function, which returns a bool vector denoting which rows should be reduced
#' @param filter_args (Optional) Names of columns and special variables to be passed to filter_fn. Required if filter_fn is 
#' @return An object of class "golden_history_column"
new_column <- function(name, fn, args, filter_fn = NULL, filter_args = NULL) {
  # Initialise new parameters (S3 class)
  column <- list(
    name = name,
    fn = fn,
    args = args,
    filter_fn = filter_fn,
    filter_args = filter_args
  )
  # Assign S3 class
  class(column) <- "golden_history_column"
  # Check column has correct members of correct types
  check_column(column)
  # Return column
  return(column)
}

#' Print the contents of a golden_history_column type S3 object
#'
#' @param x The object to be printed
#' @param ... Not used. Included for S3 method compatibility.
#' @param indent (Optional) The level the printing is indented, useful if nested within another S3 object
print.golden_history_column <- function(x, ..., indent = 0L) {
  ind0 <- paste0(rep.int(" ", indent), collapse = "")
  ind2 <- paste0(rep.int(" ", indent + 2L), collapse = "")
  cat(ind0, "<golden_history_column>\n", sep = "")
  cat(ind2, "name: ", x$name, "\n", sep = "")
  cat(ind2, "args: [", paste(x$args, collapse = ", "), "]\n", sep = "")
  if (!is.null(x$filter_fn)) {
    cat(ind2, "filter_args: [", paste(x$filter_args, collapse = ", "), "]\n", sep = "")
  }
  invisible(x)
}
