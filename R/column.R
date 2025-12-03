
#' Validate an history column object
#'
#' @param column An S3 object of class "eldoradosim_history_column"
#' @param initPop (Optional) data.table to check columns required by functions exist
check_column <- function(column, initPop = NULL) {
  if (!inherits(column, "eldoradosim_history_column")) {
    stop("Object is not of class 'eldoradosim_history_column'")
  }

  # Are the expected fields present
  required_fields <- c("name", "fn", "args")
  missing_fields <- setdiff(required_fields, names(column))
  if (length(missing_fields)) {
    stop("Column missing required fields: ", paste(missing_fields, collapse = ", "))
  }

  # ---- name ----
  if (!(is.character(column$name) && length(column$name) == 1)) {
    stop("'column$name' must be a string")
  }
  # ---- fn ----
  if (!is.function(column$fn)) {
    stop("'column$fn' must be a function")
  }

  # ---- args ----
  # Attempt to convert lists to character vectors
  if (is.list(column$args)) {
      all_strings <- all(vapply(column$args, function(e) is.character(e) && length(e) == 1, logical(1)))
      if (!all_strings) {
        stop("'column$args' must only contain strings")
      }
    column$args <- unlist(column$args, use.names = FALSE)
  }
  if (!is.character(column$args)) {
    stop("'column$args' must be a character vector")
  }
  # Check named columns exist
  if (!is.null(initPop)) {
    # Ignore special args (they begin "~")
    clean_args <- column$args[!grepl("^~", column$args)]
    # Which args aren't present among the names of initPop
    missing_columns <- clean_args[!clean_args %in% names(initPop)]
    if (length(missing_columns)) {
      stop("initPop missing columns required by column$args: ", paste(missing_columns, collapse = ", "))
    }
  }
  # Check number of args matches what function requires
  # Greater than, because of default arg potential
  if(length(column$args) > length(formals(args(column$fn)))) {
    stop("length of column$args, does not match number of arguments required by column$fn: ",
      paste(length(column$args), ">", length(formals(args(column$fn)))))
  }

  # ---- filter_fn ----
  if (!is.null(column$filter_fn)) {
      if (!is.function(column$filter_fn)) {
        stop("'column$filter_fn' must be a function")
      }

      # ---- filter_args ----
      # Attempt to convert lists to character vectors
      if (is.list(column$filter_args)) {
          all_strings <- all(vapply(column$filter_args, function(e) is.character(e) && length(e) == 1, logical(1)))
          if (!all_strings) {
            stop("'column$filter_args' must only contain strings")
          }
        column$filter_args <- unlist(column$filter_args, use.names = FALSE)
      }
      if (!is.character(column$filter_args)) {
        stop("'column$filter_args' must be a character vector")
      }
      # Check named columns exist
      if (!is.null(initPop)) {
        # Ignore special args (they begin "~")
        clean_args <- column$filter_args[!grepl("^~", column$filter_args)]
        # Which args aren't present among the names of initPop
        missing_columns <- clean_args[!clean_args %in% names(initPop)]
        if (length(missing_columns)) {
          stop("initPop missing columns required by column$filter_args: ", paste(missing_columns, collapse = ", "))
        }
      }
      # Check number of args matches what function requires
      # Greater than, because of default arg potential
      if(length(column$filter_args) > length(formals(args(column$filter_fn)))) {
        stop("length of column$args, does not match number of arguments required by column$filter_fn: ",
          paste(length(column$filter_args), ">", length(formals(args(column$filter_fn)))))
      }
  } else if (!is.null(column$filter_args)) {
      stop("'column$filter_args' provided without 'column$filter_fn'")
  }
}

#' Create a new eldoradosim_history_column
#'
#' @param name Name of the column in the output data-table
#' @param fn Reduction function, which converts the input columns to a single value
#' @param args Names of columns and special variables to be passed to fn
#' @param filter_fn (Optional) Filter function, which returns a bool vector denoting which rows should be reduced
#' @param filter_args (Optional) Names of columns and special variables to be passed to filter_fn. Required if filter_fn is used.
#' @return An object of class "eldoradosim_history_column"
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
  class(column) <- "eldoradosim_history_column"
  # Check column has correct members of correct types
  check_column(column)
  # Return column
  return(column)
}