#' Validate an history object
#'
#' @param history An S3 object of class "eldoradosim_history"
#' @param initPop (Optional) data.table to check columns required by functions exist
check_history <- function(history, initPop = NULL) {
  validate_S3(history, "Object", "eldoradosim_history")

  # Are the expected fields present
  required_fields <- c("columns", "frequency")
  validate_fields_present(history, "eldoradosim_history", required_fields)
  
  # ---- columns & nested transitions ----
  # Check every element is a 'columns' S3 object
  validate_S3_list(history$columns, "history$columns", "eldoradosim_history_column")
  # Check that every column has a unique name
  names_vec <- vapply(history$columns, `[[`, character(1), "name")
  if(length(unique(names_vec)) != length(names_vec)) {
    stop("Each element of history$columns must have a unique name")
  }
  if (!is.null(initPop)) {
    for (cl in history$columns) {
      check_column(cl, initPop)
    }
  }
    
  # ---- frequency ----
  if (!(is.numeric(history$frequency) &&
        length(history$frequency) == 1L &&
        history$frequency == as.integer(history$frequency))) {
    stop("'history$frequency' must be a positive integer")
  } else if(history$frequency <= 0) {
    stop("'history$frequency' must be a positive integer")
  }
}

#' Create a new eldoradosim_history
#'
#' @param columns eldoradosim_history_column S3 object(s)
#' @param frequency The number of simulation steps per history collection.
#' @return An object of class "eldoradosim_history"
new_history <- function(columns, frequency = 1) {
  # If columns is not already a list, upgrade it
  if (inherits(columns, "eldoradosim_history_column")) {
    columns <- list(columns)
  }
  # Initialise new parameters (S3 class)
  history <- list(
    columns = columns,
    frequency = frequency
  )
  # Assign S3 class
  class(history) <- "eldoradosim_history"
  # Check history has correct members of correct types
  check_history(history)
  # Return history
  return(history)
}
