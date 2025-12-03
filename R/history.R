#' Validate an history object
#'
#' @param history An S3 object of class "eldoradosim_history"
#' @param initPop (Optional) data.table to check columns required by functions exist
check_history <- function(history, initPop = NULL) {
  if (!inherits(history, "eldoradosim_history")) {
    stop("Object is not of class 'eldoradosim_history'")
  }

  # Are the expected fields present
  required_fields <- c("columns", "frequency")
  missing_fields <- setdiff(required_fields, names(history))
  if (length(missing_fields)) {
    stop("eldoradosim_history missing required fields: ", paste(missing_fields, collapse = ", "))
  }
  
  # ---- columns & nested transitions ----
  # Check every element is a 'columns' S3 object
  if (!is.list(history$columns)) {
    stop("'history$columns' must be a list")
  }
  if (length(history$columns) > 0) {
    ok <- vapply(history$columns, function(x) inherits(x, "eldoradosim_history_column"), logical(1))
    if (!all(ok)) {
      stop(
        "All elements of 'history$columns' must be S3 objects of class 'eldoradosim_history_column'. ",
        "Invalid elements at positions: ",
        paste(which(!ok), collapse = ", ")
      )
    }
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
