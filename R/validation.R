#' Validate that test_object is an S3 object of type s3type
#' 
#' @param test_object The object to validate
#' @param name (String) A name which refers to the object to be included in error messages (it may be what you are passing to test_object as a string)
#' @param s3type (String) The S3 class name to test for
#' @keywords internal
#' @noRd
.validate_S3 <- function(test_object, name, s3type) {
  if (!inherits(test_object, s3type)) {
    stop(name, " is not of class '",s3type,"'")
  }
}

#' Validate that test_object contains fields named by required_fields
#' 
#' @param test_object The object to validate
#' @param name (String) A name which refers to the object to be included in error messages (it may be what you are passing to test_object as a string)
#' @param required_fields (Character Vector) >=1 field names which should be present in test_object
#' @keywords internal
#' @noRd
.validate_fields_present <-function(test_object, name, required_fields) {
  missing_fields <- setdiff(required_fields, names(test_object))
  if (length(missing_fields)) {
    stop(name, " missing required fields: ", paste(missing_fields, collapse = ", "))
  }
}

#' Validate that test_object does not contain additional fields
#' 
#' @param test_object The object to validate
#' @param name (String) A name which refers to the object to be included in error messages (it may be what you are passing to test_object as a string)
#' @param required_fields (Character Vector) >=1 field names which should be present in test_object
#' @keywords internal
#' @noRd
.validate_wrong_fields <-function(test_object, name, required_fields) {
  missing_fields <- setdiff(names(test_object), required_fields)
  if (length(missing_fields)) {
    stop(name, " unexpected fields: ", paste(missing_fields, collapse = ", "))
  }
}

#' Validate that test_object is a list containing >=0 S3 objects of type s3type
#' 
#' @param test_object The object to validate
#' @param name (String) A name which refers to the object to be included in error messages (it may be what you are passing to test_object as a string)
#' @param s3type (String) The S3 class name to test for
#' @keywords internal
#' @noRd
.validate_S3_list <- function(test_object, name, s3type) {
  if (!is.list(test_object)) {
    stop("'", name, "' must be a list")
  }
  if (length(test_object) > 0) {
    ok <- vapply(test_object, function(x) inherits(x, s3type), logical(1))
    if (!all(ok)) {
      stop(
        "All elements of '", name, "' must be S3 objects of class '", s3type, "'. ",
        "Invalid elements at positions: ",
        paste(which(!ok), collapse = ", ")
      )
    }
  }
}

#' Validate that test_object is a whole number (either as an integer or numeric)
#' 
#' @param test_object The object to validate
#' @param name (String) A name which refers to the object to be included in error messages (it may be what you are passing to test_object as a string)
#' @keywords internal
#' @noRd
.validate_whole_number <- function(test_object, name) {
  if (!(is.numeric(test_object) &&
        length(test_object) == 1L &&
        test_object == as.integer(test_object))) {
    stop("'", name, "' must be a whole number")
  }
}

#' Validate that test_object is type logical
#' 
#' @param test_object The object to validate
#' @param name (String) A name which refers to the object to be included in error messages (it may be what you are passing to test_object as a string)
#' @keywords internal
#' @noRd
.validate_logical <- function(test_object, name) {
  if (!(is.logical(test_object))) {
    stop("'",name,"' must be either TRUE or FALSE")
  }
}

#' Validate that test_object is a character vector, and if a list is passed return it as a character vector
#' 
#' @param test_object The object to validate
#' @param name (String) A name which refers to the object to be included in error messages (it may be what you are passing to test_object as a string)
#' @return Either test_object, or test_object converted to a character vector
#' @keywords internal
#' @noRd
.validate_convert_char_vector <- function(test_object, name) {
  if (is.list(test_object)) {
    all_strings <- all(vapply(test_object, function(e) is.character(e) && length(e) == 1, logical(1)))
    if (!all_strings) {
      stop("'",name,"' must only contain strings")
    }
    test_object <- unlist(test_object, use.names = FALSE)
  }
  # Empty char vectors are NULL
  if (!(is.character(test_object)|| is.null(test_object))) {
    stop("'",name,"' must be a character vector")
  }
  if (any(is.na(test_object)) || any(test_object == "")) {
    stop("'",name,"' must not contain NA or empty strings")
  }
  return (test_object)
}

#' Validate that the column names found in test_object exist within initPop
#' 
#' @param test_object The object to validate
#' @param name (String) A name which refers to the object to be included in error messages (it may be what you are passing to test_object as a string)
#' @param initPop (data.table) A data.table to be tested whether test_object columns are present
#' @keywords internal
#' @noRd
.validate_columns_exist <- function(test_object, name, initPop) {
    # Ignore special args (they begin "~")
    clean_args <- test_object[!grepl("^~", test_object)]
    # Which args aren't present among the names of initPop
    missing_columns <- clean_args[!clean_args %in% names(initPop)]
    if (length(missing_columns)) {
      stop("initPop missing columns required by ",name,": ", paste(missing_columns, collapse = ", "))
    }
}

#' (Experimental) Validate whether test_object (function args) look suitable for fn
#'
#' @param test_object The object to validate
#' @param name (String) A name which refers to the object to be included in error messages (it may be what you are passing to test_object as a string)
#' @param fn (Function) A function to check arg count etc of
#' @keywords internal
#' @noRd
.validate_function_args <- function(test_object, name, fn) {
  # Greater than, because of default arg potential
  if(length(test_object) > length(formals(args(fn)))) {
    stop("length of ",name,", does not match number of arguments required by function: ",
      paste(length(test_object), ">", length(formals(args(fn)))))
  }
}

#' Decide an appropriate name for a user provided function
#'
#' @param fn The name of the function as written in the code
#' @param name If not empty string, the name the user provided will be returned
#' @return Name for a function if it can be deduced, else empty string
#' @keywords internal
#' @noRd
.get_name <- function(fn, name) {
  if (is.null(name)) {
    # Only use t as name if it's a name (e.g. it could be a full function body)
    # A string containing a space is a good sign it's not a variable name
    if (!grepl(" ", fn)) {
        return (fn)
    }
    # If name remains null, it will be addressed at the final check
  }
  return (name)
}