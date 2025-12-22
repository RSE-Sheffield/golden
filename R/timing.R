print.golden_timing <- function(x, ...) {
  cat("<golden_timing>\n", sep = "")
  if (!is.null(x$hazard)) {
    cat("\n  Hazards: \n")
    print(x$hazard)
  }
  if (!is.null(x$transition)) {
    cat("\n  Transitions: \n")
    print(x$transition)
  }
  if (!is.null(x$trajectory)) {
    cat("\n  Trajectories: \n")
    print(x$trajectory)
  }
  if (!is.null(x$columns)) {
    cat("\n  Columns: \n")
    print(x$columns)
  }
  invisible(x)
}