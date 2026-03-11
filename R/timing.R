#' Print the contents of a golden_timing type S3 object
#'
#' @param x The object to be printed
#' @param ... Not used. Included for S3 method compatibility.
#' @return No return value, called for side effects.
#'
#' @examples
#' library(data.table)
#' N <- 100
#' dt <- data.table(a = runif(N, 0, 1), b = rep(0, N))
#' # Define a hazard function, which returns a vector of equal length uncertainties
#' test_hazard <- function(a) {
#'     ret <- (a < 0.5)
#' }
#' # Define a transition function, which sets all "b" columns affected by the hazard to 100
#' test_transition <- function() {
#'     return (100)
#' }
#' # Create an S3 golden_hazard
#' haz <- new_hazard(
#'               test_hazard,
#'               c("a"),
#'               new_transition(test_transition, c(), "b")
#'             )
#' # Define a trajectory function, which adds 2 to all members of the input vector
#' test_trajectory <- function(a) {
#'     return (a + 2)
#' }
#' # Define an S3 golden_trajectory
#' trj <- new_trajectory(test_trajectory, c("b"), "b")
#' # Create an S3 golden_history, containing 1 golden_history_column
#' hist <- new_history(new_column("sum_a", sum, c("a")))
#' # Define an S3 golden_parameters
#' params <- new_parameters(
#'   hazards = haz,
#'   trajectories = trj,
#'   steps = 10,
#'   debug = FALSE,
#'   history = hist
#' )
#' # Run the simulation to collect results
#' results <- run_simulation(dt, params)
#' print(results$timing)
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