devtools::load_all()
Sys.setenv(RCPP_DEVEL_DEBUG = "1")

# Pop CSV
# Read the CSV, coerce columns and pass to create_cohort() to make pop List
demographics <- read.csv("tests/data/pop.csv")
demographics$AgeGrp <- as.integer(demographics$AgeGrp)
demographics$PopMale <- as.numeric(demographics$PopMale)
demographics$PopFemale <- as.numeric(demographics$PopFemale)
demographics$PopTotal <- as.numeric(demographics$PopTotal)
# List with fields male, age, death
initPop <- create_cohort(demographics, N=1e4)

# Life CSV
# Read the CSV and convert qx into a matrix [age, year]
rows_per_year <- 101
lifetable <- read.csv("tests/data/life_table.csv", header = TRUE)
life_qx <- as.numeric(lifetable$qx)
n_years <- length(life_qx) / rows_per_year
qx_mat <- matrix(life_qx, nrow = rows_per_year, ncol = n_years)

life_fn <- function(age, year) {
  # Convert to 1-indexed and clamp in bounds
  n_rows <- nrow(qx_mat)
  n_cols <- ncol(qx_mat)
  
  row_index <- pmin(pmax(age + 1, 1), n_rows)
  col_index <- pmin(pmax(year + 1, 1), n_cols)
  
  return(qx_mat[row_index, col_index])
}

# Mock parameters
parms <- list(
  hazards = list(list(fn = life_fn, parms=c("age", "~STEP"), freq = 1, after = -1, before = 1000)),
  steps = n_years,
  random_seed = 12L
)

outPop <- run_simulation(initPop, parms) # Currently returns empty list

library(data.table)
fwrite(outPop, "outPop.csv")