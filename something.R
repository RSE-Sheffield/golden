devtools::load_all()
Sys.setenv(RCPP_DEVEL_DEBUG = "1")
options(error = recover)
library(LCTMtools)
library(CVrisk)
library(lme4)
library(data.table)

##################
# AGE TRAJECTORY #
##################

## Define a function to increment by 1
## If dead, age is not changed
age_traj <- function(age, death_time) {
  return (ifelse(death_time == -1, age + 1, age))
}

##################
# BMI TRAJECTORY #
##################

# load data.frame from LCTMtools
# id/age/bmi/true_class
data(bmi_long, package = "LCTMtools")

## create a mixed model fit to bmi_long data
mm <- lmer(bmi ~ 1 + age + I(age^2) + (1 | id),
  data = bmi_long
)

## extract fixed effects for simple trajectory model
## This is a tiny 3 column 1 row data table
(B <- coef(summary(mm))[, 1])

## Define a function to calculate bmi from age using the quadratic model
bmi_traj <- function(age) {
  unname(B[1] + age * B[2] + age^2 * B[3])
}

#bmi_traj(25) # test

##############
# CVD HAZARD #
##############

# Redundant, this returns a data table we aren't using?
compute_CVrisk(sample_data,
  age = "age", race = "race", gender = "gender", bmi = "BMI", sbp = "sbp",
  hdl = "hdl", totchol = "totchol", bp_med = "bp_med", smoker = "smoker",
  diabetes = "diabetes", lipid_med = "lipid_med",
  fh_heartattack = "fh_heartattack", cac = "cac"
)

## Define a function to calculate risk of death via CVD from age and bmi
CVD_haz <- function(age, bmi) {
  # Clamp age in bounds of model to avoid NA
  age <- pmin(pmax(age, 30), 74)
  risk10 <- ascvd_10y_frs_simple(
    gender = "male", age = age,
    bmi = bmi, sbp = 140,
    bp_med = 0, smoker = 0, diabetes = 0
  )
  1 - (1 - risk10 / 100)^0.1 # horrible approximation
}

########################
# GENERAL DEATH HAZARD #
########################

# Read the CSV and convert qx into a matrix [age, year]
rows_per_year <- 101
lifetable <- read.csv("tests/data/life_table.csv", header = TRUE)
life_qx <- as.numeric(lifetable$qx)
n_years <- length(life_qx) / rows_per_year
qx_mat <- matrix(life_qx, nrow = rows_per_year, ncol = n_years)

# Calculates the general death hazard chance for a given age/year
life_fn <- function(age, year) {
  # Convert to 1-indexed and clamp in bounds
  n_rows <- nrow(qx_mat)
  n_cols <- ncol(qx_mat)
  
  row_index <- pmin(pmax(age + 1, 1), n_rows)
  col_index <- pmin(pmax(year + 1, 1), n_cols)
  
  return(qx_mat[row_index, col_index])
}

######################
# GENERIC TRANSITION #
######################

# Returns transitioned death_state based on current state and result of hazard
transition_fn <- function(state, i) {
    # If  result is true, and state is -1, update state to current time
    return (ifelse(state == -1, rep(i, length(state)), state))
}

######################
# INITIAL POPULATION #
######################

# Pop CSV
# Read the CSV, coerce columns and pass to create_cohort() to make pop List
demographics <- read.csv("tests/data/pop.csv")
demographics$AgeGrp <- as.integer(demographics$AgeGrp)
demographics$PopMale <- as.numeric(demographics$PopMale)
demographics$PopFemale <- as.numeric(demographics$PopFemale)
demographics$PopTotal <- as.numeric(demographics$PopTotal)
# List with fields male, age, bmi, death
initPop <- create_cohort(demographics, N=1e4)
# Init bmi (hazards run before trajectories)
initPop$bmi = bmi_traj(initPop$age)

##############
# SIMULATION #
##############

parms <- list(
  hazards = list(list(fn = CVD_haz,
                      parms=c("age", "bmi"),
                      transition_fn=transition_fn,
                      transition_state="death",
                      transition_parms=c("death", "~STEP")),
                 list(fn = life_fn,
                      parms=c("age", "~STEP"),
                      transition_fn=transition_fn,
                      transition_state="death",
                      transition_parms=c("death", "~STEP"),
                      freq = 1, after = -1, before = 1000)), # Left in as default values to show they exist
  trajectories = list(list(fn = age_traj,
                           property = "age",
                           parms=c("age", "death")),
                      list(fn = bmi_traj,
                           property = "bmi",
                           parms=c("age"))),
  steps = n_years,
  random_seed = 12L, # Not currently seeding R rng internally
  debug = TRUE
)

outPop <- run_simulation(initPop, parms)

fwrite(outPop, "outPop.csv")