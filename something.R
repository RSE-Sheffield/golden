devtools::load_all()
#Sys.setenv(RCPP_DEVEL_DEBUG = "1")
#options(error = recover)
library(LCTMtools)
library(CVrisk)
library(lme4)
library(data.table)
library(ggplot2)

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

###########
# HISTORY #
###########

reduce_fn <- function(x) {
  return (sum(x))
}
count_fn <- function(x) {
  return (length(x))
}
filter_fn <- function(x) {
  return (x == -1)
}

history <- new_history(
  columns = list(
    new_column("sum age", reduce_fn, c("age")),
    new_column("sum age alive", reduce_fn, c("age"), filter_fn, c("death")),
    new_column("no. alive", count_fn, c("age"), filter_fn, c("death")),
    new_column("av. age alive", mean, c("age"), filter_fn, c("death"))
  ),
  frequency = 1
)


###########
## HAZARDS
###########

hazlist <- list(
  new_hazard(CVD_haz,
             c("age", "bmi"),
             list(new_transition(transition_fn, c("death", "~STEP"), "death"))
             ),
  new_hazard(life_fn,
             c("age", "~STEP"),
             list(new_transition(transition_fn, c("death", "~STEP"), "death"))
             )
)


###############
## TRAJACTORIES
###############

trajlist <- list(new_trajectory(age_traj, c("age", "death"), "age"),
                 new_trajectory(bmi_traj, c("age"), "bmi")
                 )


##############
# SIMULATION #
##############

parms <- new_parameters(
  hazards = hazlist,
  trajectories = trajlist,
  steps = n_years,
  debug = TRUE,
  history = history
)


ret <- run_simulation(initPop, parms)

fwrite(ret$pop, "outPop.csv")
fwrite(ret$history, "outHistory.csv")

## initial pop
ggplot(initPop,aes(x=age,fill=factor(male),group=male))+
  geom_histogram()

## still alive
ggplot(ret$pop[death==-1],
       aes(x=age,fill=factor(male),group=male))+
  geom_histogram()

## now dead
ggplot(ret$pop[death>0],
       aes(x=age,fill=factor(male),group=male))+
  geom_histogram()

## TODO this generates a warning,
## and we may want inclusion of step done by sim
## perhaps with ~STEP name?
ret$history[,step:=1:nrow(ret$history)]

## history: number alive
ggplot(ret$history, aes(step,`no. alive`))+
  geom_line()

## average age:
ggplot(ret$history, aes(step,`av. age alive`))+
  geom_line()
