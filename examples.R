## reworking examples in neater format using package data
devtools::load_all()
## Sys.setenv(RCPP_DEVEL_DEBUG = "1")
## options(error = recover)


library(data.table)
library(ggplot2)



## Define a function to increment by 1
## If dead, age is not changed
age_traj <- function(age, death_time) {
  ifelse(death_time == -1, age + 1, age)
}

##################
# BMI TRAJECTORY #
##################
## TODO better data? this is fake
# load data.frame from LCTMtools
# id/age/bmi/true_class
data(bmi_long, package = "LCTMtools")
mm <- lm(bmi ~ 1 + age + I(age^2),
  data = bmi_long
) # regression


## Define a function to calculate bmi from age
## illustration of using predict from a regression analysis
bmi_traj <- function(age) {
  predict(mm, newdata = data.table(age = age))
}

# bmi_traj(25) # test



## TODO example of bivariate random walk for randomness & multivariateness

##############
# CVD HAZARD #
##############


## version only focussed on relevant output
globohaz <- function(sex, age, bmi) {
  n <- length(sex)
  if (length(sex) != length(age) || length(sex) != length(bmi)) {
    stop("Arguments must be equal length!\n")
  }
  ## checks/additions
  age <- pmin(pmax(age, 41), 75)
  sbp <- rep(140, n)
  tc <- rep(4.5, n)
  dm <- rep(0, n)
  smk <- rep(0, n)
  ## globorisk transforms
  agec <- as.integer(ifelse(age < 85, trunc(age / 5) - 7, 10))
  sbp <- sbp / 10
  dm <- as.integer(dm)
  smk <- as.integer(smk)
  bmi <- bmi / 5
  ## center
  agc_sex <- paste(agec, sex, sep = "_")
  sbp_c <- sbp - globorisk_rf[agc_sex][, mean_sbp]
  tc_c <- tc - globorisk_rf[agc_sex][, mean_tc]
  dm_c <- dm - globorisk_rf[agc_sex][, mean_dm]
  smk_c <- smk - globorisk_rf[agc_sex][, mean_smk]
  bmi_c <- bmi - globorisk_rf[agc_sex][, mean_bmi]
  ## compute hazard ratios
  HR <- sbp_c * globorisk_coefs[["main_sbpc"]] +
    bmi_c * globorisk_coefs[["main_bmi5c"]] +
    smk_c * globorisk_coefs[["main_smokc"]] +
    sex * smk_c * globorisk_coefs[["main_sexsmokc"]] +
    age * sbp_c * globorisk_coefs[["tvc_sbpc"]] +
    age * smk_c * globorisk_coefs[["tvc_smokc"]] +
    age * bmi_c * globorisk_coefs[["tvc_bmi5c"]]
  HR <- exp(HR) # un-log
  ## baseline hazard
  h <- globorisk_cvdr[agc_sex][, cvd_0]
  ## return
  ans <- h * HR
  ans[!is.finite(ans)] <- 0 # safety TODO CHECK
  ans
}

globohaz(c(0, 1, 1), c(00, 50, 90), rep(25, 3))

########################
# GENERAL DEATH HAZARD #
########################


## ## Read the CSV and convert qx into a matrix [age, year]
## rows_per_year <- 101
## lifetable <- fread("tests/data/life_table.csv") #TODO delete
## life_qx <- as.numeric(lifetable$qx)
## n_years <- length(life_qx) / rows_per_year
## qx_mat <- matrix(life_qx, nrow = rows_per_year, ncol = n_years)
## # Calculates the general death hazard chance for a given age/year
## life_fn <- function(age, year) {
##   # Convert to 1-indexed and clamp in bounds
##   n_rows <- nrow(qx_mat)
##   n_cols <- ncol(qx_mat)
##   row_index <- pmin(pmax(age + 1, 1), n_rows)
##   col_index <- pmin(pmax(year + 1, 1), n_cols)
##   qx_mat[cbind(row_index, col_index)]
## }
## life_fn(50, 70)
## life_fn(rep(50, 10), rep(70, 10))



lifetable_data[year == 2000]



n_ages <- 101
n_years <- nrow(lifetable_data) / n_ages
qx_array <- array(0, dim = c(2, n_ages, n_years))
qx_array[1, , ] <- lifetable_data$mxM
qx_array[2, , ] <- lifetable_data$mxF
mort_fn <- function(sex, age, year) {
  ## Convert to 1-indexed and clamp in bounds
  d <- dim(qx_array)
  n_rows <- d[2]
  n_cols <- d[3]
  row_index <- pmin(pmax(age + 1, 1), n_rows)
  col_index <- pmin(pmax(year + 1, 1), n_cols)
  qx_array[cbind(sex + 1, row_index, col_index)] # zero index female
}



mort_fn(rep(1, 10), rep(50, 10), rep(70, 10))






######################
# GENERIC TRANSITION #
######################

# Returns transitioned death_state based on current state and result of hazard
transition_fn <- function(state, i) {
  # If  result is true, and state is -1, update state to current time
  ifelse(state == -1, rep(i, length(state)), state)
}



######################
# INITIAL POPULATION #
######################
pop_snapshot

make_cohort <- function(N) {
  popcounts <- rmultinom(1, size = N, prob = c(pop_snapshot))
  sexes <- rep(1, sum(popcounts[1:nrow(pop_snapshot)]))
  sexes <- c(sexes, rep(0, N - length(sexes)))
  initPop <- data.table(
    male = as.integer(sexes),
    age = as.integer(0),
    death = as.integer(rep(-1, N))
  )
  ## do ages
  ageref <- rep(0:(nrow(pop_snapshot) - 1), 2)
  k <- 1
  for (i in 1:length(popcounts)) {
    initPop[k:(popcounts[i] + k - 1), age := ageref[i]]
    k <- k + popcounts[i]
  }
  initPop
}


initPop <- make_cohort(1e4)
initPop$bmi <- bmi_traj(initPop$age)

## initial pop
ggplot(initPop, aes(x = age, fill = factor(male), group = male)) +
  geom_histogram()



#############
## HAZARDS ##
#############

hazlist <- list(
  new_hazard(
    globohaz,
    c("male", "age", "bmi"),
    list(new_transition(transition_fn, c("death", "~STEP"), "death"))
  ),
  new_hazard(
    mort_fn,
    c("male", "age", "~STEP"),
    list(new_transition(transition_fn, c("death", "~STEP"), "death"))
  )
)

## hazlist <- list(
##   new_hazard(
##     globohaz,
##     c("male", "age", "bmi"),
##     list(new_transition(transition_fn, c("death", "~STEP"), "death"))
##   ),
##   new_hazard(
##     life_fn,
##     c("age", "~STEP"),
##     list(new_transition(transition_fn, c("death", "~STEP"), "death"))
##   )
## )






##################
## TRAJECTORIES ##
##################

trajlist <- list(
  new_trajectory(age_traj, c("age", "death"), "age"),
  new_trajectory(bmi_traj, c("age"), "bmi")
)

###########
# HISTORY #
###########

filter_fn <- function(x) {
  x == -1 # tests alive applied to 'death'
}


history <- new_history(
  columns = list(
    new_column("no. alive", length, c("age"), filter_fn, c("death")),
    new_column("av. age alive", mean, c("age"), filter_fn, c("death"))
  ),
  frequency = 1
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

## run the simulation
ret <- run_simulation(initPop, parms)


## now dead
ggplot(
  ret$pop[death > 0],
  aes(x = age, fill = factor(male), group = male)
) +
  geom_histogram()


## history: number alive
ggplot(ret$history, aes(`~STEP`, `no. alive`)) +
  geom_line()

## SUGGESTIONS/QUERIES
## TODO functions with no args?


## FOR VIGNETTE
## NOTE if you rewrite functions, this is not enough: need to remake objects
## should be noted in guide

## OTHER EXAMPLES
## TODO example of timed event
## TODO multivariate & random (see above)

## TODO
## simple exponential example as a test

## TODO data documentation not working
