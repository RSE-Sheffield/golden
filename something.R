devtools::load_all()
#Sys.setenv(RCPP_DEVEL_DEBUG = "1")
#options(error = recover)
library(data.table)
library(ggplot2)
## https://github.com/boyercb/globorisk
library(globorisk)
## https://github.com/PPgp/wpp2024
library(wpp2024)

##################
# AGE TRAJECTORY #
##################

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


## adatped from the globorisk R package
## slow: could defo do better with same package data
globohaz <- function(sex, age, bmi, full_out = FALSE) {
  n <- length(sex)
  if (length(sex) != length(age) || length(sex) != length(bmi)) {
    stop("Arguments must be equal length!\n")
  }
  age_arg <- pmin(pmax(age, 41), 75)
  ans <- globorisk(
    sex = sex,
    age = age_arg,
    sbp = rep(140, n),
    tc = rep(4.5, n),
    dm = rep(0, n),
    smk = rep(0, n),
    bmi = bmi,
    iso = rep("USA", n),
    year = rep(2000, n),
    version = "office",
    type = "all"
  )
  if (full_out) {
    return(ans)
  } else {
    return(ans$hzcvd_0)
  }
}

## tests
globohaz(c(0, 1, 1), c(00, 50, 90), rep(25, 3))

## convert these to restricted/faster look-up tables
mycoefs <- as.data.table(globorisk:::coefs)
mycoefs <- mycoefs[type == "office" & lac == 0]

myrf <- as.data.table(globorisk:::rf)
myrf <- myrf[iso == "USA"]
myrf[, agesex := paste(agec, sex, sep = "_")]
setkey(myrf, agesex)
myrf[c("1_1", "1_0")]


mycvdr <- as.data.table(globorisk:::cvdr)
mycvdr <- mycvdr[
  iso == "USA" & type == "FNF" & year == 2000,
  .(agec, sex, cvd_0)
]
mycvdr <- unique(mycvdr)
mycvdr[, agesex := paste(agec, sex, sep = "_")]
setkey(mycvdr, agesex)
mycvdr[c("1_1", "1_0")]


globohaz2 <- function(sex, age, bmi) {
  n <- length(sex)
  if (length(sex) != length(age) || length(sex) != length(bmi)) {
    stop("Arguments must be equal length!\n")
  }
  ## my checks/additions
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
  sbp_c <- sbp - myrf[agc_sex][, mean_sbp]
  tc_c <- tc - myrf[agc_sex][, mean_tc]
  dm_c <- dm - myrf[agc_sex][, mean_dm]
  smk_c <- smk - myrf[agc_sex][, mean_smk]
  bmi_c <- bmi - myrf[agc_sex][, mean_bmi]
  ## compute hazard ratios
  HR <- sbp_c * mycoefs[["main_sbpc"]] +
    bmi_c * mycoefs[["main_bmi5c"]] +
    smk_c * mycoefs[["main_smokc"]] +
    sex * smk_c * mycoefs[["main_sexsmokc"]] +
    age * sbp_c * mycoefs[["tvc_sbpc"]] +
    age * smk_c * mycoefs[["tvc_smokc"]] +
    age * bmi_c * mycoefs[["tvc_bmi5c"]]
  HR <- exp(HR) # un-log
  ## baseline hazard
  h <- mycvdr[agc_sex][, cvd_0]
  ## return
  h * HR
}

globohaz(c(0, 1, 1), c(00, 50, 90), rep(25, 3))
globohaz2(c(0, 1, 1), c(00, 50, 90), rep(25, 3))


########################
# GENERAL DEATH HAZARD #
########################

## TODO use a package to do a better job

# Read the CSV and convert qx into a matrix [age, year]
rows_per_year <- 101
## lifetable <- fread("tests/data/life_table.csv") #TODO delete

lifetable <- mx1dt[
  name == "United States of America" &
    year >= 2024,
  .(year, age, qx = mxB)
]



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
  qx_mat[row_index, col_index]
}



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
data(popAge1dt)

make_cohort <- function(N, country_name) {
  M <- as.matrix(popAge1dt[
    name == country_name &
      year == 2000 &
      age < 80,
    .(popM, popF)
  ])
  popcounts <- rmultinom(1, size = N, prob = c(M))
  sexes <- rep(1, sum(popcounts[1:nrow(M)]))
  sexes <- c(sexes, rep(0, N - length(sexes)))
  initPop <- data.table(
    male = as.integer(sexes),
    age = as.integer(0),
    death = as.integer(rep(-1, N))
  )
  ## do ages
  ageref <- rep(0:79, 2)
  k <- 1
  for (i in 1:length(popcounts)) {
    initPop[k:(popcounts[i] + k - 1), age := ageref[i]]
    k <- k + popcounts[i]
  }
  initPop
}

initPop <- make_cohort(1e4, "United States of America")
## TODO sample using population data above
# Init bmi (hazards run before trajectories)
initPop$bmi <- bmi_traj(initPop$age)

## initial pop
ggplot(initPop, aes(x = age, fill = factor(male), group = male)) +
  geom_histogram()



#############
## HAZARDS ##
#############

hazlist <- list(
  new_hazard(
    globohaz2,
    c("male", "age", "bmi"),
    list(new_transition(transition_fn, c("death", "~STEP"), "death"))
  ),
  new_hazard(
    life_fn,
    c("age", "~STEP"),
    list(new_transition(transition_fn, c("death", "~STEP"), "death"))
  )
)




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
