## reworking examples in neater format using package data
devtools::load_all()
## Sys.setenv(RCPP_DEVEL_DEBUG = "1")
## options(error = recover)


library(data.table)
library(ggplot2)

## ============ simple exponential decay
## generic transition
## Returns transitioned death_state based on current state and result of hazard
transition_fn <- function(state, i) {
  # If  result is true, and state is -1, update state to current time
  ifelse(state == -1, rep(i, length(state)), state)
}

## initial population
pop0 <- data.table(death = -1)

## constant mortality hazard
deathrate <- function() {
  0.1
}

morthaz <- new_hazard(
  deathrate,
  c(), #BUG currently
  new_transition(transition_fn, c("death", "~STEP"), "death")
)

## restrict to alive
filter_fn <- function(x) {
  x == -1 # tests alive applied to 'death'
}


noalive <- new_history(
  columns = list(
    new_column("no. alive", length, c("age"), filter_fn, c("death"))
  ),
  frequency = 1
)

parms0 <- new_parameters(
  hazards = morthaz,
  ## trajectories = trajlist,#TODO how will this be handled?
  ## empty list? with default? (same for hazards)
  steps = 10,
  debug = TRUE,
  history = noalive
)

## run the simulation
ret0 <- run_simulation(pop0, parms0)



## TODO finish
## TODO version of this with decay parameters assigned to individuals (PSA eg)
## TODO version with multiple transitions?


## ========== less trivial example



## Define a function to increment by 1
## If dead, age is not changed
age_traj <- function(age, death_time) {
  ifelse(death_time == -1, age + 1, age)
}



## multivariate trajectory example
## sbp & tc as correlated geometric random walk
bptc_traj <- function(sbp, tc, death_time) {
  lsbp <- log(sbp)
  ltc <- log(tc)
  alive <- death_time < 0
  if (sum(alive) > 1) {
    U <- matrix(rnorm(2 * sum(alive)), nrow = sum(alive), ncol = 2)
    U[, 2] <- U[, 2] + U[, 1] / 2 # correlation
    U <- U / 100 # rescale
    lsbp[alive] <- lsbp[alive] + U[, 1]
    ltc[alive] <- ltc[alive] + U[, 2]
  }
  list(sbp = exp(lsbp), tc = exp(ltc))
}

bptc_traj(rep(140, 5), rep(4.5, 5), rep(-1, 5))
bptc_traj(rep(140, 5), rep(4.5, 5), rep(1, 5))



##############
# CVD HAZARD #
##############


## version only focussed on relevant output
globohaz <- function(death, sex, age, bmi, sbp = 140, tc = 4.5) {
  n <- length(sex)
  if (length(sex) != length(age) || length(sex) != length(bmi)) {
    stop("Arguments must be equal length!\n")
  }
  ## checks/additions
  age <- pmin(pmax(age, 41), 75)
  dm <- rep(0, n)
  smk <- rep(0, n)
  ## globorisk transforms
  agec <- as.integer(ifelse(age < 85, trunc(age / 5) - 7, 10))
  ## TODO CHECK 10 not actualy in risks?
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
  ## inefficient? TODO
  ans[death > 0] <- 0 # no effect on dead
  ans[!is.finite(ans)] <- 0 # safety TODO CHECK
  ans
}



globohaz(
  c(-1, -1, 1), #alive
  c(0, 1, 1),   #sex
  c(00, 50, 90), #age
  rep(25, 3),    #BMI
  rep(145, 3),   #SBP
  rep(5, 3)      #TC
)


cvd_update <- function(cvd_count) {
  cvd_count + 1
}


########################
# GENERAL DEATH HAZARD #
########################
lifetable_data



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
# INITIAL POPULATION #
######################
## bmi fit data
bmi_fits
bmi_fits[, male := ifelse(sex == "Men", 1, 0)]



pop_snapshot

make_cohort <- function(N) {
  popcounts <- rmultinom(1, size = N, prob = c(pop_snapshot))
  sexes <- rep(1, sum(popcounts[1:nrow(pop_snapshot)]))
  sexes <- c(sexes, rep(0, N - length(sexes)))
  initPop <- data.table(
    male = as.integer(sexes),
    age = as.integer(0),
    death = as.integer(rep(-1, N)),
    cvd_count = as.integer(rep(0, N))
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
initPop[, acat := fcase(
  age >= 25 & age < 29, "25-29",
  age >= 30 & age < 35, "30-34",
  age >= 35 & age < 39, "35-39",
  age >= 40 & age < 45, "40-44",
  age >= 45 & age < 49, "45-49",
  age >= 50 & age < 55, "50-54",
  age >= 55 & age < 59, "55-59",
  age >= 60 & age < 65, "60-64",
  age >= 65 & age < 69, "65-69",
  age >= 70 & age < 75, "70-74",
  age >= 75 & age < 79, "75-79",
  age >= 80 & age < 85, "80-84",
  age >= 85, "85plus",
  default = "20-24"
)]


initPop <- merge(
  initPop,
  bmi_fits[, .(male, acat, k, theta)],
  by = c("male", "acat"),
  all.x = TRUE, all.y = FALSE
)

## sample some BMIs
initPop[, bmi := rgamma(nrow(initPop), shape = k, scale = theta)]

## pretend that cross-sectional BMIs are a reasonable way to inform trajectories
mm <- lm(bmi ~ 1 + age + I(age^2),
  data = initPop
) # regression

bmi_traj <- function(age) {
  predict(mm, newdata = data.table(age = age))
}

bmi_traj(25) # test

## initial pop
ggplot(initPop, aes(x = age, fill = factor(male), group = male)) +
  geom_histogram()


## merge in SBP and TC
initPop[, agec := as.integer(ifelse(age < 85, trunc(age / 5) - 7, 10))]
initPop[agec < 1, agec := 1]
initPop[agec > 9, agec := 9]

initPop <- merge(
  initPop,
  globorisk_rf[, .(agec, male = sex, sbp = 10 * mean_sbp, tc = mean_tc)],
  by = c("agec", "male")
)



#############
## HAZARDS ##
#############

hazlist <- list(
  ## CVD event
  new_hazard(
    globohaz,
    c("death", "male", "age", "bmi", "sbp", "tc"), # input args for hazard
    list(new_transition(
      cvd_update,  # transition function for event
      "cvd_count", # input args for transition
      "cvd_count"  # vars affected by transition
    ))
  ),
  ## deaths
  new_hazard(
    mort_fn,
    c("male", "age", "~STEP"),
    list(new_transition(transition_fn, c("death", "~STEP"), "death"))
  )
)



##################
## TRAJECTORIES ##
##################

trajlist <- list(
  ## age
  new_trajectory(age_traj, c("age", "death"), "age"),
  ## BMI
  new_trajectory(bmi_traj, c("age"), "bmi"),
  ## SBP & TC handled in bivariate fashion
  new_trajectory(
    bptc_traj,                    #trajectory fn
    c("sbp", "tc", "death"), # trajectory args
    c("sbp", "tc") # outputs (list of vectors from fn)
  )
)




###########
# HISTORY #
###########


history <- new_history(
  columns = list(
    new_column("no. alive", length, c("age"), filter_fn, c("death")),
    new_column("av. age alive", mean, c("age"), filter_fn, c("death")),
    new_column("av. cvd events", mean, c("cvd_count"), filter_fn, c("death"))
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

## history: number alive
ggplot(ret$history, aes(`~STEP`, `av. cvd events`)) +
  geom_line()

## SUGGESTIONS/QUERIES
## TODO functions with no args? **
## Does parms take a list of histories? If not slightly different behaviour

## FOR VIGNETTE
## NOTE if you rewrite functions, this is not enough: need to remake objects
## should be noted in guide

## OTHER EXAMPLES
## TODO example of timed event: how was this to work again? **
## TODO data documentation not working **
