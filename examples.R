## reworking examples in neater format using package data
devtools::load_all()
library(data.table)
library(ggplot2)
set.seed(1234)

## ============ simple exponential decay
## generic transition
## Returns transitioned death_state based on current state and result of hazard
transition_fn <- function(state, i) {
  # If  result is true, and state is -1, update state to current time
  ifelse(state == -1, rep(i, length(state)), state)
}

## initial population
N <- 1e4L
pop0 <- data.table(
  id = 1:N,
  death = rep(-1, N),
  age = rep(40, N),
  sbp = rlnorm(N, log(140), .05),
  tc = rlnorm(N, log(4.5), .01)
)


## constant mortality hazard
deathrate0 <- function() {
  0.1
}

morthaz0 <- new_hazard(
  deathrate0,
  c(),
  new_transition(transition_fn, c("death", "~STEP"), "death")
)

## restrict to alive TODO better name
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
  hazards = morthaz0,
  steps = 10,
  debug = TRUE,
  history = noalive
)

## run the simulation
result0 <- run_simulation(pop0, parms0)

## plot
ggplot(result0$history, aes(`~STEP`, `no. alive`)) +
  geom_line() +
  geom_point() +
  geom_function(fun = function(x) N * exp(-x / 10), col = 2)


## ----
## extension of this to work with age &
## illustrate dependence on individual-level data

## Define a function to increment by 1
## If dead, age is not changed
age_traj <- function(age, death_time) {
  ifelse(death_time == -1, age + 1, age)
}

## mortality hazard depending on individual characteristics
deathrate1 <- function(death, sbp, tc) {
  ifelse(death < 0, 0.1 + sbp / 1000 + tc / 100, 0)
}

morthaz1 <- new_hazard(
  deathrate1,
  c("death", "sbp", "tc"),
  new_transition(transition_fn, c("death", "~STEP"), "death")
)


## a trajectory
age_traj <- new_trajectory(age_traj, c("age", "death"), "age")


## full parameters
parms1 <- new_parameters(
  hazards = morthaz1,
  trajectories = age_traj,
  steps = 10,
  debug = FALSE,
  history = noalive
)

## run the simulation
result1 <- run_simulation(pop0, parms1)


## ---
## extension of this to consider a stochastic, mulivariate trajectory
## also illustrating multiple trajectories

## keep everyone alive here to have a look at RW dynamics
deathrate2 <- function() {
  0
}
morthaz2 <- new_hazard(deathrate2, args = c(), transitions = list())


## multivariate trajectory example
## sbp & tc as correlated geometric random walk
bptc_update <- function(sbp, tc, death_time) {
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

bptc_update(rep(140, 5), rep(4.5, 5), rep(-1, 5))
bptc_update(rep(140, 5), rep(4.5, 5), rep(1, 5))


bptc_traj <- new_trajectory(
  bptc_update, # trajectory fn
  c("sbp", "tc", "death"), # trajectory args
  c("sbp", "tc") # outputs (list of vectors from fn)
)

## make trajectories
trajlist2 <- list(
  ## age
  age_traj,
  ## SBP & TC handled in bivariate fashion
  bptc_traj
)

## filter to 1
filter_1 <- function(x) {
  x == 1
}

## history to also include random walk variables for id==1
## NOTE mean not doing anything here
noalive2 <- new_history(
  columns = list(
    new_column("no. alive", length, c("age"), filter_fn, c("death")),
    ## 3 ways of returning an individual's value:
    new_column("sbp1", sum, c("sbp"), filter_1, c("id")),
    new_column("tc1", mean, c("tc"), filter_1, c("id")),
    new_column("tc1 v2", function(x) x, c("tc"), filter_1, c("id"))
  ),
  frequency = 1
)


## full parameters
parms2 <- new_parameters(
  hazards = morthaz2,
  trajectories = trajlist2,
  steps = 1000, #longer
  debug = TRUE,
  history = noalive2
)

## run the simulation
result2 <- run_simulation(pop0[1:50], parms2) #look at a limited population
plot_data <- melt(result2$history[, .(t = `~STEP`, sbp1, tc1)], id = "t")


ggplot(plot_data, aes(t, value, col = variable)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  theme(legend.position = "none")

## ---
## example 3 building on above to include:
## multiple transitions for single hazard
## timed event via Inf hazard

## extra variables
pop0[, on_treatment := 0L]
pop0[, treatment_counter := 0L]


## trigger for treatment start:
## definitely go on treatment if sbp >= 150 or tc >= 4.5
treatment_start_haz <- function(death, on_treatment, sbp, tc) {
  ifelse(death > 0 | on_treatment > 0 | (sbp < 150 & tc < 4.5), 0, Inf)
}

## trigger treatment end:
## definitely after 5 steps if alive
treatment_end_haz <- function(death, on_treatment) {
  ifelse(death > 0 | on_treatment < 5, 0, Inf)
}


## treatment trajectory
treatment_time_update <- function(death, on_treatment) {
  ifelse(death < 0 & on_treatment > 0, on_treatment + 1, 0)
}

## treatment transitions:

## start off
treatment_start_transition <- function(on_treatment) {
  rep(1, length(on_treatment))
}

## update counter
## (NOTE this could be used in a few places, maybe ageing should not ints?)
update_counter <- function(x) {
  x + 1
}

## finish off
treatment_end_transition <- function(on_treatment) {
  rep(0, length(on_treatment))
}


## NOTE do we want to allow transitions to be formulated in this way also?
## ie to have multiple return columns?

## build parameters
trajlist3 <- list(
  ## age
  age_traj,
  ## SBP & TC handled in bivariate fashion
  bptc_traj,
  ## new trajectory for treatment
  new_trajectory(
    treatment_time_update,
    c("death", "on_treatment"),
    "on_treatment"
  )
)

hazlist3 <- list(
  ## death
  morthaz0,
  ## starting treatment
  new_hazard(
    treatment_start_haz,
    c("death", "on_treatment", "sbp", "tc"),
    ## (which triggers two transitions)
    list(
      new_transition(
        treatment_start_transition,
        "on_treatment",
        "on_treatment"
      ),
      new_transition(
        update_counter,
        "treatment_counter",
        "treatment_counter"
      )
    )
  ),
  ## finishing treatment
  new_hazard(
    treatment_end_haz,
    c("death", "on_treatment"),
    ## (triggering one transition)
    new_transition(
      treatment_end_transition,
      "on_treatment",
      "on_treatment"
    )
  )
)


## history
history3 <- new_history(
  columns = list(
    new_column("no. alive", length, "age", filter_fn, "death"),
    new_column(
      "no. alive on tx",
      function(x) sum(x > 0),
      "on_treatment",
      filter_fn, "death"
    ),
    ## an individual's value:
    new_column("sbp1", sum, "sbp", filter_1, "id"),
    new_column("tc1", mean, "tc", filter_1, "id")
  ),
  frequency = 1
)

## full parameters
parms3 <- new_parameters(
  hazards = hazlist3,
  trajectories = trajlist3,
  steps = 20, # longer
  debug = TRUE,
  history = history3
)

## run the simulation
result3 <- run_simulation(pop0, parms3) # look at a limited population

plot_data <- melt(result3$history[, .(
  t = `~STEP`,
  `no. alive`, `no. alive on tx`
)], id = "t")


ggplot(plot_data, aes(t, value, col = variable)) +
  geom_line() +
  geom_point() +
  theme(legend.position = "top")


result3$pop



## ========== less trivial example


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
## mortality hazard
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
## initial population
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
  age >= 25 & age < 30, "25-29",
  age >= 30 & age < 35, "30-34",
  age >= 35 & age < 40, "35-39",
  age >= 40 & age < 45, "40-44",
  age >= 45 & age < 50, "45-49",
  age >= 50 & age < 55, "50-54",
  age >= 55 & age < 60, "55-59",
  age >= 60 & age < 65, "60-64",
  age >= 65 & age < 70, "65-69",
  age >= 70 & age < 75, "70-74",
  age >= 75 & age < 80, "75-79",
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


## hazards
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


## trajectories
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

## history
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

## FOR VIGNETTE
## NOTE if you rewrite functions, this is not enough: need to remake objects
## should be noted in guide
## NOTE history: single item not lists

## OTHER EXAMPLES
## TODO data documentation not working **
