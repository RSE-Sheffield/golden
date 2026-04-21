# golden <img src="man/figures/logo.png" align="right" height="138" alt="" /> #


**golden** is a flexible patient-level microsimulation modelling framework focussed on trajectories of patient risk factors and hazards of events, authored by [Pete Dodd](https://sheffield.ac.uk/smph/people/academic/population-health/pete-dodd) and [Robert Chisholm](https://sheffield.ac.uk/cs/people/research-staff/robert-chisholm).

## Installation

**golden** can be installed from GitHub using `devtools` via:

```r
devtools::install_github("RSE-Sheffield/golden")
```

or by manually downloading a `.zip` from the [golden GitHub repository](https://github.com/RSE-Sheffield/golden) to be installed via:

```r
install.packages("<path to .zip>", repos = NULL, type="source")
```

Following either of the actions, you should now be able to load golden like any other package.

```r
library(golden)
```


Please see the vignette: `vignette("getting-started", package="golden")` for more details. (Note: you will need to set `build_vignettes = TRUE` in `devtools::install_github` to build the vignette.)

A quickstart introduction and example is provided below.

## Example use

**golden** focuses on *trajectories*, *hazards*, *transitions* and a *history*
In general user-functions should take vector arguments and return (lists of) vectors.

Let's load it & see how it works:

```r
library(golden)
```

### Make a population

This is not handled by the package. Here's a simple example population - one row per individual:

```r
N <- 1e4L                         # population size
pop0 <- data.frame(
  id = 1:N,                       # patient ids
  death = rep(-1, N),             # time of death
  age = rep(40, N),               # age
  sbp = rlnorm(N, log(140), .05), # systolic BP
  tc = rlnorm(N, log(4.5), .01)   # total cholesterol
)
```

### IMPORTANT NOTE: use vectorized functions!

Functions defining trajectories, hazards, and transitions are expected to operate on vectors.
This means, e.g., that standard R 'if-else' control flow may need to be adapted.
In the examples below, we have tended to use `ifelse(test, x, y)` for clarity.
Often, patterns like

```r
ans <- x
ans[test] <- y
```
may be faster than `ifelse` patterns.
It may also be possible to wrap existing code that works on scalars, e.g. using `Vectorize`.

### Define trajectories

Ageing can serve as a simple example of a trajectory. We define an update rule and create a trajectory object that knows how to apply the update:
```r
## update function
age_update <- function(age, death_time) {
  ifelse(death_time == -1, age + 1, age)
}

## new_trajectory creates a trajectory
age_traj <- new_trajectory(
  fn = age_update,          # update function
  args = c("age", "death"), # args for update function
  states = "age"            # column(s) to be updated
)
```

### Define hazards

[Hazards](https://en.wikipedia.org/wiki/Survival_analysis#Hazard_function_and_cumulative_hazard_function) are the per-unit-time generalization of probabilities and are used to define event rates.

Here's a hazard function for death:
```r
## hazard function
deathrate <- function(death, sbp, tc) {
  ifelse(death < 0, 0.1 + sbp / 1000 + tc / 100, 0)
}
```

Events are enacted by application of (multiple) transition functions to the population. Here's a generic transition function:
```r
## generic transition: returns state as input value
transition_fn <- function(state, i) {
  # If  result is true, and state is -1, update state to current time
  ifelse(state == -1, rep(i, length(state)), state)
}
```
By applying `transition_fn(death, time)` to our population we will record their time of death.

As with trajectories, we create a hazard object by providing context for how to calculate the hazard on the population and what transitions are associated:
```r
## new_hazard creates a hazard
morthaz <- new_hazard(
  fn = deathrate,                 # hazard function
  args = c("death", "sbp", "tc"), # args for hazard fn
  ## (list of) transitions to apply
  transitions = new_transition(
    fn = transition_fn,          # generic transition
    args = c("death", "~STEP"),  # input cols
    states = "death"             # output cols
  )
)
```

### Define a history

Running the model will return the final population. However, we might want to capture time series summaries at each time step along the way.
Further, we may only want to calculate summaries on subsets of rows (the population) - we do this using a filter function to pick out rows of interest.
Here's an example of specifying a history that counts who's alive at each step:
```r
## filter to restrict to alive
filter_alive <- function(x) {
  x == -1 # tests alive applied to 'death'
}

## new_history creates a history
noalive <- new_history(
  columns = list(
    new_column(    # creates a col in history
      name = "no. alive",        # column name
      fn = length,               # summary function
      args = "age",              # input args for summary fn
      filter_fn = filter_alive,  # filter to row-restrict
      filter_args = "death"      # input variables for filter
    )
  ),
  frequency = 1    # record history every 1 steps
)
```

### Collect parameters & run model

We can now group our trajectories, hazards, and history to create a parameter object, and run the model:

```r
## full parameters
parms <- new_parameters(
  hazards = morthaz,
  trajectories = age_traj,
  steps = 10,
  debug = FALSE,
  print_timing = TRUE,
  history = noalive
)

## run the simulation
result <- run_simulation(pop0, parms)
```

We could inspect the output like this:
```r
## look at final state
head(result$pop)

## plot time series
plot(result$history, `no. alive` ~ `~STEP`, type = "l")

## timing information to guide optimization
result$timing
```

## Modelling interdependence

The framework can be used to model interdependence between different components of a system, e.g. to represent resource constraints or feedback loops. We illustrate with a simple SIR (Susceptible-Infectious-Recovered) infection model.

We'll use 1->2->3 to represent S, I, and R compartments and will need a function to transition these, and a history to record their prevalences:
```r
## 1->2->3 transition function
advance <- function(x){
  x <- x + 1
  x
}

## history to record prevalences
prevs <- new_history(
  list(
    new_column(
      name = "Susceptible",
      fn = function(x) sum(x == 1) / length(x),
      args = "state"
    ),
    new_column(
      name = "Infected",
      fn = function(x) sum(x == 2) / length(x),
      args = "state"
    ),
    new_column(
      name = "Recovered",
      fn = function(x) sum(x == 3) / length(x),
      args = "state"
    )
  )
)
```

We create hazards for recovery and infection (which depends on infection prevalence):

```r
## recovery hazard
recrate <- function(x) {
  ans <- (x == 2) * 1 / 7 # only applies to I
  ans
}

## create relevant hazard object
recover_hazard <- new_hazard(
  fn = recrate, # function returning hazard
  args = "state", # arguments for hazard function
  transitions = new_transition( # creates a transition
    fn = advance, # 1->2->3
    args = "state", # args for transition fn
    states = "state" # col transition acts on
  )
)

## force of infection hazard
foi <- function(x) {
  R0 <- 2
  beta <- R0 / 7 # R0 * recovery rate
  foi <- beta * sum(x == 2) / length(x)
  foi * (x == 1) # only applies to S
}


## as hazard object
infect_hazard <- new_hazard(
  fn = foi, # function returning hazard
  args = "state", # arguments for hazard function
  transitions = new_transition( # creates a transition
    fn = advance, # 1->2->3
    args = "state", # args for transition fn
    states = "state" # col transition acts on
  )
)
```

Now we can create and run a simulation, and plot the results:

```r
## parameter object
parms <- new_parameters(
  hazards = list(
    infect_hazard,
    recover_hazard
  ),
  steps = 100,
  history = prevs,
  debug = FALSE
)

## initial population
N <- 1e3L
pop <- data.frame(id = seq_len(N), state = rep(1, N))
pop[1:10, "state"] <- 2 #10 infected individuals


## run the simulation
result <- run_simulation(pop, parms)

## plot the results
matplot(result$history$`~STEP`, result$history[, -1], type = "l", lty = 1, lwd = 2)
legend("topright", legend = names(result$history)[-1], col = 1:3, lty = 1, lwd = 2)
```


## Acknowledgements

Development of this package was carried out as part of the ELDORADO project, funded under Horizon Europe via EDCTP3
