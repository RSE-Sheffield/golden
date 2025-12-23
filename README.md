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

### Define trajectories

Ageing can serve as a simple example of a trajectory. We define an update rule and create a trajectory object that knows how to apply the update:
```r
## update function
age_update <- function(age, death_time) {
  ifelse(death_time == -1, age + 1, age)
}

## new_trajectory creates a trajectory
age_traj <- new_trajectory(
  age_update,        # update function
  c("age", "death"), # args for update function
  "age"              # column(s) to be updated
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
  deathrate, # hazard function
  c("death", "sbp", "tc"), # args for hazard fn
  ## (list of) transitions to apply
  new_transition(
    transition_fn,       # generic transition
    c("death", "~STEP"), # input cols
    "death"              # output cols
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
      "no. alive", # column name
      length,      # summary function
      "age",       # input args for summary fn
      filter_alive,# filter to row-restrict
      "death"      # input variables for filter
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


## Acknowledgements

Development of this package was carried out as part of the ELDORADO project, funded under Horizon Europe via EDCTP3
