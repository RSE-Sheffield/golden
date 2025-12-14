library(here)
library(usethis)
library(data.table)

## https://github.com/boyercb/globorisk
library(globorisk)
## https://github.com/PPgp/wpp2024
library(wpp2024)
data(mx1dt)
data(popAge1dt)


## develop some test data for USA

## --- globorisk

## convert these to restricted/faster look-up tables
globorisk_coefs <- as.data.table(globorisk:::coefs)
globorisk_coefs <- globorisk_coefs[type == "office" & lac == 0]

globorisk_rf <- as.data.table(globorisk:::rf)
globorisk_rf <- globorisk_rf[iso == "USA"]
globorisk_rf[, agesex := paste(agec, sex, sep = "_")]
setkey(globorisk_rf, agesex)
globorisk_rf[c("1_1", "1_0")] #test


globorisk_cvdr <- as.data.table(globorisk:::cvdr)
globorisk_cvdr <- globorisk_cvdr[
  iso == "USA" & type == "FNF" & year == 2000,
  .(agec, sex, cvd_0)
]
globorisk_cvdr <- unique(globorisk_cvdr)
globorisk_cvdr[, agesex := paste(agec, sex, sep = "_")]
setkey(globorisk_cvdr, agesex)
globorisk_cvdr[c("1_1", "1_0")] #test


## adatped from the globorisk R package
## slow: could do better with same package data
globohaz_slow <- function(sex, age, bmi, full_out = FALSE) {
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
  h * HR
}

globohaz(c(0, 1, 1), c(00, 50, 90), rep(25, 3))
globohaz_slow(c(0, 1, 1), c(00, 50, 90), rep(25, 3))

## include these data in package
usethis::use_data(
  globorisk_rf,
  globorisk_coefs,
  globorisk_cvdr,
  overwrite = TRUE
)


## --- demographic data
country_name <- "United States of America"

## for initial population
pop_snapshot <- as.matrix(popAge1dt[
  name == country_name &
    year == 2000,
  .(popM, popF)
])

## for mortality rates
lifetable_data <- mx1dt[
  name == country_name &
    year >= 2000,
  .(year, age, mxM, mxF, mxB)
]

## include these data in package
usethis::use_data(
  lifetable_data,
  pop_snapshot,
  overwrite = TRUE
)

## --- BMI data

## load some fits of BMI data
## see https://github.com/petedodd/bmitb
load("~/Documents/bmitb/data/DRB.Rdata")
DRB <- DRB[iso3 == "USA"] #NB 2022

bmi_fits <- DRB[, .(sex = Sex, acat = age, k, theta)]

## include these data in package
usethis::use_data(
  bmi_fits,
  overwrite = TRUE
)


## document
devtools::document()
