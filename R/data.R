#' Example BMI distribution
#'
#'
#'
#' @description This dataset contains example fits of body mass index (BMI) to gamma distributions for the United States in 2022. The original data were taken from an analysis of the contribution of undernutrition to tuberculosis incidence, which was published in the Lancet Global Health in 2026. The data on which these fits are based is from the NCD Risk Factor Collaboration (NCD-RisC) estimates dataset.
#'
#' @format A data.frame/data.table with 28 rows and 4 variables:
#' \describe{
#'   \item{sex}{Biological sex (`char`): Men, Women}
#'   \item{acat}{Age category in years (`char`): 0-4, 5-9, ..., 85plus}
#'   \item{k}{Gamma distribution shape parameter (`num`): k > 0}
#'   \item{theta}{Gamma distribution scale parameter (`num`): theta > 0}
#' }
#' @source https://github.com/petedodd/bmitb
#' @name bmi_fits
#' @docType data
#' @export
"bmi_fits"

#' Example globorisk coefficients
#'
#'
#' @description Example data on cardiovascular disease (CVD) risk from the Globorisk model, which is a model for estimating 10-year risk of fatal and non-fatal CVD events, see:
#'
#' https://doi.org/10.1016/S2213-8587(17)30015-3
#' https://doi.org/10.1016/S2213-8587(15)00081-9
#'
#' These data are available in the `globorisk` R package, see the source link below. This subset is for the United States, and contains the coefficients for the risk prediction equations.
#'
#' The exact extraction of these data from the `globorisk` package is in the `data-raw/dataprep.R` file in this package.
#'
#' @format A data.frame/data.table with 1 rows and 18 variables:
#' \describe{
#'   \item{type}{Risk model type (`char`, set to "office").}
#'   \item{main_sbpc}{Coefficient for centered and scaled systolic blood pressure (`num`).}
#'   \item{main_tcc}{Coefficient for centered and scaled total cholesterol (`num`).}
#'   \item{main__Idm_1}{Variable not used in this model, but included as `NA` for completeness (`num`).}
#'   \item{main_smok}{Variable not used in this model, but included as `NA` for completeness (`num`).}
#'   \item{main_sexdm}{Variable not used in this model, but included as `NA` for completeness (`num`).}
#'   \item{main_sexsmok}{Variable not used in this model, but included as `NA` for completeness (`num`).}
#'   \item{tvc_sbpc}{Coefficient for centered and scaled systolic blood pressure by age interaction term (`num`).}
#'   \item{tvc_tcc}{Variable not used in this model, but included as `NA` for completeness (`num`).}
#'   \item{tvc_dm}{Variable not used in this model, but included as `NA` for completeness (`num`).}
#'   \item{tvc_smok}{Variable not used in this model, but included as `NA` for completeness (`num`).}
#'   \item{main_bmi5c}{Coefficient for centered and scaled body mass index (BMI) (`num`).}
#'   \item{main_smokc}{Coefficient for centered and scaled smoking status (`num`).}
#'   \item{main_sexsmokc}{Coefficient for centered and scaled smoking by sex interaction term (`num`).}
#'   \item{tvc_smokc}{Coefficient for centered and scaled smoking status by age interaction term (`num`).}
#'   \item{tvc_bmi5c}{Coefficient for centered and scaled BMI by age interaction term (`num`).}
#'   \item{main_sbpsexc}{Variable not used in this model, but included as `NA` for completeness (`num`).}
#'   \item{lac}{Globorisk use Local Area Countries flag (`num`, set to 0).}
#' }
#' @source https://github.com/boyercb/globorisk
#' @name globorisk_coefs
#' @docType data
#' @export
"globorisk_coefs"

#' Example globorisk baseline hazard
#'
#'
#'
#' @description Example data on cardiovascular disease (CVD) risk from the Globorisk model, which is a model for estimating 10-year risk of fatal and non-fatal CVD events, see:
#'
#' https://doi.org/10.1016/S2213-8587(17)30015-3
#' https://doi.org/10.1016/S2213-8587(15)00081-9
#'
#' These data are available in the `globorisk` R package, see the source link below. This subset is for the United States in 2000, and contains the baseline hazard.
#'
#' The exact extraction of these data from the `globorisk` package is in the `data-raw/dataprep.R` file in this package.
#'
#' @format A data.frame/data.table with 20 rows and 4 variables:
#' \describe{
#'   \item{agec}{Age category (`int`): 1,...,10.}
#'   \item{sex}{Sex (`int`): 0, 1.}
#'   \item{cvd_0}{Baseline hazard per year for CVD events (`num`).}
#'   \item{agesex}{Age and sex concatenated category (`char`): "1_0", "1_1", ..., "10_1".}
#' }
#' @source https://github.com/boyercb/globorisk
#' @name globorisk_cvdr
#' @docType data
#' @export
"globorisk_cvdr"

#' Example globorisk reference values
#'
#'
#'
#' @description Example data on cardiovascular disease (CVD) risk from the Globorisk model, which is a model for estimating 10-year risk of fatal and non-fatal CVD events, see:
#'
#' https://doi.org/10.1016/S2213-8587(17)30015-3
#' https://doi.org/10.1016/S2213-8587(15)00081-9
#'
#' These data are available in the `globorisk` R package, see the source link below. This subset is for the United States, and contains the reference values for centering.
#'
#' The exact extraction of these data from the `globorisk` package is in the `data-raw/dataprep.R` file in this package.
#'
#' @format A data.frame/data.table with 18 rows and 9 variables:
#' \describe{
#'   \item{iso}{ISO country code (`char`): "USA".}
#'   \item{agec}{Age category (`int`): 1,...,10.}
#'   \item{sex}{Sex (`int`): 0, 1.}
#'   \item{mean_sbp}{Systolic blood pressure mean value for centering (`num`).}
#'   \item{mean_tc}{Total cholesterol mean value for centering (`num`).}
#'   \item{mean_dm}{Diabetes mean value for centering (`num`).}
#'   \item{mean_smk}{Smoking status mean value for centering (`num`).}
#'   \item{mean_bmi}{Body mass index mean value for centering (`num`).}
#'   \item{agesex}{Age and sex concatenated category (`char`): "1_0", "1_1", ..., "9_1".}
#' }
#' @source https://github.com/boyercb/globorisk
#' @name globorisk_rf
#' @docType data
#' @export
"globorisk_rf"

#' Example life table data
#'
#' @description Example life table data for the United States from 2000 to 2100, with age-specific mortality rates for each sex and in total. The original data were taken from the World Population Prospects 2024 revision, which is published by the United Nations Department of Economic and Social Affairs, Population Division.
#'
#' https://population.un.org/wpp/
#'
#' The data on which these life tables are based is from the WPP 2024 revision, as made available through the `wpp2024` R package available from the source below. The exact extraction of these data from the WPP 2024 revision is in the `data-raw/dataprep.R` file in this package.
#'
#' @format A data.frame/data.table with 10,201 rows and 5 variables:
#' \describe{
#'   \item{year}{The year of the life table data (`integer`), ranging from 2000 to 2100.}
#'   \item{age}{The age group for the life table data (`integer`), ranging from 0 to 100.}
#'   \item{mxM}{Actuarial mortality for men; age-specific hazard of death (`num`).}
#'   \item{mxF}{Actuarial mortality for women; age-specific hazard of death (`num`).}
#'   \item{mxB}{Actuarial mortality for both sexes combined; age-specific hazard of death (`num`).}
#' }
#' @source https://github.com/PPgp/wpp2024
#' @name lifetable_data
#' @docType data
#' @export
"lifetable_data"

#' Example population structure
#'
#' @description Example population data for the United States for 2000, with age-specific populations in thousands. The original data were taken from the World Population Prospects 2024 revision, which is published by the United Nations Department of Economic and Social Affairs, Population Division.
#'
#' https://population.un.org/wpp/
#'
#' The data on which these population data are based is from the WPP 2024 revision, as made available through the `wpp2024` R package available from the source below. The exact extraction of these data from the WPP 2024 revision is in the `data-raw/dataprep.R` file in this package.
#'
#' @format A matrix with 101 rows and 2 columns:
#' \describe{
#'   \item{popM}{Named first matrix colum: the male population in thousands for each age group, with rows for ages 0 to 100 years.}
#'  \item{popF}{Named second matrix column: the female population in thousands for each age group, with rows for ages 0 to 100 years.}
#' }
#' @source https://github.com/PPgp/wpp2024
#' @name pop_snapshot
#' @docType data
#' @export
"pop_snapshot"
