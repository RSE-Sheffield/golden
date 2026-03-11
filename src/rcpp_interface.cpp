// Not currently used
//#ifdef _OPENMP
//#include <omp.h>
//#endif
#include <Rcpp.h>
using namespace Rcpp;

#include "Simulation.h"

//' Create a new cohort
//'
//' Temporary testing method, probably replaced in future with R's simdata package or similar
//'
//' @param demog Demographic information containing columns AgeGrp/PopMale/PopFemale/PopTotal
//' @param N Size of the population to generate
//' @return A sample population data.table, with columns male/age/bmi/death
//' 
//' @examples
//' library(data.table)
//' demog <- data.table(
//'   AgeGrp = c(0, 1, 2, 3),
//'   PopMale = c(1000, 1100, 1050, 980),
//'   PopFemale = c(950, 1020, 1005, 970)
//' )
//' demog[, PopTotal := PopMale + PopFemale]
//' cohort <- create_cohort(demog, 100)
// [[Rcpp::export]]
List create_cohort(List demog, unsigned int N) {
    // Validate demog contains the required columns
    if (!demog.containsElementNamed("AgeGrp"))
        throw std::invalid_argument("List 'demog' expected to contain vector 'AgeGrp'\n");
    if (!demog.containsElementNamed("PopMale"))
        throw std::invalid_argument("List 'demog' expected to contain vector 'PopMale'\n");
    if (!demog.containsElementNamed("PopFemale"))
        throw std::invalid_argument("List 'demog' expected to contain vector 'PopFemale'\n");
    if (!demog.containsElementNamed("PopTotal"))
        throw std::invalid_argument("List 'demog' expected to contain vector 'PopTotal'\n");
    
    // Calculate the scale factor
    NumericVector pTot = demog["PopTotal"];
    const double SCALE_FACTOR = static_cast<double>(N) / sum(pTot);
    
    // Scale the male and female vectors
    NumericVector mTot = demog["PopMale"];
    NumericVector fTot = demog["PopFemale"];
    mTot = mTot * SCALE_FACTOR;
    fTot = fTot * SCALE_FACTOR;
    // Floor final value to avoid exceeding (this is naive, need to essentially make the total floored)
    fTot[fTot.size()-1] = floor(fTot[fTot.size()-1]);
    // Create and fill the output vector
    IntegerVector out_male = IntegerVector(N);
    NumericVector out_age = NumericVector(N);
    IntegerVector ages = demog["AgeGrp"];
    unsigned int i_i = 0; // Index counter
    double d_i = 0; // double counter to track the shape of the demographic bins
    
    // Setup males
    for (unsigned int m_i = 0; m_i < mTot.size(); ++m_i) {
        while(d_i < (double)mTot[m_i]) {
            out_male[i_i] = 1;
            out_age[i_i] = ages[m_i];
            i_i+=1;
            d_i+=1;
        }
        d_i -= mTot[m_i];
    }
    // Setup females
    for (unsigned int f_i = 0; f_i < fTot.size(); ++f_i) {
        while(d_i < fTot[f_i]) {
            out_male[i_i] = 0;
            out_age[i_i] = ages[f_i];
            i_i+=1;
            d_i+=1;
        }
        d_i -= fTot[f_i];
    }
    
   List ret = List::create(
        _["male"] = out_male,
        _["age"] = out_age,
        _["bmi"] = NumericVector(N, 0), // invalid init, externally init pre-sim
        _["death"] = IntegerVector(N, -1) // invalid init, gets set to time of death
    );
    // Make it a data.table
    ret.attr("class") = CharacterVector::create("data.table", "data.frame");
    ret.attr("row.names") = IntegerVector::create(NA_INTEGER, -static_cast<int>(N));
    return ret;
}

//' Execute a patient trajectory simulation
//'
//' @param initPop data.table containing initial population for simulation
//' @param parameters Simulation configuration
//' @return An list containing final population, history and timing data.tables
//'
//' @examples
//' library(data.table)
//' N <- 100
//' dt <- data.table(a = runif(N, 0, 1), b = rep(0, N))
//' # Define a hazard function, which returns a vector of equal length uncertainties
//' test_hazard <- function(a) {
//'     ret <- (a < 0.5)
//' }
//' # Define a transition function, which sets all "b" columns affected by the hazard to 100
//' test_transition <- function() {
//'     return (100)
//' }
//' # Create an S3 golden_hazard
//' haz <- new_hazard(
//'               test_hazard,
//'               c("a"),
//'               new_transition(test_transition, c(), "b")
//'             )
//' # Define a trajectory function, which adds 2 to all members of the input vector
//' test_trajectory <- function(a) {
//'     return (a + 2)
//' }
//' # Define an S3 golden_trajectory
//' trj <- new_trajectory(test_trajectory, c("b"), "b")
//' # Create an S3 golden_history, containing 1 golden_history_column
//' hist <- new_history(new_column("sum_a", sum, c("a")))
//' # Define an S3 golden_parameters
//' params <- new_parameters(
//'   hazards = haz,
//'   trajectories = trj,
//'   steps = 10,
//'   debug = FALSE,
//'   history = hist
//' )
//' # Run the simulation to collect results
//' results <- run_simulation(dt, params)
// [[Rcpp::export]]
List run_simulation(List initPop, List parameters) {
    try {
        // Call golden::check_parameters()
        {        
            Environment golden = Environment::namespace_env("golden");
            Function check_parameters = golden["check_parameters"];

            check_parameters(parameters, initPop);
        }
        // Init and run simulation
        Simulation s(parameters);
        return s.run(initPop);
    } catch (std::exception &e) {
        forward_exception_to_r(e);
    } catch(...) {
        ::Rf_error("Unknown C++ exception within run_simulation()"); 
    }
    return List();
}
