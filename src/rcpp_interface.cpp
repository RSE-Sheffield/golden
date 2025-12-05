#include <random>
#include <stdexcept>
#include <sstream>
#include <set>
#include <limits>
#include <cmath>
#include <chrono>
// Not currently used
//#ifdef _OPENMP
//#include <omp.h>
//#endif
#include <Rcpp.h>
using namespace Rcpp;

#include "utils/dynamic_call.h"
#include "utils/debug.h"

/**
 * Temporary testing method, probably replaced in future with R's simdata package or similar
 */
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

// [[Rcpp::export]]
List run_simulation(List initPop, List parameters) {
    // Call eldoradosim::check_parameters()
    {        
        Environment eldoradosim = Environment::namespace_env("eldoradosim");
        Function check_parameters = eldoradosim["check_parameters"];
        check_parameters(parameters);
    }
    // Extract configuration options
    const int STEPS = parameters["steps"];
    if (static_cast<int32_t>(parameters["random_seed"])) {
        // R uses a global random state, however if a user provides a seed we will override that state
        Function set_seed("set.seed");
        set_seed(static_cast<int32_t>(parameters["random_seed"]));
    }
#ifdef NDEBUG
    const bool DEBUG = static_cast<bool>(parameters["debug"]);
    warning("eldoradosim's model debug checks are enabled, these may impact performance.");
#else
    const bool DEBUG = true;
    warning("You are using a development build of eldoradosim, this may impact performance.");
#endif
    const std::set<std::string> special_args = {"~STEP"};
    List hazards = parameters["hazards"];
    List trajectories = parameters["trajectories"];
    List history = parameters.containsElementNamed("history") ? parameters["history"] : R_NilValue;
    
    // Create a deep-copy of the initial pop, that we will return with changes at the end
    // @todo create other columns, to log last hazard score?
    List outPop = clone(initPop);
    // Init History Data-table
    List history_dt = List::create();
    if (history.size()) {
        // Create the ~STEP column
        const int history_freq = history["frequency"];
        const int history_rows = STEPS / history_freq;
        IntegerVector stepv = IntegerVector(history_rows, 0);
        for (int i = 0; i < history_rows; ++i) {
            stepv[i] = (i * history_freq) + 1;
        }
        history_dt["~STEP"] = stepv;
    }
    // Simulation loop
    for (int i = 0; i < STEPS; ++i) {
        int h_i = 1;  // 1 index'd similar to R
        // Execute hazards in order, if required at current step
        for (List hazard : hazards) {
            // If hazard is active this step
            const unsigned int freq = hazard.containsElementNamed("freq") ? hazard["freq"] : 1;
            const int before = hazard.containsElementNamed("before") ? hazard["before"] : std::numeric_limits<int>::max();
            const int after = hazard.containsElementNamed("after") ? hazard["after"] : -1;
            if(i % freq == 0 && i < before && i > after) {
                // Build arg list to execute hazard chance
                List call_args = build_args(hazard["args"], outPop, i);
                // Execute hazard function and process results
                // Result should be a vector of chance, need to process these as a vector vs random
                // Then apply transition functions to affected agents
                NumericVector hazard_result = dynamic_call(hazard["fn"], call_args);
                if (DEBUG)
                    check_result(i, "hazard", h_i, hazard_result);
                // Process hazard's transitions
                List transition_list = hazard["transitions"];
                int t_i = 1;  // 1 index'd similar to R
                for(List transition : transition_list) {
                    // Build arg list to execute hazard transition
                    call_args = build_args(transition["args"], outPop, i);
                    // @todo can this be hidden inside a util function cleanly?
                    // Only apply transitions in cases where hazard occurred
                    // This may be faster unvectorised cpp?
                    NumericVector chance = runif(Rf_length(hazard_result));  // Generate vector of random float [0, 1)
                    String ts_name = transition["state"];
                    // Select correct output based on output state type
                    if (Rf_isNumeric(outPop[ts_name])) {
                        NumericVector transition_result = dynamic_call(transition["fn"], call_args);
                        if (DEBUG)
                            check_result(i, "transition", t_i, transition_result, "hazard", h_i);
                        NumericVector transition_state = outPop[ts_name];
                        outPop[ts_name] = ifelse(hazard_result >= chance, transition_result, transition_state);
                    } else if (Rf_isInteger(outPop[ts_name])) {
                        IntegerVector transition_result = dynamic_call(transition["fn"], call_args);
                        IntegerVector transition_state = outPop[ts_name];
                        outPop[ts_name] = ifelse(hazard_result >= chance, transition_result, transition_state);
                    } else if (Rf_isLogical(outPop[ts_name])) {
                        LogicalVector transition_result = dynamic_call(transition["fn"], call_args);
                        LogicalVector transition_state = outPop[ts_name];
                        outPop[ts_name] = ifelse(hazard_result >= chance, transition_result, transition_state);
                    } else {
                        // @todo Support other types?(Complex, String, Date, Datetime)
                        stop("Unsupported type at transition processing");
                    }
                    ++t_i;
                }
            }
            ++h_i;
        }
        int t_i = 1; // 1-indexed counter
        // Execute trajectories in order
        for (List trajectory : trajectories) {
            // Currently assumed that trajectories are always active
            // Build arg list to execute trajectory chance
            List call_args = build_args(trajectory["args"], outPop, i);
            // Execute trajectory function and store result directly in trajectory's property
            // Slightly different path, depending on whether it returns 1 or multiple properties
            if (Rf_length(trajectory["property"]) == 1) {
                // Single return value (returned list should be a column)
                String trajectory_prop = trajectory["property"];
                outPop[trajectory_prop] = dynamic_call(trajectory["fn"], call_args);
                if (DEBUG)
                    check_result(i, "trajectory", t_i, outPop[trajectory_prop]);
            } else {
                // Multiple return values (returned list, should be a list of columns)
                List trajectory_returns = dynamic_call(trajectory["fn"], call_args);
                CharacterVector trajectory_properties = trajectory["property"];
                if (Rf_length(trajectory_properties) != Rf_length(trajectory_returns))
                    stop("Trajectory function return value contains a different number of properties than expected.");
                for (int i = 0; i < Rf_length(trajectory_properties); ++i ) {
                    String trajectory_prop = trajectory_properties[i];
                    outPop[trajectory_prop] = trajectory_returns[i];
                    if (DEBUG)
                        check_result(i, "trajectory", t_i, outPop[trajectory_prop]);
                }
            }
            t_i++;
        }
        // Process any history to be collected
        if (history.size()) {
            // Is history desired this step
            const unsigned int freq = history["frequency"];
            if (i % freq == 0) {
                List columns = history["columns"];
                const unsigned int hist_i = i / freq;
                // Calculate and store result for each column of output
                for (List col : columns) {
                    String name = col["name"];
                    // Calculate filter vector if required by column
                    LogicalVector filter_v = LogicalVector();
                    if (col["filter_fn"] != R_NilValue) {
                        List call_args = build_args(col["filter_args"], outPop, i);
                        filter_v = dynamic_call(col["filter_fn"], call_args);
                    }
                    // Columns may be filtered here
                    List call_args = col["filter_fn"] != R_NilValue
                        ? build_args_filtered(col["args"], outPop, i, filter_v)
                        : build_args(col["args"], outPop, i);
                    // Call the dynamic function
                    SEXP result = dynamic_call(col["fn"], call_args);
                    if (hist_i == 0) {
                        // Detect the type of the result and create corresponding vector
                        if (Rf_isNumeric(result)) {
                            history_dt[name] = NumericVector(STEPS/freq, 0);
                        } else if (Rf_isInteger(result)) {
                            history_dt[name] = IntegerVector(STEPS/freq, 0);
                        } else if (Rf_isLogical(result)) {
                            history_dt[name] = LogicalVector(STEPS/freq, false);
                        } else {
                            // @todo Support other types?(Complex, String, Date, Datetime)
                            stop("Unsupported type at history processing");
                        }
                    }
                    // Store result
                    if (Rf_isNumeric(result)) {
                        NumericVector t = history_dt[name];
                        t[hist_i] = as<double>(result);
                    } else if (Rf_isInteger(result)) {
                        IntegerVector t = history_dt[name];
                        t[hist_i] = as<int>(result);
                    } else if (Rf_isLogical(result)) {
                        LogicalVector t = history_dt[name];
                        t[hist_i] = as<bool>(result);
                    } else {
                        // @todo Support other types?(Complex, String, Date, Datetime)
                        stop("Unsupported type at history processing");
                    }
                }
            }
        }
        printf("\rStep %d complete", i);
    }
    printf("\rSimulation Complete!\n");
    
    // Mark outpop as a data table
    outPop.attr("class") = CharacterVector::create("data.table", "data.frame");
    outPop.attr("row.names") = IntegerVector::create(NA_INTEGER, -Rf_length(outPop[0]));    
    if (history.size()) {
        // Mark history as a data table
        // (This must be done after we're finished adding columns)
        const int history_freq = history["frequency"];
        const int history_rows = STEPS / history_freq;
        history_dt.attr("class") = CharacterVector::create("data.table", "data.frame");
        history_dt.attr("row.names") = IntegerVector::create(NA_INTEGER, -history_rows);
        // Package it into a structure for return
        List ret = List::create(
            _["pop"]   = outPop,
            _["history"] = history_dt
        );
        return ret;
    } else {
        return outPop;
    }
}