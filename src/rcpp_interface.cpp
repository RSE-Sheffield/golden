#include <Rcpp.h>
#include <random>
#include <stdexcept>
#include <sstream>
#include <set>
#include <limits>
#include <cmath>
#include <chrono>
#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;

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

/**
 * Dynamically call an R function from cpp
 *
 * R functions may require any number of arguments
 * The RCPP Function::operator() cannot be called in a varadic manner
 * (e.g. dynamically changing the number of arguments at runtime)
 * Hence this method instead acts as a wrapper, whereby a vector of arguments can be passed to the function
 * @param f The R function to be called
 * @param args A List of arguments to be passed.
 */
SEXP dynamic_call(Function f, List args) {
    // Create a call object
    Language call(f);

    // Append all arguments dynamically
    for (R_xlen_t i = 0; i < args.size(); ++i) {
        call.push_back(args[i]);
        // @note To support named arguments a PairList(<name>, args[i]) would be pushed back here.
    }

    // Evaluate the call
    // @todo what happens if the call is stored and eval'd twice?
    return call.eval();
}
/**
 * Debug method to check output of dynamic methods, to ensure they don't contain NaN/Inf
 * 
 * @param s_i Step index
 * @param fn_type String representation of the function type (e.g. hazard, trajectory, transition)
 * @param fn_i Function index, e.g. 2nd hazard in the list. Assumed 1-indexed same as R, rather than 0-indexed C
 * @param _result The result vector to be checked, assumed to be NumericVector (only floating point types have NaN)
 * @param parent_typ (Optional) For transition functions, this will be set to "hazard"
 * @param p_i (Optional) Index of the parent type (hazard). Assumed 1-indexed same as R, rather than 0-indexed C
 * @note Not sure R has a concept of Inf, everything so far seems to be NA, which I assume to be NaN
 * @note Labelling may need adjustments if allowing multiple transitions per hazard
 */
void check_result(int s_i, const std::string &fn_typ, int fn_i, SEXP _result, const std::string &parent_typ = "", int p_i = 0) {
    if (Rf_isNumeric(_result)) {
        NumericVector result = _result;
        int nan_count = 0;
        int inf_count = 0;
        int range_count = 0;
        for (const double &t : result) {
            nan_count += std::isnan(t)?1:0;
            inf_count += std::isinf(t)?1:0;
            if (t < 0) {
                ++range_count;
            }
        }
        if (nan_count + inf_count) {
            // This should never be hit
            std::stringstream err;
            err << "[DEBUG]During step " << s_i << " " << fn_typ << "[" << fn_i << "] ";
            if (!parent_typ.empty()) {
                err << "from " << parent_typ << "[" << p_i << "] ";
            }
            err << "return contained " << nan_count <<" NaN values and " << inf_count << " Inf values.\n";
            throw std::runtime_error(err.str());
        }
        if (fn_typ == "hazard" && range_count) {
            // Special case, warn if hazard return is <0
            std::stringstream err;
            err << "[DEBUG]During step " << s_i << " " << fn_typ << "[" << fn_i << "] ";
            err << "return contained " << range_count <<" negative values.\n";
            err << "Hazards are expected to return values greater than or equal to 0\n";
            warning(err.str());
        }
    } else {
        if (fn_typ == "hazard") {
            // This should never be hit
            std::stringstream err;
            err << "[DEBUG]During step " << s_i << " " << fn_typ << "[" << fn_i << "] ";
            err << "return was type " << type2name(_result) << " expected type double\n";
            throw std::runtime_error(err.str());
        }
    }
}

/**
 * Utility which returns vector v with only elements with corresponding true element inside keep
 * 
 * @param v Input vector to be filtered, assumed type IntegerVector, NumericVector, LogicalVector or CharacterVector
 * @param keep Logical vector denoting elements of v to be retained
 * @return A copy of v with selected elements removed
 */
SEXP logical_filter(SEXP v, LogicalVector keep) {
    if (Rf_length(v) != keep.size()) {
        stop("Argument vector length mismatch in logical_filter()");
    }

    // Count length of output vector
    // To avoid dynamic resize
    // @todo this could be calculated once
    int out_len = 0;
    for (int i = 0; i < keep.size(); i++) {
        if (keep[i]) out_len++;
    }

    // Special case per type
    int j = 0;
    if (Rf_isNumeric(v)) {
        NumericVector typed_v(v);
        NumericVector out(out_len);
        for (int i = 0; i < keep.size(); ++i)
            if (keep[i]) out[j++] = typed_v[i];
        return out;
    } else if(Rf_isInteger(v)) {
        IntegerVector typed_v(v);
        IntegerVector out(out_len);
        for (int i = 0; i < keep.size(); ++i)
            if (keep[i]) out[j++] = typed_v[i];
        return out;
    } else if(Rf_isLogical(v)) {
        LogicalVector typed_v(v);
        LogicalVector out(out_len);
        for (int i = 0; i < keep.size(); ++i)
            if (keep[i]) out[j++] = typed_v[i];
        return out;
    } else if(Rf_isString(v)) {
        CharacterVector typed_v(v);
        CharacterVector out(out_len);
        for (int i = 0; i < keep.size(); ++i)
            if (keep[i]) out[j++] = typed_v[i];
        return out;
    } else {
        stop("Unsupported SEXP type in logical_filter()");
    }
}

/**
 * Utility function covering the common task of building up the call arguments to be passed to dynamic_call()
 *
 * @param p String vector of column names and ~SPECIAL values
 * @param table Data table which columns will be taken from
 * @param current_step Value to provided for "~STEP"
 * @return List containing R objects and variables to be passed to dynamic_call()
 */
List build_args(StringVector p, List table, int current_step) {
    List call_args;
    for (const String arg:p) {
        const std::string arg_string = arg.get_cstring();
        if (arg_string.length() > 1 && arg_string[0] == '~') {
            // Map a special arg
            if (arg_string == "~STEP") {
                // Special arg corresponds to the step at runtime
                call_args.push_back(current_step);
            } else {
                // This should never be hit
                std::stringstream err;
                err << "Special arg '" << arg_string <<"' not yet implemented in build_args().\n";
                throw std::runtime_error(err.str());
            }
        } else {
            call_args.push_back(table[arg]);
        }
    }
    return call_args;
}
/**
 * Utility function covering the common task of building up the call arguments to be passed to dynamic_call()
 * This version applies logical_filter() to columns included in the return value with filter_v
 * @param filter_v LogicalVector passed to keep parameter of logical_filter
 * @see build_args()
 */
List build_args_filtered(StringVector p, List table, int current_step, LogicalVector filter_v) {
    List call_args;
    for (const String arg:p) {
        const std::string arg_string = arg.get_cstring();
        if (arg_string.length() > 1 && arg_string[0] == '~') {
            // Map a special arg
            if (arg_string == "~STEP") {
                // Special arg corresponds to the step at runtime
                call_args.push_back(current_step);
            } else {
                // This should never be hit
                std::stringstream err;
                err << "Special arg '" << arg_string <<"' not yet implemented in build_args().\n";
                throw std::runtime_error(err.str());
            }
        } else {
            call_args.push_back(logical_filter(table[arg], filter_v));
        }
    }
    return call_args;
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
    // Add/init required columns
    if (!outPop.containsElementNamed("death")) {
        IntegerVector death_col(Rf_length(outPop[0]), -1);
        outPop.push_back(death_col);
    } else {
        IntegerVector death_col(Rf_length(outPop[0]), -1);
        outPop["death"] = death_col;
    }
    // Init History Data-table
    List history_dt = List::create();
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
                // Result should be a vector of death chance, need to process these as a vector vs random
                // Then update the death flag for affected agents
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
    
    if (history.size()) {
        // Mark history as a data table
        // (This can't be done before it has columns)
        const unsigned int freq = history["frequency"];
        const int history_rows = STEPS / freq;
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