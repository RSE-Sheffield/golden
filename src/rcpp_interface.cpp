#include <Rcpp.h>
#include <random>
#include <stdexcept>
#include <sstream>
#include <set>
#include <limits>
#include <cmath>
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
    
    return List::create(
        _["male"] = out_male,
        _["age"] = out_age,
        _["bmi"] = NumericVector(N, 0), // invalid init, externally init pre-sim
        _["death"] = IntegerVector(N, -1) // invalid init, gets set to time of death
    );
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
    NumericVector result = _result;
    int nan_count = 0;
    int inf_count = 0;
    for (const double &t : result) {
        nan_count += std::isnan(t)?1:0;
        inf_count += std::isinf(t)?1:0;
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
}

inline void check_hazard_contains(List h, const int i, const std::string &name, const std::string &typ) {
    if (!h.containsElementNamed(name.c_str())) {
        std::stringstream err;
        err << "Hazard[" << i <<"] expected to contain " << typ << " '" << name << "'\n";
        throw std::invalid_argument(err.str());
    }
}
inline void check_transition_contains(List t, const int h_i, const int t_i, const std::string &name, const std::string &typ) {
    if (!t.containsElementNamed(name.c_str())) {
        std::stringstream err;
        err << "Hazard[" << h_i <<"] Transition[" <<t_i<< "] expected to contain " << typ << " '" << name << "'\n";
        throw std::invalid_argument(err.str());
    }
}
inline void check_trajectory_contains(List h, const int i, const std::string &name, const std::string &typ) {
    if (!h.containsElementNamed(name.c_str())) {
        std::stringstream err;
        err << "Trajectory[" << i <<"] expected to contain " << typ << " '" << name << "'\n";
        throw std::invalid_argument(err.str());
    }
}

// [[Rcpp::export]]
List run_simulation(List initPop, List parms) {
    // Load parameters, or use default if not present
    if (!parms.containsElementNamed("steps"))
        throw std::invalid_argument("List 'parms' expected to contain integer 'steps'\n");
    const int STEPS = parms.containsElementNamed("steps") ? parms["steps"] : 1;
    const uint64_t RANDOM_SEED = parms.containsElementNamed("random_seed") ? static_cast<uint64_t>(parms["random_seed"]) : 2999569345;
#ifdef NDEBUG
    const bool DEBUG = parms.containsElementNamed("debug") ? static_cast<bool>(parms["debug"]) : false;
    warning("eldoradosim's model debug checks are enabled, these may impact performance.");
#else
    const bool DEBUG = true;
    warning("You are using a development build of eldoradosim, this may impact performance.");
#endif
    std::set<std::string> special_args = {"~STEP"};
    // Validate initPop has columns required by hazard functions
    if (!parms.containsElementNamed("hazards"))
        throw std::invalid_argument("List 'parms' expected to contain vector 'hazards'\n");
    List hazards = parms["hazards"];
    if (!hazards.length())
        throw std::invalid_argument("'parms$hazards' is empty\n");
    for (int h_i = 0; h_i < Rf_length(hazards); ++h_i) {
        List h = hazards[h_i];
        // Check hazard actually contains required elements
        // @todo Can we instead define an R type which would be mostly default init?
        check_hazard_contains(h, h_i, "parms", "vector");
        check_hazard_contains(h, h_i, "fn", "function");
        check_hazard_contains(h, h_i, "transitions", "vector");
        StringVector hazardParms = h["parms"];
        // Check that listed parameters are found inside initPop columns
        for (const String hp : hazardParms) {
            const std::string hp_string = hp.get_cstring();
            if (hp_string.length() > 1 && hp_string[0] == '~') {
                // Special internal arg
                if (special_args.find(hp_string) == special_args.end()) {
                    std::stringstream err;
                    err << "Unrecognised special hazard arg '" << hp_string <<"'.\n";
                    throw std::invalid_argument(err.str());
                }
            } else {
                // Arg from population datatable
                if(!initPop.containsElementNamed(hp_string.c_str())) {
                    std::stringstream err;
                    err << "Column '" << hp_string <<"' expected inside data.table 'initPop'\n";
                    throw std::invalid_argument(err.str());
                }
            }
        }
        List transition_list = h["transitions"];
        for (int t_i = 0; t_i < Rf_length(transition_list); ++t_i) {
            List transit = transition_list[t_i];
            check_transition_contains(transit, h_i, t_i, "parms", "vector");
            check_transition_contains(transit, h_i, t_i, "fn", "function");
            check_transition_contains(transit, h_i, t_i, "state", "string");
            hazardParms = transit["parms"];
            // Check that listed transition parameters are found inside initPop columns
            for (const String hp : hazardParms) {
                const std::string hp_string = hp.get_cstring();
                if (hp_string.length() > 1 && hp_string[0] == '~') {
                    // Special internal arg
                    if (special_args.find(hp_string) == special_args.end()) {
                        std::stringstream err;
                        err << "Unrecognised special hazard transition arg '" << hp_string <<"'.\n";
                        throw std::invalid_argument(err.str());
                    }
                } else {
                    // Arg from population datatable
                    if(!initPop.containsElementNamed(hp_string.c_str())) {
                        std::stringstream err;
                        err << "Column '" << hp_string <<"' expected inside data.table 'initPop'\n";
                        throw std::invalid_argument(err.str());
                    }
                }
            }
            // Confirm transition_state state exists in initPop
            String ts = transit["state"];
            if(!initPop.containsElementNamed(ts.get_cstring())) {
                std::stringstream err;
                err << "Transition state column '" << ts.get_cstring() <<"' expected inside data.table 'initPop'\n";
                throw std::invalid_argument(err.str());
            }            
        }
    }
    // Validate initPop has columns required by trajectory functions
    if (!parms.containsElementNamed("trajectories"))
        throw std::invalid_argument("List 'parms' expected to contain vector 'trajectories'\n");
    List trajectories = parms["trajectories"];
    if (!trajectories.length())
        throw std::invalid_argument("'parms$trajectories' is empty\n");
    for (int t_i = 0; t_i < Rf_length(trajectories); ++t_i) {
        List t = trajectories[t_i];
        // Check hazard actually contains required elements
        // @todo Can we instead define an R type which would be mostly default init?
        check_trajectory_contains(t, t_i, "parms", "vector");
        check_trajectory_contains(t, t_i, "fn", "function");
        check_trajectory_contains(t, t_i, "property", "string");
        StringVector trajParms = t["parms"];
        // Check that listed parameters are found inside initPop columns
        for (const String tp : trajParms) {
            const std::string tp_string = tp.get_cstring();
            if (tp_string.length() > 1 && tp_string[0] == '~') {
                // Special internal arg
                if (special_args.find(tp_string) == special_args.end()) {
                    std::stringstream err;
                    err << "Unrecognised special trajectory arg '" << tp_string <<"'.\n";
                    throw std::invalid_argument(err.str());
                }
            } else {
                // Arg from population datatable
                if(!initPop.containsElementNamed(tp_string.c_str())) {
                    std::stringstream err;
                    err << "Column '" << tp_string <<"' expected inside data.table 'initPop'\n";
                    throw std::invalid_argument(err.str());
                }
            }
        }        
    }
    // @todo Validate data table has other mandatory columns
    
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
    // Create "death" column if it's not present
    // Init Random (currently using runif)
    // @todo seed runif
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
                StringVector p = hazard["parms"];
                List call_args;
                for (const String arg:p) {
                    const std::string arg_string = arg.get_cstring();
                    if (arg_string.length() > 1 && arg_string[0] == '~') {
                        // Map a special arg
                        if (arg_string == "~STEP") {
                            // Special arg corresponds to the step at runtime
                            call_args.push_back(i);
                        } else {
                            // This should never be hit
                            std::stringstream err;
                            err << "Hazard special arg '" << arg_string <<"' not yet implemented.\n";
                            throw std::runtime_error(err.str());
                        }
                    } else {
                        call_args.push_back(outPop[arg]);
                    }
                }
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
                    p = transition["parms"];
                    call_args = List();
                    for (const String arg:p) {
                        const std::string arg_string = arg.get_cstring();
                        if (arg_string.length() > 1 && arg_string[0] == '~') {
                            // Map a special arg
                            if (arg_string == "~STEP") {
                                // Special arg corresponds to the step at runtime
                                call_args.push_back(i);
                            } else {
                                // This should never be hit
                                std::stringstream err;
                                err << "Hazard transition special arg '" << arg_string <<"' not yet implemented.\n";
                                throw std::runtime_error(err.str());
                            }
                        } else {
                            call_args.push_back(outPop[arg]);
                        }
                    }
                    // @todo This currently assumes transition_result and transition_state are both float vectors
                    // If they are instead set to general SEXP, the result ends up a logical vector
                    // If transition state is not even cast to SEXP, ifelse() expects it to be a scalar??
                    // Need a better strategy, as some transition results are likely to be int vector.
                    NumericVector transition_result = dynamic_call(transition["fn"], call_args); // @todo This assumes result is floating point (setting to SAXP gets result converted to TRUE)
                    if (DEBUG)
                        check_result(i, "transition", t_i, transition_result, "hazard", h_i);
                    // Only apply transitions in cases where hazard occurred
                    // This may be faster unvectorised cpp?
                    NumericVector chance = runif(Rf_length(hazard_result));  // Generate vector of random float [0, 1)
                    String ts_name = transition["state"];
                    NumericVector transition_state = outPop[ts_name]; // @todo This assumes result is floating point
                    outPop[ts_name] = ifelse(hazard_result >= chance, transition_result, transition_state);
                    ++t_i;
                }
            }
            ++h_i;
        }
        int t_i = 1; // 1-indexed counter
        // Execute trajectories in order
        for (List trajectory : trajectories) {
            // Currently assumed that trajectories are always active
            // Build arg list to execute hazard chance
            StringVector p = trajectory["parms"];
            List call_args;
            for (const String arg:p) {
                const std::string arg_string = arg.get_cstring();
                if (arg_string.length() > 1 && arg_string[0] == '~') {
                    // Map a special arg
                    if (arg_string == "~STEP") {
                        // Special arg corresponds to the step at runtime
                        call_args.push_back(i);
                    } else {
                        // This should never be hit
                        std::stringstream err;
                        err << "Trajectory special arg '" << arg_string <<"' not yet implemented.\n";
                        throw std::runtime_error(err.str());
                    }
                } else {
                    call_args.push_back(outPop[arg]);
                }
            }
            // Execute hazard function and store result directly in trajectory's property
            String trajectory_prop = trajectory["property"];
            outPop[trajectory_prop] = dynamic_call(trajectory["fn"], call_args);
            if (DEBUG)
                check_result(i, "trajectory", t_i, outPop[trajectory_prop]);
            t_i++;
        }
        printf("\rStep %d complete", i);
    }
    printf("\rSimulation Complete!\n");
    return outPop;
}