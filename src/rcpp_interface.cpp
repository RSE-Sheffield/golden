#include <Rcpp.h>
#include <random>
#include <stdexcept>
#include <sstream>
#include <set>
#include <limits>
#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;

// [[Rcpp::export]]
bool is_odd(int d){
    return d % 2 != 0;
}

// [[Rcpp::export]]
LogicalVector is_odd_vector(IntegerVector x) {
    const int n = x.size();
    LogicalVector result(n);
    #pragma omp parallel for
    for (int i = 0; i < n; i++) {
        result[i] = x[i] % 2 != 0;
    }
    
    return result;
}

/**
 * Convenience function returning the inclusive prefix sum of x
 */
NumericVector prefix_sum(NumericVector x) {
    NumericVector out(x.size());
    std::partial_sum(x.begin(), x.end(), out.begin());
    return out;
}

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

inline void check_hazard_contains(List h, const int i, const std::string &name, const std::string &typ) {
    if (!h.containsElementNamed(name.c_str())) {
        std::stringstream err;
        err << "Hazard[" << i <<"] expected to contain " << typ << " '" << name << "'\n";
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
    const int STEPS = parms.containsElementNamed("steps") ? parms["steps"] : 1;
    const uint64_t RANDOM_SEED = parms.containsElementNamed("random_seed") ? static_cast<uint64_t>(parms["random_seed"]) : 2999569345;
    std::set<std::string> special_args = {"~STEP"};  // @note technically ~RESULT is only valid for transitions
    // Validate initPop has columns required by hazard functions
    if (!parms.containsElementNamed("hazards"))
        throw std::invalid_argument("List 'parms' expected to contain vector 'hazards'\n");
    List hazards = parms["hazards"];
    for (int h_i = 0; h_i < Rf_length(hazards); ++h_i) {
        List h = hazards[h_i];
        // Check hazard actually contains required elements
        // @todo Can we instead define an R type which would be mostly default init?
        check_hazard_contains(h, h_i, "parms", "vector");
        check_hazard_contains(h, h_i, "fn", "function");
        check_hazard_contains(h, h_i, "transition_fn", "function");
        check_hazard_contains(h, h_i, "transition_state", "string");
        check_hazard_contains(h, h_i, "transition_parms", "vector");
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
        hazardParms = h["transition_parms"];
        // Check that listed transition parameters are found inside initPop columns
        for (const String hp : hazardParms) {
            const std::string hp_string = hp.get_cstring();
            if (hp_string.length() > 1 && hp_string[0] == '~') {
                // Special internal arg
                if (special_args.find(hp_string) == special_args.end() && hp_string != "~RESULT") {
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
        String ts = h["transition_state"];
        if(!initPop.containsElementNamed(ts.get_cstring())) {
            std::stringstream err;
            err << "Transition state column '" << ts.get_cstring() <<"' expected inside data.table 'initPop'\n";
            throw std::invalid_argument(err.str());
        }
    }
    // Validate initPop has columns required by trajectory functions
    if (!parms.containsElementNamed("trajectories"))
        throw std::invalid_argument("List 'parms' expected to contain vector 'trajectories'\n");
    List trajectories = parms["trajectories"];
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
                NumericVector result = dynamic_call(hazard["fn"], call_args);
                // This may be faster unvectorised cpp?
                NumericVector chance = runif(Rf_length(result));  // Generate vector of random float [0, 1)
                // Build arg list to execute hazard transition
                p = hazard["transition_parms"];
                call_args = List();
                for (const String arg:p) {
                    const std::string arg_string = arg.get_cstring();
                    if (arg_string.length() > 1 && arg_string[0] == '~') {
                        // Map a special arg
                        if (arg_string == "~STEP") {
                            // Special arg corresponds to the step at runtime
                            call_args.push_back(i);
                        } else if (arg_string == "~RESULT") {
                            // Special arg corresponds to True/False whether hazard passed
                            call_args.push_back((result >= chance)); 
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
                String transition_state = hazard["transition_state"];
                outPop[transition_state] = dynamic_call(hazard["transition_fn"], call_args);
            }
        }
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
        }
        printf("\rStep %d complete", i);
    }
    printf("\rSimulation Complete!\n");
    return outPop;
}