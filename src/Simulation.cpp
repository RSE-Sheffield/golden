#include "Simulation.h"
/**
 * defined in R/src/include/Rinterface.h
 * This bool denotes whether R is executing in interactive mode
 * CRAN requires that non-interactive runs do not output to console
 */
extern Rboolean R_Interactive;

#include "utils/dynamic_call.h"
#include "utils/debug.h"


const std::set<std::string> Simulation::SPECIAL_ARGS = {"~STEP"};

int initHistoryFreq(List parameters) {
    List history = parameters.containsElementNamed("history") ? parameters["history"] : R_NilValue;
    if (history.size())
        return history["frequency"];
    return 0;
}
int initHistoryRows(List parameters) {
    List history = parameters.containsElementNamed("history") ? parameters["history"] : R_NilValue;
    if (history.size())
        return static_cast<int>(parameters["steps"]) / static_cast<int>(history["frequency"]);
    return 0;
}

Simulation::Simulation(List parameters)
    : STEPS(parameters["steps"])
    , RANDOM_SEED(parameters["random_seed"])
#ifdef NDEBUG
    , DEBUG(static_cast<bool>(parameters["debug"]))
#else
    , DEBUG(true)
#endif
    , PRINT_TIMING(static_cast<bool>(parameters["print_timing"]))
    , HISTORY_FREQ(initHistoryFreq(parameters))
    , HISTORY_ROWS(initHistoryRows(parameters))
    , columns(R_NilValue)
    , population(R_NilValue)
    , historyLog(List::create()){
    hazards = parameters["hazards"];
    trajectories = parameters["trajectories"];
    history = parameters.containsElementNamed("history") ? parameters["history"] : R_NilValue;
    // Extract configuration options
    if (this->RANDOM_SEED) {
        // R uses a global random state, however if a user provides a non-zero seed we will override that state
        Function set_seed("set.seed");
        set_seed(this->RANDOM_SEED );
    }
    if (DEBUG) {
        warning("golden's model debug checks are enabled, these may impact performance.");
    }
    // Create function timers
    for (const List h : hazards) {
        hazardTimers[h["name"]] = Timer();
        List transitions = h["transitions"];
        for (const List t : transitions)
            transitionTimers[t["name"]] = Timer();
    }
    for (const List t : trajectories)
        trajectoryTimers[t["name"]] = Timer();
    // Init History columns (difficult to do this in init list as requires assignment)
    if (history.size()) {
        columns = history["columns"];
    for (const List c : columns)
        columnTimers[c["name"]] = Timer();
    }
}

List Simulation::run(List initPop) {
    // Create a deep-copy of the initial pop, that we will return with changes at the end
    // @todo create other columns, to log last hazard score?
    population = clone(initPop);
    POP_SIZE = Rf_length(population[0]);
    
    // Reset timers
    for (auto &timer : hazardTimers)
        timer.second.resetDuration();
    for (auto &timer : transitionTimers)
        timer.second.resetDuration();
    for (auto &timer : trajectoryTimers)
        timer.second.resetDuration();
    for (auto &timer : columnTimers)
        timer.second.resetDuration();
        
    // Init History Data-table
    if (history.size()) {
        historyLog = List::create();
        IntegerVector stepv = IntegerVector(HISTORY_ROWS, 0);
        for (int i = 0; i < HISTORY_ROWS; ++i) {
            stepv[i] = (i * HISTORY_FREQ) + 1;
        }
        historyLog["~STEP"] = stepv;
    }
    
    // Simulation loop
    simTimer.start();
    for (step = 0; step < STEPS;) {
        stepHazards();
        stepTrajectories();
        stepHistory();
        // Increment step count
        ++step;
        // Calculate eta if running in an interactive console
        if (R_Interactive) {
            const float currentRuntime = simTimer.getRunningSeconds();
            const float avgStepTime = currentRuntime / step;
            const float remainingTime = avgStepTime * (STEPS - step);
            Rprintf("\rStep %d/%d complete, est. %.0fs remaining.", step, STEPS, remainingTime);
        }
    }
    simTimer.stop();
    Rprintf("\rSimulation Complete in %.2f seconds!      \n", simTimer.getElapsedSeconds());
    
    return buildOutput();
}

void Simulation::stepHazards() {
    int h_i = 1;  // 1 index'd similar to R
    // Execute hazards in order, if required at current step
    for (List hazard : hazards) {
        // If hazard is active this step
        const unsigned int freq = hazard["freq"];
        const int last = hazard["last"];
        const int first = hazard["first"];
        const int step1 = step + 1;
        if(step % freq == 0 && step1 <= last && step1 >= first) {
            // Build arg list to execute hazard chance
            List call_args = build_args(hazard["args"], population, step);
            // Execute hazard function and process results
            // p=1-exp(-h*dt)
            // p = probability
            // h = hazard result
            // dt = 1 (could vary in practice, but currently fixed)
            // if rng < p for an agent, then transition function is applied
            // hence p >= 1 is guaranteed, p > 1 potentially erroneous
            hazardTimers[hazard["name"]].start();
            NumericVector h = dynamic_call(hazard["fn"], call_args);
            hazardTimers[hazard["name"]].stop();
            const int dt = 1;
            NumericVector p = 1 - exp(-h * dt);
            NumericVector rng = runif(Rf_length(h));  // Generate vector of random float [0, 1)
            if (DEBUG)
                check_hazard_result(step, hazard["name"], h, Rf_length(population[0]));
            // Process hazard's transitions
            List transition_list = hazard["transitions"];
            int t_i = 1;  // 1 index'd similar to R
            for(List transition : transition_list) {
                // Build arg list to execute hazard transition
                call_args = build_args(transition["args"], population, step);
                // @todo can this be hidden inside a util function cleanly?
                // Only apply transitions in cases where hazard occurred
                // Execute transition
                transitionTimers[transition["name"]].start();
                SEXP _transition_result = dynamic_call(transition["fn"], call_args);
                transitionTimers[transition["name"]].stop();
                // Slightly different path, depending on whether it returns 1 or multiple properties
                if (Rf_length(transition["state"]) == 1) {
                    // Single return value (returned list should be a column)
                    // Select correct output based on output state type
                    String ts_name = transition["state"];
                    if (DEBUG)
                        check_length(step, transition["name"], _transition_result, Rf_length(population[0]));
                    if (Rf_isNumeric(population[ts_name])) {
                        NumericVector transition_result = _transition_result;
                        if (DEBUG)
                            check_result(step, transition["name"], transition_result, Rf_length(population[0]));
                        NumericVector transition_state = population[ts_name];
                        population[ts_name] = ifelse(rng < p, transition_result, transition_state);
                    } else if (Rf_isInteger(population[ts_name])) {
                        IntegerVector transition_result = _transition_result;
                        IntegerVector transition_state = population[ts_name];
                        population[ts_name] = ifelse(rng < p, transition_result, transition_state);
                    } else if (Rf_isLogical(population[ts_name])) {
                        LogicalVector transition_result = _transition_result;
                        LogicalVector transition_state = population[ts_name];
                        population[ts_name] = ifelse(rng < p, transition_result, transition_state);
                    } else {
                        // @todo Support other types?(Complex, String, Date, Datetime)
                        stop("Unsupported type at transition processing");
                    }
                } else {
                    // Multiple return values (returned list, should be a list of columns)
                    List transition_results = _transition_result;
                    CharacterVector transition_states = transition["state"];
                    if (Rf_length(transition_results) != Rf_length(transition_states))
                        stop("Transition function return value contains a different number of states than expected.");
                    for (int i = 0; i < Rf_length(transition_states); ++i) {
                        String ts_name = transition_states[i];
                        if (DEBUG)
                            check_length(step, transition["name"], transition_results[i], Rf_length(population[0]));
                        if (Rf_isNumeric(population[ts_name])) {
                            NumericVector transition_result = transition_results[i];
                            if (DEBUG)
                                check_result(step, transition["name"], transition_result, Rf_length(population[0]));
                            NumericVector transition_state = population[ts_name];
                            population[ts_name] = ifelse(rng < p, transition_result, transition_state);
                        } else if (Rf_isInteger(population[ts_name])) {
                            IntegerVector transition_result = transition_results[i];
                            IntegerVector transition_state = population[ts_name];
                            population[ts_name] = ifelse(rng < p, transition_result, transition_state);
                        } else if (Rf_isLogical(population[ts_name])) {
                            LogicalVector transition_result = transition_results[i];
                            LogicalVector transition_state = population[ts_name];
                            population[ts_name] = ifelse(rng < p, transition_result, transition_state);
                        } else {
                            // @todo Support other types?(Complex, String, Date, Datetime)
                            stop("Unsupported type at transition processing");
                        }
                    }
                }
                ++t_i;
            }
        }
        ++h_i;
    }
}
void Simulation::stepTrajectories() {
    int t_i = 1; // 1-indexed counter
    // Execute trajectories in order
    for (List trajectory : trajectories) {
        // Currently assumed that trajectories are always active
        // Build arg list to execute trajectory chance
        List call_args = build_args(trajectory["args"], population, step);
        // Execute trajectory function and store result directly in trajectory's property
        trajectoryTimers[trajectory["name"]].start();
        SEXP trajectory_result = dynamic_call(trajectory["fn"], call_args);
        trajectoryTimers[trajectory["name"]].stop();
        // Slightly different path, depending on whether it returns 1 or multiple properties
        if (Rf_length(trajectory["property"]) == 1) {
            // Single return value (returned list should be a column)
            String trajectory_prop = trajectory["property"];            
            population[trajectory_prop] = trajectory_result;
            if (DEBUG)
                check_result(step, trajectory["name"], population[trajectory_prop], Rf_length(population[0]));
        } else {
            // Multiple return values (returned list, should be a list of columns)
            List trajectory_returns = trajectory_result;
            CharacterVector trajectory_properties = trajectory["property"];
            if (Rf_length(trajectory_properties) != Rf_length(trajectory_returns))
                stop("Trajectory function return value contains a different number of properties than expected.");
            for (int i = 0; i < Rf_length(trajectory_properties); ++i) {
                String trajectory_prop = trajectory_properties[i];
                population[trajectory_prop] = trajectory_returns[i];
                if (DEBUG) // Possibly want to make naming of which return vector clearer
                    check_result(step, trajectory["name"], population[trajectory_prop], Rf_length(population[0]));
            }
        }
        t_i++;
    }
}
void Simulation::stepHistory() {
    if (history.size()) {
        // Is history desired this step
        if (step % HISTORY_FREQ == 0) {
            const unsigned int hist_i = step / HISTORY_FREQ;
            // Calculate and store result for each column of output
            for (List col : columns) {
                String name = col["name"];
                // Calculate filter vector if required by column
                columnTimers[col["name"]].start();
                LogicalVector filter_v = LogicalVector();
                if (col["filter_fn"] != R_NilValue) {
                    List call_args = build_args(col["filter_args"], population, step);
                    filter_v = dynamic_call(col["filter_fn"], call_args);
                    if (DEBUG)
                        check_result(step, col["name"], filter_v, Rf_length(population[0]));
                }
                // Columns may be filtered here
                List call_args = col["filter_fn"] != R_NilValue
                    ? build_args_filtered(col["args"], population, step, filter_v)
                    : build_args(col["args"], population, step);
                // Call the reduction function
                SEXP result = dynamic_call(col["fn"], call_args, true);
                columnTimers[col["name"]].stop();
                if (DEBUG)
                    check_result(step, col["name"], result, 1);
                if (hist_i == 0) {
                    // Detect the type of the result and create corresponding vector
                    if (Rf_isNumeric(result)) {
                        historyLog[name] = NumericVector(HISTORY_ROWS, 0.0);
                    } else if (Rf_isInteger(result)) {
                        historyLog[name] = IntegerVector(HISTORY_ROWS, 0);
                    } else if (Rf_isLogical(result)) {
                        historyLog[name] = LogicalVector(HISTORY_ROWS, false);
                    } else {
                        // @todo Support other types?(Complex, String, Date, Datetime)
                        stop("Unsupported type at history processing");
                    }
                }
                // Store result
                if (Rf_isNumeric(result)) {
                    NumericVector t = historyLog[name];
                    t[hist_i] = as<double>(result);
                } else if (Rf_isInteger(result)) {
                    IntegerVector t = historyLog[name];
                    t[hist_i] = as<int>(result);
                } else if (Rf_isLogical(result)) {
                    LogicalVector t = historyLog[name];
                    t[hist_i] = as<bool>(result);
                } else {
                    // @todo Support other types?(Complex, String, Date, Datetime)
                    stop("Unsupported type at history processing");
                }
            }
        }
    }
}

SEXP Simulation::dynamic_call(Function f, List args, bool is_history_column) {
    // Create a call object
    Language call(f);

    // Append all arguments dynamically
    for (R_xlen_t i = 0; i < args.size(); ++i) {
        call.push_back(args[i]);
        // @note To support named arguments a PairList(<name>, args[i]) would be pushed back here.
    }

    // Evaluate the call
    // @todo what happens if the call is stored and eval'd twice?
    SEXP ret = call.eval();
    if (Rf_length(ret) == 1 && !is_history_column) {
        // Function returning scalar, it's return value must be upgraded
        return promote_scalar(ret, POP_SIZE);
    }
    return ret;
}

List Simulation::buildOutput() {
    // Can only set as data.table once all columns have been added
    
    // Mark population as a data table
    population.attr("class") = CharacterVector::create("data.table", "data.frame");
    population.attr("row.names") = IntegerVector::create(NA_INTEGER, -Rf_length(population[0]));
    if (history.size()) {
        // Mark history as a data table
        // (This must be done after we're finished adding columns)
        historyLog.attr("class") = CharacterVector::create("data.table", "data.frame");
        historyLog.attr("row.names") = IntegerVector::create(NA_INTEGER, -HISTORY_ROWS);
    }
    // Package it into a structure for return
    List ret = List::create(
        _["pop"]   = population,
        _["history"] = historyLog,
        _["timing"] = buildTimingReport()
    );
    return ret;
}

List Simulation::buildTimingReport() {
    // Hazards
    CharacterVector hazard_name;
    NumericVector hazard_total, hazard_avg, hazard_pct;
    // Transitions
    CharacterVector transition_name;
    NumericVector transition_total, transition_avg, transition_pct;

    for (const List h : hazards) {
        std::string hname = as<std::string>(h["name"]);
        Timer& htimer = hazardTimers[hname];

        hazard_name.push_back(hname);
        hazard_total.push_back(htimer.getDurationSeconds());
        hazard_avg.push_back(htimer.getDurationSeconds() / htimer.getDurationCount());
        hazard_pct.push_back(htimer.getDurationSeconds() / simTimer.getDurationSeconds());
        
        List transitions = h["transitions"];
        for (const List t : transitions) {
            std::string tname = as<std::string>(t["name"]);
            Timer& ttimer = transitionTimers[tname];

            transition_name.push_back(tname);
            transition_total.push_back(ttimer.getDurationSeconds());
            transition_avg.push_back(ttimer.getDurationSeconds() / ttimer.getDurationCount());
            transition_pct.push_back(ttimer.getDurationSeconds() / simTimer.getDurationSeconds());
        }
    }
    DataFrame hazards_df = DataFrame::create(
        _["Hazard"] = hazard_name,
        _["Total Time (s)"] = hazard_total,
        _["Avg Time (s)"] = hazard_avg,
        _["% Runtime"] = hazard_pct
    );
    hazards_df.names() = CharacterVector::create(
        "Hazard", "Total Time (s)", "Avg Time (s)", "% Runtime");
    hazards_df.attr("class") = CharacterVector::create("data.table", "data.frame");
    hazards_df.attr("row.names") = IntegerVector::create(NA_INTEGER, -Rf_length(hazard_pct));
    DataFrame transitions_df = DataFrame::create(
        _["Transition"] = transition_name,
        _["Total Time (s)"] = transition_total,
        _["Avg Time (s)"] = transition_avg,
        _["% Runtime"] = transition_pct
    );
    transitions_df.names() = CharacterVector::create(
        "Transition", "Total Time (s)", "Avg Time (s)", "% Runtime");
    transitions_df.attr("class") =
        CharacterVector::create("data.table", "data.frame");
    transitions_df.attr("row.names") = IntegerVector::create(NA_INTEGER, -Rf_length(transition_pct));

    // Trajectories
    CharacterVector trajectory_name;
    NumericVector trajectory_total, trajectory_avg, trajectory_pct;
    
    for (const List t : trajectories) {
        std::string tname = as<std::string>(t["name"]);
        Timer& ttimer = trajectoryTimers[tname];

        trajectory_name.push_back(tname);
        trajectory_total.push_back(ttimer.getDurationSeconds());
        trajectory_avg.push_back(ttimer.getDurationSeconds() / ttimer.getDurationCount());
        trajectory_pct.push_back(ttimer.getDurationSeconds() / simTimer.getDurationSeconds());
    }
    
    DataFrame trajectories_df = DataFrame::create(
        _["Trajectory"] = trajectory_name,
        _["Total Time (s)"] = trajectory_total,
        _["Avg Time (s)"] = trajectory_avg,
        _["% Runtime"] = trajectory_pct
    );
    trajectories_df.names() = CharacterVector::create(
        "Trajectory", "Total Time (s)", "Avg Time (s)", "% Runtime");
    trajectories_df.attr("class") = CharacterVector::create("data.table", "data.frame");
    trajectories_df.attr("row.names") = IntegerVector::create(NA_INTEGER, -Rf_length(trajectory_pct));
    
    List ret = List::create(
        _["hazards"] = hazards_df,
        _["transitions"] = transitions_df,
        _["trajectories"] = trajectories_df
    );
    if (history.size()) {
        // Columns
        CharacterVector column_name;
        NumericVector column_total, column_avg, column_pct;
        
        List columns = history["columns"];

        for (const List c : columns) {
            std::string cname = as<std::string>(c["name"]);
            Timer& ctimer = columnTimers[cname];

            column_name.push_back(cname);
            column_total.push_back(ctimer.getDurationSeconds());
            column_avg.push_back(ctimer.getDurationSeconds() / ctimer.getDurationCount());
            column_pct.push_back(ctimer.getDurationSeconds() / simTimer.getDurationSeconds());
        }
        
        DataFrame columns_df = DataFrame::create(
            _["Column"] = column_name,
            _["Total Time (s)"] = column_total,
            _["Avg Time (s)"] = column_avg,
            _["% Runtime"] = column_pct
        );
        columns_df.names() = CharacterVector::create(
            "Column", "Total Time (s)", "Avg Time (s)", "% Runtime");
        columns_df.attr("class") = CharacterVector::create("data.table", "data.frame");
        columns_df.attr("row.names") = IntegerVector::create(NA_INTEGER, -Rf_length(column_pct));
        
        ret.push_back(columns_df, "columns");
    }
    // Setup as S3 class for the print method
    ret.attr("class") = CharacterVector::create("golden_timing");
    // Don't print a full timing report for quick runs
    if (simTimer.getDurationSeconds() > 1 && PRINT_TIMING) {
       print(ret);
    }
    return ret;
}
