#ifndef SIMULATION_H_
#define SIMULATION_H_

#include <map>
#include <set>
#include <string>

#include <Rcpp.h>
using namespace Rcpp;

#include "utils/Timer.h"

class Simulation {
    static const std::set<std::string> SPECIAL_ARGS;
    const int STEPS;
    const int RANDOM_SEED;
    const bool DEBUG;
    const bool PRINT_TIMING;
    const int HISTORY_FREQ;
    const int HISTORY_ROWS;
    // Param
    List hazards;
    List trajectories;
    List history;
    List columns;
    // Runtime data
    int POP_SIZE = 0; // Set by run(), never updated
    List population;
    List historyLog;
    int step = 0; // Simulation iteration index (0-indexed)
    // Timers
    Timer simTimer;
    std::map<std::string, Timer> hazardTimers;
    std::map<std::string, Timer> transitionTimers;
    std::map<std::string, Timer> trajectoryTimers;
    std::map<std::string, Timer> columnTimers;
    
    /**
     * Execute all hazards (and child transitions) which are active for the current step
     */
    void stepHazards();
    /**
     * Execute all trajectories
     */
    void stepTrajectories();
    /**
     * Execute all columns active for the current step
     */
    void stepHistory();
    /**
     * Dynamically call an R function from cpp
     *
     * R functions may require any number of arguments
     * The RCPP Function::operator() cannot be called in a varadic manner
     * (e.g. dynamically changing the number of arguments at runtime)
     * Hence this method instead acts as a wrapper, whereby a vector of arguments can be passed to the function
     * @param f The R function to be called
     * @param args A List of arguments to be passed.
     * @param is_history_column Prevents scalar result being upgraded
     * @note If the return value is scalar, it will be upgraded to a vector of the appropriate length
     */
    SEXP dynamic_call(Function f, List args, bool is_history_column = false);
    
    /**
     * Construct the structure to be returned by run()
     */
    List buildOutput();
    /**
     * Construct a nested list of each function's runtime
     */
    List buildTimingReport();
    /**
     * Crude method for printing the result of buildTimingReport() to console
     * @param timing List returned by buildTimingReport()
     */
    void printTimingReport(List timing);
    
  public:
    /**
     * Initialise a simulation model
     * @parameters Instance of R S3 class golden_parameters, which defines the simulation structure
     */
    Simulation(List parameters);
    /**
     * Run the simulation with the provided initial population
     * @param initPop
     * @return Either data.table containing final population, or list containing $pop the same data.table and $history the collected history.
     */
    List run(List initPop);
};

#endif  // SIMULATION_H_
