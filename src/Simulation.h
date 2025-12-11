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
    const int HISTORY_FREQ;
    const int HISTORY_ROWS;
    // Param
    List hazards;
    List trajectories;
    List history;
    List columns;
    // Runtime data
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
     * @parameters Instance of R S3 class eldoradosim_parameters, which defines the simulation structure
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
