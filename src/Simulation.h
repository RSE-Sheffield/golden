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
    
    void stepHazards();
    void stepTrajectories();
    void stepHistory();
    
    List buildOutput();
    
    Timer timerSim;
    std::map<std::string, Timer> HazardTimers;
    std::map<std::string, Timer> TrajectoryTimers;
    std::map<std::string, Timer> TransitionTimers;
    
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
