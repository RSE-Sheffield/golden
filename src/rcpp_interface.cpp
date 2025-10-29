#include <Rcpp.h>
#include <random>
#include <stdexcept>
#include <sstream>
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



// [[Rcpp::export]]
List run_simulation(List initPop, List parms) {
    // Load parameters, or use default if not present
    const int STEPS = parms.containsElementNamed("steps") ? parms["steps"] : 1;
    const uint64_t RANDOM_SEED = parms.containsElementNamed("random_seed") ? static_cast<uint64_t>(parms["random_seed"]) : 2999569345;
    
    // Validate initPop has columns required by hazard functions
    if (!parms.containsElementNamed("hazards"))
        throw std::invalid_argument("List 'parms' expected to contain vector 'hazards'\n");
    List hazards = parms["hazards"];
    for (int h_i = 0; h_i < hazards.size(); ++h_i) {
        List h = hazards[h_i];
        // Check hazard actually contains 'parms'
        if (!h.containsElementNamed("parms")) {
            std::stringstream err;
            err << "Hazard[" << h_i <<"] expected to contain vector 'parms'\n";
            throw std::invalid_argument(err.str());
        }
        // Check hazard actually contains 'fn'
        if (!h.containsElementNamed("fn")) {
            std::stringstream err;
            err << "Hazard[" << h_i <<"] expected to contain function 'fn'\n";
            throw std::invalid_argument(err.str());
        }
        StringVector hazardParms = h["parms"];
        // Check that listed parameters are found inside initPop columns
        for (const auto &hp : hazardParms) {
            if(!initPop.containsElementNamed(hp)) {
                std::stringstream err;
                err << "Columns '" << hp <<"' expected inside data.table 'initPop'\n";
                throw std::invalid_argument(err.str());
            }
        }
    }
    
    // Init R Rng
    std::mt19937_64 rng(RANDOM_SEED);
    
    // Simulation loop
    for (int i = 0; i< STEPS; ++i) {
        // Execute hazards in order, if required at current step
        for (List hazard : hazards) {
            // If hazard is active this step
            const unsigned int freq = hazard.containsElementNamed("freq") ? hazard["freq"] : 1;
            if(i % freq == 0) {
                // Execute hazard functions
                StringVector p = hazard["parms"];
                std::string p_0 = as<std::string>(p[0]);
                NumericVector c_0 = initPop[p_0];  // This may be making a copy of the vector, as changes to c_0 are not reflected in initPop[p_0]
                Function fn = hazard["fn"];
                initPop[p_0] = fn(c_0); // Have to explicitly store column back
                Rcout << as<NumericVector>(initPop[p_0]) << "\n";
            }
        }
    }
    return List();
}