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
    // Debug testing
    printf("i_i: %d, N: %d\n", (int)i_i, (int)N);
    
    return List::create(
        _["male"] = out_male,
        _["age"] = out_age,
        _["dead"] = IntegerVector(N, 0) // zero init, everyone begins alive
    );
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