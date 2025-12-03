#ifndef _DEBUG_H
#define _DEBUG_H
/**
 * debug.h
 * Validation methods used throughout the code.
 */
#include <sstream>
#include <Rcpp.h>
using namespace Rcpp;
 
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

#endif  // _DEBUG_H
