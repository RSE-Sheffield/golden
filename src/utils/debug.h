#ifndef _DEBUG_H
#define _DEBUG_H
/**
 * debug.h
 * Validation methods used throughout the code.
 */
#include <sstream>
#include <Rcpp.h>
using namespace Rcpp;

void check_length(int s_i, const std::string &name, SEXP _result, const int expected_length) {
    if (Rf_length(_result) != expected_length) {
        std::stringstream err;
        err << "[DEBUG]During step " << s_i << " " << name;
        err << "return had wrong length " << Rf_length(_result) <<", expected length " << expected_length << "\n";
        throw std::runtime_error(err.str());
    }
}
/**
 * Debug method to check output of dynamic methods, to ensure they don't contain NaN/Inf
 * 
 * @param s_i Step index
 * @param name String representation of the function type (e.g. hazard[[1]$transitions[[2]])
 * @param _result The result vector to be checked, assumed to be NumericVector (only floating point types have NaN)
 * @param expected_length The length the result vector should be (or 1 if not a vector)
 * @note Not sure R has a concept of Inf, everything so far seems to be NA, which I assume to be NaN
 */
void check_result(int s_i, const std::string &name, SEXP _result, const int expected_length) {
    check_length(s_i, name, _result, expected_length);
    if (Rf_isNumeric(_result)) {
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
            err << "[DEBUG]During step " << s_i << " " << name;
            err << " return contained " << nan_count <<" NaN values and " << inf_count << " Inf values.\n";
            throw std::runtime_error(err.str());
        }
    }
}
/**
 * Extended variant with additional hazard only checks
 * @see check_result(int, const std::string&, SEXP)
 */
void check_hazard_result(int s_i, const std::string &name, SEXP _result, const int expected_length) {
    check_length(s_i, name, _result, expected_length);
    if (Rf_isNumeric(_result)) {
        NumericVector result = _result;
        // Positive Inf is permitted
        if (Rf_isNumeric(_result)) {
            NumericVector result = _result;
            int nan_count = 0;
            for (const double &t : result) {
                nan_count += std::isnan(t)?1:0;
            }
            if (nan_count) {
                // This should never be hit
                std::stringstream err;
                err << "[DEBUG]During step " << s_i << " " << name;
                err << " return contained " << nan_count <<" NaN values\n";
                throw std::runtime_error(err.str());
            }
        }
        int negative_range_count = 0;
        int high_range_count = 0;
        for (const double &t : result) {
            if (t < 0) {
                ++negative_range_count;
            } else if (t > 1 && !std::isinf(t)) {
                ++high_range_count;
            }
        }
        if (negative_range_count) {
            // Special case, warn if hazard return is <0
            std::stringstream err;
            err << "[DEBUG]During step " << s_i << " " << name;
            err << " return contained " << negative_range_count <<" negative values.\n";
            err << "Hazards are expected to return values greater than or equal to 0\n";
            warning(err.str());
        }
        if (high_range_count) {
            // Special case, warn if hazard return is <0
            std::stringstream err;
            err << "[DEBUG]During step " << s_i << " " << name;
            err << " return contained " << high_range_count <<" values > 1.\n";
            err << "Frequent hazard returns exceeding 1 may indicate steps are too long.\n";
            err << "Consider reparametrizing so the implicit step size is shorter.\n";
            warning(err.str());
        }
    } else {
        // This should never be hit
        std::stringstream err;
        err << "[DEBUG]During step " << s_i << " " << name;
        err << " return was type " << type2name(_result) << " expected type double\n";
        throw std::runtime_error(err.str());
    }
}


#endif  // _DEBUG_H
