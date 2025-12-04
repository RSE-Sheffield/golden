#ifndef _DYNAMIC_CALL_H
#define _DYNAMIC_CALL_H
/**
 * debug.h
 * Validation methods used throughout the code.
 */
#include <sstream>
#include <string>
#include <Rcpp.h>
using namespace Rcpp;

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
 * Utility which returns vector v with only elements with corresponding true element inside keep
 * 
 * @param v Input vector to be filtered, assumed type IntegerVector, NumericVector, LogicalVector or CharacterVector
 * @param keep Logical vector denoting elements of v to be retained
 * @return A copy of v with selected elements removed
 */
SEXP logical_filter(SEXP v, LogicalVector keep) {
    if (Rf_length(v) != keep.size()) {
        stop("Argument vector length mismatch in logical_filter()");
    }

    // Count length of output vector
    // To avoid dynamic resize
    // @todo this could be calculated once
    int out_len = 0;
    for (int i = 0; i < keep.size(); i++) {
        if (keep[i]) out_len++;
    }

    // Special case per type
    int j = 0;
    if (Rf_isNumeric(v)) {
        NumericVector typed_v(v);
        NumericVector out(out_len);
        for (int i = 0; i < keep.size(); ++i)
            if (keep[i]) out[j++] = typed_v[i];
        return out;
    } else if(Rf_isInteger(v)) {
        IntegerVector typed_v(v);
        IntegerVector out(out_len);
        for (int i = 0; i < keep.size(); ++i)
            if (keep[i]) out[j++] = typed_v[i];
        return out;
    } else if(Rf_isLogical(v)) {
        LogicalVector typed_v(v);
        LogicalVector out(out_len);
        for (int i = 0; i < keep.size(); ++i)
            if (keep[i]) out[j++] = typed_v[i];
        return out;
    } else if(Rf_isString(v)) {
        CharacterVector typed_v(v);
        CharacterVector out(out_len);
        for (int i = 0; i < keep.size(); ++i)
            if (keep[i]) out[j++] = typed_v[i];
        return out;
    } else {
        stop("Unsupported SEXP type in logical_filter()");
    }
}

/**
 * Utility function covering the common task of building up the call arguments to be passed to dynamic_call()
 *
 * @param p String vector of column names and ~SPECIAL values
 * @param table Data table which columns will be taken from
 * @param current_step Value to provided for "~STEP"
 * @return List containing R objects and variables to be passed to dynamic_call()
 */
List build_args(StringVector p, List table, int current_step) {
    List call_args;
    for (const String arg:p) {
        const std::string arg_string = arg.get_cstring();
        if (arg_string.length() > 1 && arg_string[0] == '~') {
            // Map a special arg
            if (arg_string == "~STEP") {
                // Special arg corresponds to the step at runtime
                call_args.push_back(current_step);
            } else {
                // This should never be hit
                std::stringstream err;
                err << "Special arg '" << arg_string <<"' not yet implemented in build_args().\n";
                throw std::runtime_error(err.str());
            }
        } else {
            call_args.push_back(table[arg]);
        }
    }
    return call_args;
}

/**
 * Utility function covering the common task of building up the call arguments to be passed to dynamic_call()
 * This version applies logical_filter() to columns included in the return value with filter_v
 * @param filter_v LogicalVector passed to keep parameter of logical_filter
 * @see build_args()
 */
List build_args_filtered(StringVector p, List table, int current_step, LogicalVector filter_v) {
    List call_args;
    for (const String arg:p) {
        const std::string arg_string = arg.get_cstring();
        if (arg_string.length() > 1 && arg_string[0] == '~') {
            // Map a special arg
            if (arg_string == "~STEP") {
                // Special arg corresponds to the step at runtime
                call_args.push_back(current_step);
            } else {
                // This should never be hit
                std::stringstream err;
                err << "Special arg '" << arg_string <<"' not yet implemented in build_args().\n";
                throw std::runtime_error(err.str());
            }
        } else {
            call_args.push_back(logical_filter(table[arg], filter_v));
        }
    }
    return call_args;
}


#endif  // _DYNAMIC_CALL_H
