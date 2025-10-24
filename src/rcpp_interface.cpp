#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool is_odd(int d){
    return d % 2 != 0;
}
