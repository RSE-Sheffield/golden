#include <Rcpp.h>
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