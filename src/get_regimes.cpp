#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector get_regimes(IntegerVector H) {

  	int n = H.size();
  
	for (int i = 1; i < n; ++i) {

		if (H[i] == -1) {
			H[i] = H[i-1];
		}
	}

	return(H);
}