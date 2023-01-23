#include <Rcpp.h>
#include <vector>

using namespace Rcpp;

// Helper functions
double vecMean(NumericVector vec);


//' Calculate B0
//'
//' Calculates the B0 value for a given numeric vector of values; assuming they're appropriate values corresponding to glass fragment refractive indices.
//'
//' @param arr vector of refractive indices.
//'
//' @return A numeric corresponding to the maximum between-sum-of-squares estimate from the sample.
//' @export find_B0
//'
// [[Rcpp::export]]
List find_B0(NumericVector arr) {
  int j = arr.size();
  int k = j;
  NumericVector B (k-1);

  double ov_avg = vecMean(arr);


  for (int ind = 0; ind < k - 1; ind++) {
    NumericVector g1 = arr[Range(0, ind)];
    NumericVector g2 = arr[Range(ind+1, (k-1))];

    B[ind] = (ind+1) * pow((vecMean(g1) - ov_avg), 2) + (k - ind - 1) * pow((vecMean(g2) - ov_avg), 2);
  }


  int B_0i = which_max(B); // Uses Rcpp::which_max() sugar function.
  double B_0 = B.at(B_0i);

  return List::create(_["x"] = B_0,
                      _["i"] = B_0i+1); // Add 1 for the sake of R
}




// Helper functions

double vecMean(NumericVector vec) {
  double added_val = 0.0;
  for (unsigned int i = 0; i < vec.size(); i++) {
    added_val += vec[i];
  }
  return added_val / vec.size();
}
