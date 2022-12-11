#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double meanC(NumericVector x) {
  int n = x.size();
  double total = 0;

  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total / n;
}

// [[Rcpp::export]]
int helloworld() {
  std::cout << "Hello world!";
  return 0;
}

// [[Rcpp::export]]
double sumofsquares(NumericVector x) {
  int n = x.size();
  double total = 0;

  for(int i = 0; i < n; ++i) {
    total += x[i] * x[i];
  }
  return total;
}


/*** R

*/

