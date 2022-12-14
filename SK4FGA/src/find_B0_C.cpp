#include <Rcpp.h>
#include <vector>

using namespace Rcpp;

double vecMean(std::vector<double> vec);
int whichMax(std::vector<double> vec);

// [[Rcpp::export]]
List find_B0_C(std::vector<double> arr) {
  int j = arr.size();
  int k = j;
  std::vector<double> B (k-1, 0);

  double ov_avg = vecMean(arr);


  for (int ind = 0; ind < k - 1; ind++) {
    std::vector<double> g1;
    std::vector<double> g2;
    g1.insert(g1.begin(), arr.begin(), arr.begin() + (ind+1));
    g2.insert(g2.begin(), arr.begin() + (ind+1), arr.begin() + k);

    B[ind] = ind * pow((vecMean(g1) - ov_avg), 2) + pow((k - ind) * (vecMean(g2) - ov_avg), 2);
  }

  int B_0i = whichMax(B);
  double B_0 = B.at(B_0i);

  return List::create(_["x"] = B_0,
                      _["i"] = B_0i+1); // Add 1 for the sake of R
}

//[[Rcpp::export]]
double vecMean(std::vector<double> vec) {
  double added_val = 0.0;
  for (unsigned int i = 0; i < vec.size(); i++) {
    added_val += vec.at(i);
  }
  return added_val / vec.size();
}

//[[Rcpp::export]]
int whichMax(std::vector<double> vec) {
  int max_i = 0;
  for (unsigned int i = 0; i < vec.size(); i++) {
    if (vec.at(i) > vec.at(max_i)) {
      max_i = i;
    }
  }
  return max_i;
}
