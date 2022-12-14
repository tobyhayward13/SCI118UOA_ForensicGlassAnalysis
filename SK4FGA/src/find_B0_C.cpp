#include <Rcpp.h>
#include <vector>

using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
std::pair<double, int> find_B0_C(std::vector<double> arr) {
  #include <vector>
  int j = arr.size();
  int k = j;
  vector<double> B(k-1, 0);

  double ov_avg = vecMean(arr);


  for (int ind = 0; ind < k - 1; ind++) {
    vector<double> g1;
    vector<double> g2;
    g1.insert(g1.begin(), arr.begin(), arr.begin() + (ind+1));
    g2.insert(g2.begin(), arr.begin() + (ind+1), arr.begin() + k);

    B[ind] = ind * pow((vecMean(g1) - ov_avg), 2) + pow((k - ind) * (vecMean(g2) - ov_avg), 2);
  }
  int B_0i = whichMax(B);
  double B_0 = B.at(B_0i);
  std::pair <double, int> result(B_0, B_0i);
  for (int i = 0; i < B.size(); i++) {
    cout << "B" << i+1 << ": " << B.at(i) << "\n";
  }
  return result;
}

//[[Rcpp::export]]
double vecMean(std::vector<double> vec) {
  double added_val = 0.0;
  for (int i = 0; i < vec.size(); i++) {
    added_val += vec.at(i);
  }
  return added_val / vec.size();
}

//[[Rcpp::export]]
int whichMax(std::vector<double> vec) {
  int max_i = 0;
  for (int i = 0; i < vec.size(); i++) {
    if (vec.at(i) > vec.at(max_i)) {
      max_i = i;
    }
  }
  return max_i;
}
