#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


// [[Rcpp::export]]
std::vector<int> zeros(int N) {
  std::vector<int> X(N);
  return X;
}

// [[Rcpp::export]]
NumericVector rep_len_sugar(NumericVector a, int b) {
  NumericVector out = rep_len(a, b);
  return out;  
}

// [[Rcpp::export]]
NumericVector residuals(List mod) {
  NumericVector resids = mod["residuals"];
  return resids;
}

// [[Rcpp::export]]
List components(List mod) {
  NumericVector resids = as<NumericVector>(mod["residuals"]);
  NumericVector coefs = as<NumericVector>(mod["coefficients"]);
  NumericVector fitted = as<NumericVector>(mod["fitted.values"]);
  List out = List::create(
    Named("Residuals") = resids,
    Named("Coeficients") = coefs,
    Named("Fitted Values") = fitted
  );
    return out;
}


double f(double x) { 
  return std::pow(x, 2); 
}

// [[Rcpp::export]]
std::vector<double> square(std::vector<double> x) {
  std::transform(x.begin(), x.end(), x.begin(), f);
  return x;
}

// [[Rcpp::export]]
NumericVector sort_Cpp(NumericVector y) { 
  std::sort(y.begin(), y.end());
  return y;
} 

// [[Rcpp::export]]
NumericVector nth_partial_sort(NumericVector x, int nth) {
  NumericVector y = clone(x);
  std::nth_element(y.begin(), y.begin()+nth, y.end());
  std::sort(y.begin(), y.begin()+nth);
  return y;
}

// [[Rcpp::export]]
double pi_Sugar(const int N) {
  RNGScope scope; // ensure RNG gets set/reset
  NumericVector x = runif(N);
  NumericVector y = runif(N);
  NumericVector d = sqrt(x*x + y*y);
  return 4.0 * sum(d <= 1.0) / N;
}

// [[Rcpp::export]]
arma::vec getEigenValues(arma::mat M) {
  return arma::eig_sym(M);
}

// [[Rcpp::export]]
void test_reference(NumericVector A) {
  
  NumericVector B = A;
  
  Rcout << "Before: " << std::endl << "A: " << A << std::endl << "B: " << B << std::endl; 
  
  A[1] = 5; // 2 -> 5
  
  Rcout << "After: " << std::endl << "A: " << A << std::endl << "B: " << B << std::endl; 
}

// [[Rcpp::export]]
void test_copy(NumericVector A) {
  
  NumericVector B = clone(A);
  
  Rcout << "Before: " << std::endl << "A: " << A << std::endl << "B: " << B << std::endl; 
  
  A[1] = 5; // 2 -> 5
  
  Rcout << "After: " << std::endl << "A: " << A << std::endl << "B: " << B << std::endl; 
}

// [[Rcpp::export]]
void implicit_ref(const Rcpp::NumericVector& X) {
  NumericVector Y(X);
  Y = Y + 1.0;
}
// [[Rcpp::export]]
void test(double& A) {
  double B = A; //B is a copy of A
  Rcout << "Before: " << std::endl << "A: " << A << std::endl << "B: " << B << std::endl; 
  A = 1.0;
  Rcout << "After: " << std::endl << "A: " << A << std::endl << "B: " << B << std::endl; 
}

// [[Rcpp::export]]
NumericVector foo(NumericVector x, NumericVector y) {
  return Rcpp::wrap( ifelse( x < y, x*x, -(y*y) )) ;
}

// [[Rcpp::export]]
double inner_prod_rcpp(NumericVector x, NumericVector y) {
  int K = x.length();
  double ip = 0;
  for (int k = 0; k < K; k++) {
    ip += x(k) * y(k);
  }
  return(ip);
}

// [[Rcpp::export]]
double inner_prod_rcpparma(arma::vec x, arma::vec y) {
  arma::mat out = x.t() * y;
  return(out(0));
}

// [[Rcpp::export]]
List fastLm(const arma::mat& X, const arma::colvec& y) {
  int n = X.n_rows, k = X.n_cols;
  
  arma::colvec coef = arma::solve(X, y);
  arma::colvec res  = y-X*coef;
  
  return List::create(Named("coefficients") = coef);
}

// [[Rcpp::export]]
arma::mat VARSim_Rcpp(arma::mat& A, arma::mat& epsilon) {
  int m = epsilon.n_rows; int n = epsilon.n_cols;
  arma::mat out(m, n, arma::fill::zeros);
  for (int i=1; i<m; i++) {
    out.row(i) = out.row(i-1) * A.t() + epsilon.row(i);
  }
  return out;
}



