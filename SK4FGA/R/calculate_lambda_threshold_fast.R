#' Empirically calculate a significance threshold for Lambda.
#'
#' For a given significance value, this function simulates the distribution of lambda in the context of glass-fragment refractive indices and returns the quantile at
#' that significance level.
#'
#' @param k Number of indices.
#' @param alpha Level of significance.
#'
#' @return An estimated 100(1-alpha)% quantile from the distribution of Lambda.
#' @export numeric
#'
calculate_lambda_threshold_fast <- function(k, alpha) {
  if (k > 20) return (calculate_lambda_threshold_C(k, alpha))
  Y = lambdas[,k-1]
  quantile(Y, 1-alpha)
}

