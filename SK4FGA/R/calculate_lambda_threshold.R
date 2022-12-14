#' Empirically calculate a significance threshold for Lambda.
#'
#' For a given significance value, this function simulates the distribution of lambda in the context of glass-fragment refractive indices and returns the quantile at
#' that significance level.
#'
#' @param k Number of indices.
#' @param alpha Level of significance.
#' @param .nsims Number of simulated arrays. More simulations implies a closer estimated distribution of the true lambda distribution.
#'
#' @return An estimated 100(1-alpha)% quantile from the distribution of Lambda.
#' @export numeric
#'
calculate_lambda_threshold <- function(k, alpha, .nsims = 1e4) {
  X = matrix(generate_indices(.nsims * k), nrow = .nsims)
  Y = unlist(apply(X, 1, function(r) find_B0(r)$x))
  quantile(pi/(2*(pi-2)) * Y/1.6e-9, 1-alpha)
}

