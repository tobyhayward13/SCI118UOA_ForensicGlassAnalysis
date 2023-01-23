#' Calculate the significance threshold for Lambda.
#'
#' For a given significance value, this function uses critical values determined from simulated data formed on 1 million arrays, and returns the quantile estimated at
#' that significance level.
#' For values of k > 20, it assumes a chi squared distribution with k/(pi - 2) degrees of freedom.
#'
#' @param k Number of indices.
#' @param alpha Level of significance.
#'
#' @return A 100(1-alpha)\% quantile estimate from the distribution of Lambda.
#' @export calculate_lambda_threshold
#'
calculate_lambda_threshold <- function(k, alpha) {
  # If the array is of small size, use saved critical values.
  if (k <= 20) return (approx(seq(0, 1, 0.01), critical_values[,k-1], 1-alpha)$y)

  # Else. Assume Chi-Squared Distribution
  qchisq(1-alpha, k/(pi-2))
}

