#' Calculate Hotelling's T^2 Statistic for two independent samples.
#'
#' @param d1 matrix or data.frame type object containing the multivariate data for the first sample.
#' @param d2 matrix or data.frame type object containing the multivariate data for the second sample.
#'
#' @return T^2 value for the two objects.
#' @export find_T2
#'
find_T2 <- function(d1, d2){
  # This is a function which takes two data.frames as input and returns the T^2 statistic.
  # Ensure they have the same number of columns
  if (ncol(d1) != ncol(d2)) stop('Incompatible number of columnns!')

  # Get necessary constants
  d1.means = apply(d1, 2, mean)
  d2.means = apply(d2, 2, mean)
  d1.n = nrow(d1)
  d2.n = nrow(d2)
  npar = ncol(d1)

  # Mahalanobis Distance Squared

  # Pooled within groups cov matrix

  d1.cov = cov(d1)
  d2.cov = cov(d2)

  S = ((d1.n-1)*d1.cov + (d2.n-1)*d2.cov) / (d1.n + d2.n - 2)
  S_inv = solve(S)

  #

  md_squared =
    t(d1.means - d2.means) %*%
    S_inv %*%
    (d1.means - d2.means)

  # T^2 statistic
  as.numeric((d1.n * d2.n) / (d1.n + d2.n) * md_squared)
}


#' Calculate the Probability for a given T^2 statistic.
#'
#' @param t T^2 statistic.
#' @param n1 Number of observations in first sample.
#' @param n2 Number of observations in second sample.
#' @param p Number of parameters.
#'
#' @return A probability.
#' @export ptsquared
#'
ptsquared <- function(t, n1, n2, p){
  f = (n1 + n2 - p - 1) / (p * (n1 + n2 - 2)) * t

  df1 = p
  df2 = n1 + n2 - p - 1

  # Probability
  pf(f, df1, df2, lower.tail = F)
}

