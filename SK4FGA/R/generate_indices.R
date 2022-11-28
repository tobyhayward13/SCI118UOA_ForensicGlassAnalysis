#' Generate a test set of Glass-Fragment Refractive indices.
#'
#' Returns a vector of randomly generated refractive indices from a expected normal distribution of glass fragments.
#'
#' @param n Number of refractive indices to generate.
#' @param .sd_multi Scale factor of the standard deviation. Greater values imply more variance in the random sample.
#'
#' @return A vector of randomly generated RIs.
#' @export numeric
#'
#' @examples
#'
#' test_ris = generate_indices(8)
#' partition(test_ris)
#'
#' test_ris_varied = generate_indices(.sd_multi = 5) # More variance will give a higher chance of splitting in the algorithm.
#' partition(test_ris)
#'
generate_indices <- function(n = 10, .sd_multi = 1){
  mu = 1.518
  sigma = 4e-5
  rnorm(n, mu, sigma * .sd_multi)
}
