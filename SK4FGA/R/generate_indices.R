generate_indices <- function(n = 10, .sd_multi = 1){
  mu = 1.518
  sigma = 4e-5
  rnorm(n, mu, sigma * .sd_multi)
}
