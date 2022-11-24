calculate_lambda_threshold <- function(k, alpha, .nsims = 1e4) {
  X = matrix(generate_indices(.nsims * k), nrow = .nsims)
  Y = unlist(apply(X, 1, function(r) find_B0(r)$x))
  quantile(pi/(2*(pi-2)) * Y/1.6e-9, 1-alpha)
}

