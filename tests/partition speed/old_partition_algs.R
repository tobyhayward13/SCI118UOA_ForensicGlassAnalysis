# Source file for old developed algorithms

library(SK4FGA)
library(Rcpp)




partition_oldR <- function(array, alpha = 0.05, .debug = FALSE){
  # Partitions the array in to clusters by the SKM2 algorithm.
  # index ~ an array of refractive indices
  # alpha ~ threshold for split


  # SK splitting algorithm

  recursive_sk <- function(array, alpha, i = 1, j = length(array$x)){
    # This is the algorithm that actually creates the SK tree.

    # If the length of the array is 1, return array.
    if (length(array$x) == 1) return(array)

    # Find B_0
    B0 = find_B0_oldR(array$x)

    # Find Lambda_*
    # Lambda variance
    S2 = 1.6e-9
    lambda = pi/(2*(pi-2)) * B0$x/S2

    # Calculate threshold
    threshold = calculate_lambda_threshold_oldR(k = length(array$x), alpha = alpha)

    # If lambda* in lambda* distribution at 100(1-alpha) percentile, split.
    if (lambda > threshold) { # Typo within thesis (> not <).
      return(
        list(
          recursive_sk(list(x = array$x[1:B0$i], ix = array$ix[1:B0$i]), alpha),
          recursive_sk(list(x = array$x[(B0$i+1):length(array$x)], ix = array$ix[(B0$i+1):length(array$x)]), alpha)
        )
      )
    }
    # Else return the array
    return(array)
  }


  if (.debug) debug(recursive_sk)

  # Main Call

  # Pre-sort the array and save indices.
  array = sort(array, index.return = T)


  # Recursive algorithm call
  result = recursive_sk(array, alpha)


  # Else, return array of corresponding groups
  groups = numeric(length(array$x))

  untreed.groups = ungroup.partition(result)
  for (i in 1:length(untreed.groups)){
    for (j in untreed.groups[[i]]$ix) groups[j] = i
  }

  part.tree = list(
    groups = groups,
    tree = result)
  class(part.tree) = 'sk_partition_tree'

  return(part.tree)
}

calculate_lambda_threshold_oldR <- function(k, alpha, .nsims = 1e4) {
  X = matrix(generate_indices(.nsims * k), nrow = .nsims)
  # Sort each row.
  X = t(apply(X, 1, sort))
  Y = unlist(apply(X, 1, function(r) find_B0_oldR(r)$x))
  quantile(pi/(2*(pi-2)) * Y/(1.6e-9), 1-alpha)
}

find_B0_oldR <- function(array, i = 1, j = length(array)){
  # This function takes an array and an interval and determines the B_0 value for that interval
  # of the array.

  # There is probably a better way to do this using vectorization, but for now will work.
  # Initialise a vector of B's
  k = j-i+1
  B = numeric(k-1)

  # Calculate overall mean (mean.overall)
  mean.overall = mean(array)

  for (index in 1:(k-1)){
    g1 = array[1:index]
    g2 = array[(index+1):k]

    B[index] = index * (mean(g1) - mean.overall)^2 + (k-index) * (mean(g2) - mean.overall)^2
  }

  B_0 = max(B)
  B_0.i = which.max(B)

  return(list(x = B_0, i = B_0.i))
}







partition_C <- function(array, alpha = 0.05, .debug = FALSE){
  # Partitions the array in to clusters by the SKM2 algorithm.
  # index ~ an array of refractive indices
  # alpha ~ threshold for split


  # SK splitting algorithm

  recursive_sk <- function(array, alpha, i = 1, j = length(array$x)){
    # This is the algorithm that actually creates the SK tree.

    # If the length of the array is 1, return array.
    if (length(array$x) == 1) return(array)

    # Find B_0 (uses c++ version in SK4FGA)
    B0 = find_B0(array$x)

    # Find Lambda_*
    # Lambda variance
    S2 = 1.6e-9
    lambda = pi/(2*(pi-2)) * B0$x/S2

    # Calculate threshold
    threshold = calculate_lambda_threshold_C(k = length(array$x), alpha = alpha)

    # If lambda* in lambda* distribution at 100(1-alpha) percentile, split.
    if (lambda > threshold) { # Typo within thesis (> not <).
      return(
        list(
          recursive_sk(list(x = array$x[1:B0$i], ix = array$ix[1:B0$i]), alpha),
          recursive_sk(list(x = array$x[(B0$i+1):length(array$x)], ix = array$ix[(B0$i+1):length(array$x)]), alpha)
        )
      )
    }
    # Else return the array
    return(array)
  }


  if (.debug) debug(recursive_sk)

  # Main Call

  # Pre-sort the array and save indices.
  array = sort(array, index.return = T)


  # Recursive algorithm call
  result = recursive_sk(array, alpha)


  # Else, return array of corresponding groups
  groups = numeric(length(array$x))

  untreed.groups = ungroup.partition(result)
  for (i in 1:length(untreed.groups)){
    for (j in untreed.groups[[i]]$ix) groups[j] = i
  }

  part.tree = list(
    groups = groups,
    tree = result)
  class(part.tree) = 'sk_partition_tree'

  return(part.tree)
}


calculate_lambda_threshold_C <- function(k, alpha, .nsims = 1e4) {
  X = matrix(generate_indices(.nsims * k), nrow = .nsims)
  # Sort each row.
  X = t(apply(X, 1, sort))
  Y = unlist(apply(X, 1, function(r) find_B0(r)$x))
  quantile(pi/(2*(pi-2)) * Y/(1.6e-9), 1-alpha)
}


