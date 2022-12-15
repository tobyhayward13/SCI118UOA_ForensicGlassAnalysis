#' Create Partitions of an RI array.
#'
#' Partitions the array of assumed glass fragment refractive indices into statistically
#' significant groups.
#'
#' @param array Vector of refractive indices.
#' @param alpha Significance parameter "[0,1]". Higher values are more likely to partition the
#' array further.
#' @param .debug Runs debugging.
#'
#' @return None
#' @export partition
#'
#' @examples
#'
#' ris = generate_indices()
#' partition(ris)
#'
#'
partition_C <- function(array, alpha = 0.05, .debug = FALSE){
  # Partitions the array in to clusters by the SKM2 algorithm.
  # index ~ an array of refractive indices
  # alpha ~ threshold for split


  # SK splitting algorithm

  recursive_sk <- function(array, alpha, i = 1, j = length(array$x)){
    # This is the algorithm that actually creates the SK tree.

    # If the length of the array is 1, return array.
    if (length(array$x) == 1) return(array)

    # Find B_0
    B0 = find_B0_C(array$x)

    # Find Lambda_*
    # Lambda variance
    S2 = 1.6e-9
    lambda = pi/(2*(pi-2)) * B0$x/S2

    # Calculate threshold
    threshold = calculate_lambda_threshold_C(k = length(array), alpha = alpha)

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

