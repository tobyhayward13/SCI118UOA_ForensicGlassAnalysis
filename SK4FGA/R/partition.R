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
#' @return sk_partition_tree
#' @export partition
#'
#' @examples
#'
#' set.seed(123)
#' ris = generate_indices(8, 4)
#' part = partition(ris)
#' plot(part)
#' part$groups
#'
#'
partition <- function(array, alpha = 0.05, .debug = FALSE){
  # Partitions the array in to clusters by the SKM2 algorithm.
  # index ~ an array of refractive indices
  # alpha ~ threshold for split


  # SK splitting algorithm

  recursive_sk <- function(array, alpha, i = 1, j = length(array$x)){
    # This is the algorithm that actually creates the SK tree.

    # If the length of the array is 1, return array.
    if (length(array$x) == 1) return(array)

    # Find B_0
    B0 = find_B0(array$x)

    # Find Lambda_*
    # Lambda variance
    S2 = 1.6e-9
    lambda = pi/(2*(pi-2)) * B0$x/S2

    # Calculate threshold
    threshold = calculate_lambda_threshold(k = length(array$x), alpha = alpha)

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

  # Check for bad values and debugging
  if (alpha >= 1 | alpha <= 0) stop('alpha is not in (0,1)!')
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
  attr(part.tree, 'alg') = 'SKM2'

  return(part.tree)
}



#' ungroup.partition
#'
#' Ungroups the tree object in the output from partition()
#'
#' @param tree tree object returned from partition()
#'
#' @return A list object containing the indices of the
#' @export ungroup.partition
#'
ungroup.partition <- function(tree){

  groups = vector('list')

  # Initialise a stack
  a.stack = vector('list')
  # Append initial partition to stack
  a.stack = append(a.stack, list(tree))

  while (length(a.stack) > 0) {
    # Pop top of stack
    current = a.stack[[1]]
    a.stack = a.stack[-1]

    if (has.children(current)){
      a.stack = append(a.stack, list(current[[1]]))
      a.stack = append(a.stack, list(current[[2]]))
      next
    }

    # Otherwise append the group to the groups list.
    groups = append(groups, list(current))

  }

  groups
}

