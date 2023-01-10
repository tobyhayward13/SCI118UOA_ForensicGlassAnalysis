#' Create Partitions of a multivariate array of objects.
#'
#' Partitions the array of assumed glass fragment chemical compositions and features into statistically
#' significant groups.
#'
#' @param data A list of data.frames or matrices corresponding to individual observations of glass fragment features.
#' @param alpha Significance parameter "[0,1]". Higher values are more likely to partition the
#' array further.
#' @param .debug Runs debugging.
#'
#' @return A list of groupings and the tree formed.
#' @export partition.multi
#'
partition.multi <- function(data, alpha = 0.05, .debug = FALSE){

  recursive_part <- function(data, alpha, i = 1, j = length(data)){
    # This is the algorithm that actually creates the SK tree.

    # If the length of the array is 1, return array.
    if (length(data) == 1) return(list(data = do.call(rbind, data), ix = names(data)))

    # Find T_0
    # Returns both T^2 value and index where it was found.
    T0 = find_T0(data)

    # Calculate significance
    n1 = nrow(do.call(rbind, data[1:T0$i]))
    n2 = nrow(do.call(rbind, data[(T0$i+1):length(data)]))
    p = ncol(data[[1]]) - 2

    pvalue = ptsquared(T0$x, n1, n2, p)

    # If p value is significant to the 100(alpha) level, split.
    if (pvalue < alpha) {
      return(
        list(
          recursive_part(data[1:T0$i], alpha),
          recursive_part(data[(T0$i+1):length(data)], alpha)
        )
      )
    }
    # Else return the array
    return(list(data = do.call(rbind, data), ix = names(data)))
  }


  # Check for bad values and debugging
  if (alpha >= 1 | alpha <= 0) stop('alpha is not in (0,1)!')
  if (.debug) debug(recursive_part)

  # Sort the array by Euclidean distance to the mean.
  data = order_euclid(data)

  # Main Call
  # Recursive algorithm call
  result = recursive_part(data, alpha)

  # Else, return array of corresponding groups
  groups = numeric(length(data))
  # Groups index correspond to position of data in list of data.
  item.names = names(data)

  untreed.groups = ungroup.partition(result)
  for (i in 1:length(untreed.groups)){
    for (name in untreed.groups[[i]]$ix) {
      j = which(item.names == name)
      groups[j] = i
    }
  }

  part.tree = list(
    groups = groups,
    tree = result)
  class(part.tree) = 'sk_partition_tree'

  return(part.tree)
}







#' Order a list of data frames containing numerical columns by their euclidean distance to the mean.
#'
#' Meant for internal use only.
#'
#' @param alist A list of data frames.
#'
#' @return A list of data frames.
#'
order_euclid <- function(alist){
  # Sorts a list of split dataframes as outputted by the prepare_data() function.
  # Calculate overall mean for each group.
  all.data = do.call(rbind, alist)
  mid = apply(all.data, 2, mean)

  # Average values for each feature within those in list
  points = lapply(alist, function(p) apply(p, 2, mean))

  # Calculate distances
  points.d = sapply(points, function(v) sum((v - mid)^2))

  # Return data in order of smallest euclidean distance to largest.
  indices = sort(points.d, index.return = T)$ix
  alist[indices]
}


