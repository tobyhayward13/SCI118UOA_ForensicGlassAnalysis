# This is my attempt at trying to optimise the B0 algorithm

library(SK4FGA)

find_B0_better <- function(array){
  # This function takes an array and an interval and determines the B_0 value for that interval
  # of the array.
  # This one is optimized with vector calculus.

  # There is probably a better way to do this using vectorization, but for now will work.
  k = length(array)

  # Calculate overall mean (mean.overall)
  mean.overall = mean(array)

  s1 = rep(1,k) %*% t(array)
  s2 = s1
  s1[row(s1) < col(s1)] = 0
  s2[row(s2) >= col(s2)] = 0

  B = 1:k * (apply(s1, 1, sum) / (1:k) - mean.overall)^2 + (k-1):0 * (apply(s2, 1, sum) / c((k-1):1, 1) - mean.overall)^2

  B_0 = max(B)
  B_0.i = which.max(B)

  return(list(x = B_0, i = B_0.i))
}

array = generate_indices(sample(1e4, 1))

system.time(find_B0(array))
system.time(find_B0_better(array))

# A lot slower and uses way more memory.













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
#' @examples None
#'
#'
partition2 <- function(array, alpha = 0.05, .debug = FALSE){
  # Partitions the array in to clusters by the SKM2 algorithm.
  # index ~ an array of refractive indices
  # alpha ~ threshold for split


  # SK splitting algorithm

  recursive_sk <- function(array, alpha, i = 1, j = length(array$x)){
    # This is the algorithm that actually creates the SK tree.

    # If the length of the array is 1, return array.
    if (length(array$x) == 1) return(array)

    # Find B_0
    B0 = find_B0_better(array$x) ######################

    # Find Lambda_*
    # Lambda variance
    S2 = 1.6e-9
    lambda = pi/(2*(pi-2)) * B0$x/S2

    # Calculate threshold
    threshold = calculate_lambda_threshold(k = length(array), alpha = alpha)

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
  return(
    list(
      groups = groups,
      tree = result
    )
  )
}







# Compare algorithms.

library(tidyverse)

# Generate test arrays

# generate_test_arrays <- function(max.size, max.scalevar, each = 10){
#   test_arrays = vector('list')
#   sizes = 2:max.size
#   variance_inc = 1:max.scalevar
#
#   for (s in sizes){
#     for (v in variance_inc){
#       for (e in 1:each){
#         array = generate_indices(s, v)
#         test_arrays = append(test_arrays, list(list(array, size = s, scalevar = v)))
#       }
#     }
#   }
#   test_arrays
# }
#
#
# test_arrays = generate_test_arrays(max.size = 12, max.scalevar = 6, each = 10)
# length(test_arrays)

# Perform time tests

# times1 = times2 = sizes = sd.scales = numeric(length(test_arrays))
#
# for (i in 1:length(test_arrays)){
#   print(sprintf('%d/%d Completed', i, length(test_arrays)))
#   print(test_arrays[i])
#   times1[i] = system.time(partition(test_arrays[[i]][[1]]))[3]
#   times2[i] = system.time(partition2(test_arrays[[i]][[1]]))[3]
#   sizes[i] = test_arrays[[i]]$size
#   sd.scales[i] = test_arrays[[i]]$scalevar
# }
#
#
# # Data write out.
# data = tibble(p1 = times1,
#               p2 = times2,
#               size = sizes,
#               sdscale = sd.scales) %>%
#   pivot_longer(cols = 1:2, names_to = 'partition_type', values_to = 'time')
#
# write_csv(data, 'partition_times.csv')




# Data import and vis
data = read_csv('partition_times.csv')

data %>%
  ggplot(aes(x = size, y = time, col = partition_type)) +
  geom_point() +
  geom_smooth()

data %>%
  group_by(size, sdscale, partition_type) %>%
  summarise(mean_time = mean(time)) %>%
  ggplot(aes(x = size, y = mean_time, fill = partition_type)) +
  geom_col(position = 'dodge') +
  facet_wrap(~sdscale)


explain_fit = lm(data = data,
                 time ~ size + sdscale + partition_type)

summary(explain_fit)

# No evidence to show that there is a difference in timings for the kind of data we would expect.







