#' Calculate B0 for a given array of Refractive Indices.
#'
#' Calculates the B0 value for a given numeric vector of values; assuming they're appropriate values corresponding to glass fragment refractive indices.
#'
#' @param array vector of refractive indices.
#' @param i Starting element (default = 1)
#' @param j Ending element (default = length(array))
#'
#' @return A numeric corresponding to the maximum between-sum-of-squares estimate from the sample.
#' @export numeric
#'
find_B0 <- function(array, i = 1, j = length(array)){
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
