#' Calculate T0 for a given list of glass fragments and features.
#'
#' Calculates the T0 value for a given list of data sets corresponding to glass fragment features
#' Assuming they're appropriate values corresponding to glass fragment features, and the first two columns are not features.
#'
#' @param data list ofglass fragment chemical (or otherwise) features.
#' @param i Starting element (default = 1)
#' @param j Ending element (default = length(array))
#'
#' @return A numeric corresponding to the maximum between-sum-of-squares estimate from the sample.
#' @export numeric
#'
find_T0 <- function(data, i = 1, j = length(data)){
  # This function takes an array and an interval and determines the B_0 value for that interval
  # of the array.

  # There is probably a better way to do this using vectorization, but for now will work.
  # Initialise a vector of T's
  k = j-i+1
  test.T = numeric(k-1)

  for (index in 1:(k-1)){
    g1 = data[1:index]
    g2 = data[(index+1):k]

    d1 = do.call(rbind, g1)[,-(1:2)]
    d2 = do.call(rbind, g2)[,-(1:2)]

    test.T[index] = find_T2(d1, d2)
  }

  T_0 = max(test.T)
  T_0.i = which.max(test.T)

  return(list(x = T_0, i = T_0.i))
}
