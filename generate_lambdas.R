# GENERATE LAMBDAS
# Author: Toby Hayward

# This is the script used to generate a dataset lambdas for the purpose of forensic glass analysis within the SK4FGA package.
# It is the same code that was used to interpolate the lambda critical values in the package.


# Sizes of arrays to estimate lambda
k = 2:20
# Number of arrays for each size.
n = 1e6
# Variance of an array of RI's under the null hypothesis.
S2 = 1.6e-9

lambda = matrix(0, nrow = n, ncol = length(k))

start.time = Sys.time()

for (test.k in k) {
  # Debugging
  print(paste('Test:', test.k))
  print(paste('Time Elapsed:', Sys.time() - start.time))

  betas = numeric(n)
  for (i in 1:n) {
    test_set = sort(generate_indices(test.k))
    betas[i] = find_B0_C(test_set)$x
  }

  lambda[,test.k-1] = pi/(2*(pi-2)) * betas/S2
}

end.time = Sys.time()
# About 10 minutes on a standard PC.
(duration = end.time - start.time)

lambda.csv = as.data.frame(lambda)
colnames(lambda.csv) = k

write.csv('lambdas.csv')


