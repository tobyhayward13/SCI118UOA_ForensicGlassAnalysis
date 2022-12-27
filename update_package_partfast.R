# This is the script used to generate the .rda file in the package that is used for the partition_fast algorithm.
# After running this script, close and re install the package. Then use of partition_fast should be available.


k = 2:20
n = 1e6
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
(duration = end.time - start.time)

usethis::use_data(lambdas, internal = T)
