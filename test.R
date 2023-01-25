# A test file to test our package out.

library(SK4FGA)

ri <- generate_indices(20, .sd_multi = 5)

ri

part = partition(ri)
plot(part)

test.data = prepare_data(glass, 1)[1:5]
part = partition.multi(test.data)
plot(part)

test.data = prepare_data(vehicle.glass, 1)[sample(1:50, 3)]
part = partition.multi(test.data)
plot(part)





# Testing with 2n items; n from the glass dataset and n randomly generated, should be statistically significantly close items.
library(mvtnorm)

set.seed(123)
n = 6
test.data = prepare_data(glass, 1)[sample(1:50, n)]
mean_vecs = lapply(test.data, function(d) apply(d, 2, mean))
cov_mats = lapply(test.data, cov)

test.data.new = vector("list", n)
size = nrow(test.data[[1]])

for (i in 1:n) {
  mu = mean_vecs[[i]]
  sig = cov_mats[[i]]
  test.data.new[[i]] = as.data.frame(rmvnorm(size, mean = mu, sigma = sig))
}
names(test.data.new) = paste0(names(test.data), '_test')

test.data.new

final_test = append(test.data, test.data.new)
# Shuffle
final_test = final_test[sample(length(final_test))]

part.final = partition.multi(final_test)
plot(part.final)

