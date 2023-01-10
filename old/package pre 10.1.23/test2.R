# Using the approx fun to create a linear interpolation.

library(SK4FGA)

x = seq(0, 2*pi, 0.01)
y = sin(x) + rnorm(length(x), sd = 0.1)

plot(y)

f = approxfun(x, y)
z = f(x)
lines(z, col = 'red')

x = c(1, 10)
y = 2*x
y

f = approxfun(x, y)

plot(1:20, 2 * (1:20))
lines(f(1:20))


# Approx fun will not interpolate outside the bounds.


library(SK4FGA)

lambdas
hist(lambdas[,1])


# For the case of k = 8

hist(lambdas[,8-1])

quants.x = seq(0, 1, 0.001)
quants = quantile(lambdas[,8-1], quants.x)
plot(quants.x, quants)

f = approxfun(quants.x, quants)

f(0.95)

save(f, file = 'test_fun.r')
rm(f)

load('test_fun.r')

f(0.95)


# Cool. Can we export multiple functions to the same file?
# k = 5

hist(lambdas[,5-1])

quants.x = seq(0, 1, 0.001)
quants = quantile(lambdas[,5-1], quants.x)
plot(quants.x, quants)

f5 = approxfun(quants.x, quants)

f5(0.95)

save(f, f5, file = 'test_fun.r')

rm(f, f5)

load('test_fun.r')

f5(0.95)
f(0.95)

# Excellent. Now we can develop the r script which stores the linear interpolations and use those for all quantiles outside of some particular saved values.
# Alternatively, since in the documentation it is not advised to be saving approximate functions, we can use the quantiles matrix and call approx() to perform effectively the
# same operation.

quants.x = seq(0, 1, 0.01)
critical_values = matrix(0, nrow = length(quants.x), ncol = ncol(lambdas))

for (k in 1:ncol(lambdas)) {  # Corresponding to Lambdas
  critical_values[,k] = quantile(lambdas[,k], quants.x)
}


critical_values = critical_values


usethis::use_data(critical_values, internal = T)
