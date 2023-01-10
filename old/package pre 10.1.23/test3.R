# Third test file for the SK4FGA package

library(SK4FGA)

# Testing C++ code.
# Have coded a sumofsquares function in C++ that calculates the sum of squares for a numeric vector.

sumofsquaresR <- function(v){
  sum(v^2)
}

v = rnorm(4e8)
system.time(sumofsquaresR(v))
system.time(sumofsquares(v))

ri <- generate_indices(20, .sd_multi = 1)
ri

part = partition(ri)
part.c = partition_C(ri)

# ungroup.partition(part$tree)

plot_part(part)
plot_part(part.c)

# Excellent it works! So much faster. Differences in decisions though.

