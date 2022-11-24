# A test file to test our package out.

library(SK4FGA)

ri <- generate_indices(30, .sd_multi = 5)
ri

part = partition(ri)

plot.tree(part)
