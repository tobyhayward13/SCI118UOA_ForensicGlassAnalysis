# A test file to test our package out.

library(SK4FGA)

ri <- generate_indices(.sd_multi = 3)
ri

part = partition(ri)

plot.tree(part)
