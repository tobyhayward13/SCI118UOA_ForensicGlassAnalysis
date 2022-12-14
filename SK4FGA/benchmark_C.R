# Test 2

# Testing the speeds of the partition algorithm on both my .R code and Lewis's c++ code.

library(SK4FGA)
library(tidyverse)

# Initial test case

x = generate_indices(20, .sd_multi = 5)
x

result.R = partition(x)
plot.tree(result.R)

result.C = partition_C(x)
plot.tree(result.C)

# Different Trees likely. But same groupings

lapply(ungroup.partition(result.R$tree), function(p) p$ix)
lapply(ungroup.partition(result.C$tree), function(p) p$ix)



# Sample sizes
sample_sizes = 3:12

# Variation scales
# Small and large
variations = c(1, 6)

# Number of trials per run
n = 10


# Create the vectors

sample_size = rep(sample_sizes, each = length(variations) * n)
variation = rep(rep(variations, each = length(sample_sizes)), n)


timings_R = timings_C = numeric(length(sample_size))

# Run the tests

start.time = Sys.time()

for (i in 1:length(sample_size)) {
  # Print debugging
  if (i %% 10 == 0) print(paste0('Test #', i))

  ri = generate_indices(sample_size[i], .sd_multi = variation[i])
  t1 = system.time(partition(ri))
  t2 = system.time(partition_C(ri)) # partition_C ###

  timings_R[i] = t1[3]
  timings_C[i] = t2[3]
}

end.time = Sys.time()
# Time to run test
end.time - start.time


# Visualisation

data.time = tibble(
  sample_size,
  variation,
  timings_R,
  timings_C
)

data.time %>%
  pivot_longer(cols = 3:4, names_to = 'language', values_to = 'Time_s') %>%
  # Assumes ony variations\ multipliers of 1 and 6.
  mutate(variation = ifelse(variation == 1, 'Small Variation (1)', 'Large Variation (6)')) %>%
  group_by(sample_size, variation, language) %>%
  mutate(avg_time = mean(Time_s)) %>%
  ggplot(aes(x = sample_size, y = Time_s, col = language)) +
  geom_point() +
  facet_wrap(~variation) +
  geom_line(aes(y = avg_time)) +
  theme_bw() +
  labs(title = 'Time Comparison between two algoritms',
       x = 'Sample Size', y = 'Time (s)', col = 'Coding Language')











## Is it consistent with the other algorithm
# It'll be good to know if the new algorithm is consistent with the other one. We know that machine precision differs in the two languages which may cause differences in splitting.


set.seed(123)
x = generate_indices(20, .sd_multi = 5)
x

result.R = partition(x)
plot.tree(result.R)

result.C = partition_C(x)
plot.tree(result.C)

lapply(ungroup.partition(result.R$tree), function(p) p$ix)
lapply(ungroup.partition(result.C$tree), function(p) p$ix)

# Not the same. This is a little bit of a concern.










