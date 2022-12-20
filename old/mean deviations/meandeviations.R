# Second test file for SK4FGA

library(SK4FGA)

data(glass)
data = prepare_data(glass)

# First test, take a random sample from the data and create a simulated sample based on the summary statistics of it's features.

set.seed(39)
i = sample(1:length(data), 1)
test_obj = data[[i]]

test_obj

# Create the function that samples with an argument to slightly deviate the mean.

generate_pbtsp_data <- function(sam, .mean_deviate = 0){
  # Split into 4 fragments
  sam.split = split(sam, sam$fragment)
  means = lapply(sam.split, function(d) apply(d[,-(1:2)], 2, mean))
  sds = lapply(sam.split, function(d) apply(d[,-(1:2)], 2, sd))

  # Assume that all fragment splits are of equal size (3 rows)
  means = unlist(rep(means, each = 3)) + .mean_deviate
  sds = unlist(rep(sds, each = 3))

  # Create the data
  features = matrix(ncol = ncol(sam)-2,
                    rnorm(length(means), means, sds),
                    byrow = T)
  colnames(features) = colnames(sam)[-(1:2)]

  final = cbind(
    item = 't1',
    fragment = paste0('f', rep(1:4, each = 3)),
    as.data.frame(features)
  )
  final
}


generate_pbtsp_data(test_obj)





# Now to join it and see if it partitions

i = sample(1:length(data), 1)
sam = data[[i]]
sim_sam = generate_pbtsp_data(sam)

# names
data_names = c(sam$item[[1]], sim_sam$item[[1]])
test.data = append(list(sam), list(sim_sam))
names(test.data) = data_names

result = partition.multi(test.data)

plot.tree(result)

# Excellent, so for data that should look exactly the same the algorithm works. Now to come up with a rigorous method to test this.
# result$groups for two samples should return either 1 1 for a match or 1 2 for a mismatch. This can be our test to see if there is a match.

# From previous sample
length(unique(result$groups))
# == 1
# == match.

# New obviously different sample.
sim_sam = generate_pbtsp_data(sam, .mean_deviate = 10)
# names
data_names = c(sam$item[[1]], sim_sam$item[[1]])
test.data = append(list(sam), list(sim_sam))
names(test.data) = data_names


result = partition.multi(test.data)

plot.tree(result)

length(unique(result$groups))
# == 2
# => mismatch.








# For this analysis, use tidyverse operations (maybe)
library(tidyverse)

# Initialise numbers of deviations and number of tests per mean deviation
# We should choose a sensible amount of deviation based on the data.
item_sds = matrix(unlist(lapply(data, function(d) apply(d[,-(1:2)], 2, sd))),
       ncol = 7, byrow = T) %>%
  as_tibble() %>%
  cbind(item = names(data), .)

colnames(item_sds) = c('item', colnames(glass)[-(1:2)])

item_sds %>%
  as_tibble() %>%
  pivot_longer(cols = -1, names_to = 'element', values_to = 'conc_sd') %>%
  ggplot(aes(x = element, y = conc_sd)) +
  geom_boxplot()

summary(item_sds)

# Deviations vary by element, but let's choose a small increment like 0.001


devs = seq(0, 0.5, 0.001)
n.tests = 20
devs.tests = rep(devs, each = n.tests)

# Initialise logical vector to determine if a split occurred.
did.split = logical(length(devs.tests))
# Initialise a character vector to store the inspired sample.
sample_name = character(length(devs.tests))


# Now perform the loop (takes about 20 - 40 seconds)

for (i in 1:length(did.split)){
  sam.i = sample(1:length(data), 1)
  sam = data[[sam.i]]
  sim_sam = generate_pbtsp_data(sam, .mean_deviate = devs.tests[i])

  # names
  data_names = c(sam$item[[1]], sim_sam$item[[1]])
  test.data = append(list(sam), list(sim_sam))
  names(test.data) = data_names

  result = partition.multi(test.data)

  # Export inspiration sample name
  sample_name[i] = data_names[1]

  # Did it split?
  did.split[i] = length(unique(result$groups)) == 2

}

did.split


# Visualisation
tibble(
  mean_deviation = devs.tests,
  sample_name,
  did.split
) %>%
  group_by(mean_deviation) %>%
  summarise(n_mismatches = sum(did.split)) %>%
  filter(mean_deviation < 0.3) %>%
  ggplot(aes(x = mean_deviation, y = n_mismatches)) +
  geom_point() +
  geom_line(alpha = 0.3) +
  scale_y_continuous(breaks = 0:n.tests) +
  scale_x_continuous(breaks = seq(0, 0.3, 0.05)) +
  theme_bw() +
  labs(title = 'Number of mismatches for deviated meanvalue')






