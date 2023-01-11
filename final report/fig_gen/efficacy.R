# Incorporating the idea from the euclidean distance document, I'll be using this script to generate the results.
library(SK4FGA)
library(tidyverse)
library(mvtnorm)

# glass.data = prepare_data(glass)
#
# get_fragment_dist <- function(d){
#   d.split = split(d, factor(d$fragment))
#   get_info <- function(e){
#     means = apply(e[,-(1:2)], 2, mean)
#     sds = apply(e[,-(1:2)], 2, sd)
#     list(means, sds)
#   }
#   append(lapply(d.split, get_info), list(item = d$item[1]))
# }
#
#
# mv_cov <- function(x) {
#   cov(x)
# }
#
# mv_mean <- function(x) {
#   apply(x, 2, mean)
# }
#
# generate_data <- function(n = 10){
#   # Create the sample
#   sam = sample(1:length(glass.data), n)
#   data.test = vector('list', n)
#
#
#
#   for (i in 1:n){
#     current = glass.data[[sam[i]]]
#     current.cov = mv_cov(current)
#     current.mean = mv_mean(current)
#     test.dat = rmvnorm(12, sigma = current.cov, mean = current.mean) %>%
#       # For consistency, I will round the numerics in the test data to 4 d.p; just like the original dataset.
#       round(4) %>%
#       as.data.frame()
#
#     colnames(test.dat) = colnames(current)
#
#     data.test[[i]] = test.dat
#   }
#
#   names(data.test) = paste0('t', 1:n)
#
#   list(data = data.test, ref.items = names(glass.data)[sam])
# }
#
# # Number of simulations
# n = 10000
#
# # Sample size of those ripped from dataset.
# sample.size = 10
#
# success_rate = numeric(n)
#
# # Recording the length of time to simulate.
# start.time = Sys.time()
#
# for (i in 1:n){
#   # Progress
#   if (i %% (n %/% 100) == 0) print(paste0(i / (n %/% 100), '% complete (', round(difftime(Sys.time(), start.time, units = 'secs')), ' seconds elapsed)'))
#
#
#   random_data = generate_data(sample.size)
#   ref_data = glass.data[random_data$ref.items]
#   test = append(random_data$data, ref_data)
#
#
#   part = tryCatch(partition.multi(test), error = function(p) NULL)
#
#   if (is.null(part)) {
#     success_rate[i] = NA
#     next
#   }
#
#   match = logical(sample.size)
#
#   for (j in 1:sample.size) {
#     # Test 1
#     tj.group = part$groups[which(names(test) == paste0('t', j))]
#     ref.item = random_data$ref.items[j]
#     ref.item.group = part$groups[which(names(test) == ref.item)]
#     match[j] = tj.group == ref.item.group
#
#   }
#
#   success_rate[i] = sum(match) / sample.size
#
#
# }
#
# end.time = Sys.time()
# (duration = end.time - start.time)
#
# results = tibble(success_rate)
#
# # Percentage of total success
# sum(success_rate == 1, na.rm = T) / length(success_rate[!is.na(success_rate)])
#
# # Visualisation of Success
#
# p.multi = results %>%
#   filter(!is.na(success_rate)) %>%
#   group_by(success_rate) %>%
#   summarise(n = n()) %>%
#   ungroup() %>%
#   summarise(success_rate, n, p = n / sum(n)) %>%
#   ggplot(aes(x = success_rate, y = p)) +
#   geom_col(col = 'black', fill = scales::hue_pal()(2)[2]) +
#   expand_limits(x = 0) +
#   scale_x_continuous(breaks = seq(0, 1, 0.1)) +
#   scale_y_continuous(labels = scales::percent) +
#   theme_bw() +
#   labs(title = 'Success Rate of SKmulti', x = 'Success Rate', y = 'Percentage')
# p.multi
#
#
#
#
#
#
#
#
# # Number of simulations
# n = 10000
#
# # Sample size of those ripped from dataset.
# sample.size = 10
#
# success_rate.uni = numeric(n)
#
# # Recording the length of time to simulate.
# start.time = Sys.time()
#
# for (i in 1:n){
#   # Progress
#   if (i %% (n %/% 10) == 0) print(paste0(i / (n %/% 10) * 10, '% complete (', round(difftime(Sys.time(), start.time, units = 'secs')), ' seconds elapsed)'))
#
#
#   ref_data = generate_indices(sample.size, .sd_multi = 6)
#   random_data = rnorm(sample.size, ref_data, sd = 4e-5)
#   test = append(random_data, ref_data)
#
#
#   part = tryCatch(partition(test), error = function(p) NULL)
#
#   if (is.null(part)) {
#     success_rate[i] = NA
#     next
#   }
#
#   match = logical(sample.size)
#
#   for (j in 1:(sample.size)) {
#     match[j] = part$groups[j] == part$groups[j+sample.size]
#
#   }
#
#   success_rate.uni[i] = sum(match) / sample.size
#
#
# }
#
# end.time = Sys.time()
# (duration = end.time - start.time)
#
#
#
#
#
# results.uni = tibble(success_rate.uni)
#
# # Percentage of total success
# sum(success_rate.uni == 1, na.rm = T) / length(success_rate.uni[!is.na(success_rate.uni)])
#
# # Visualisation of Success
#
# p.uni = results.uni %>%
#   filter(!is.na(success_rate.uni)) %>%
#   group_by(success_rate.uni) %>%
#   summarise(n = n()) %>%
#   ungroup() %>%
#   summarise(success_rate.uni, n, p = n / sum(n)) %>%
#   ggplot(aes(x = success_rate.uni, y = p)) +
#   geom_col(col = 'black', fill = scales::hue_pal()(1)) +
#   expand_limits(x = 0) +
#   scale_x_continuous(breaks = seq(0, 1, 0.1)) +
#   scale_y_continuous(labels = scales::percent) +
#   theme_bw() +
#   labs(title = 'Success Rate of SKM2', x = 'Success Rate', y = 'Percentage')
# p.uni


# efficacy_data = tibble(results, results.uni)
# write_csv(efficacy_data, 'efficacy_data.csv')

setwd('../final report')

efficacy_data = read_csv('fig_gen/efficacy_data.csv')

results = efficacy_data[1]
results.uni = efficacy_data[2]
binom_data = tibble(success_rate = 0:10, pb = dbinom(success_rate, 10, 1-0.05))


p.multi = results %>%
  filter(!is.na(success_rate)) %>%
  group_by(success_rate) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  summarise(success_rate, n, p = n / sum(n)) %>%
  mutate(success_rate = success_rate*10) %>%
  right_join(binom_data) %>%
  ggplot(aes(x = success_rate, y = p)) +
  geom_col(col = 'black', fill = scales::hue_pal()(2)[2]) +
  geom_point(aes(y = pb, col = 'ProbBinomial(x, 10, 1-0.05)'), alpha = 0.5) +
  geom_line(aes(y = pb, col = 'ProbBinomial(x, 10, 1-0.05)'), alpha = 0.5) +
  expand_limits(x = 0) +
  scale_x_continuous(breaks = 0:10) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_bw() +
  labs(title = 'Success Rate of SKmulti', x = 'Number of Correct Matchings', subtitle = 'As estimated on 10,000 simultations with array sizes k = 10', y = 'Percentage') +
  theme(legend.position = 'top',
        legend.title = element_blank()) +
  scale_color_manual(values = 'black')
p.multi

p.uni = results.uni %>%
  filter(!is.na(success_rate.uni)) %>%
  group_by(success_rate.uni) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  summarise(success_rate.uni, n, p = n / sum(n)) %>%
  rename('success_rate' = success_rate.uni) %>%
  mutate(success_rate = success_rate*10) %>%
  right_join(binom_data) %>%
  ggplot(aes(x = success_rate, y = p)) +
  geom_col(col = 'black', fill = scales::hue_pal()(1)) +
  geom_point(aes(y = pb, col = 'ProbBinomial(x, 10, 1-0.05)'), alpha = 0.5) +
  geom_line(aes(y = pb, col = 'ProbBinomial(x, 10, 1-0.05)'), alpha = 0.5) +
  expand_limits(x = 0) +
  scale_x_continuous(breaks = 0:10) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_bw() +
  labs(title = 'Success Rate of SKM2', subtitle = 'As estimated on 10,000 simultations with array sizes k = 10', x = 'Number of Correct Matchings', y = 'Percentage') +
  theme(legend.position = 'top',
        legend.title = element_blank()) +
  scale_color_manual(values = 'black')
p.uni

ggsave('../figures/pmulti.png', p.multi, width = 4.5, height = 4)
ggsave('../figures/puni.png', p.uni, width = 4.5, height = 4)

