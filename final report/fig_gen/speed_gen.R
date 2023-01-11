library(tidyverse)
library(SK4FGA)

setwd('../final report')

time_data = read_csv('fig_gen/partition_times.csv')
time_data

p1 = time_data %>%
  pivot_longer(cols = 2:4, names_to = 'alg', values_to = 'time') %>%
  group_by(alg, k) %>%
  mutate(mean_time = mean(time),
         alg = factor(alg, levels = c('part_R.time', 'part_C.time', 'part_fast.time'))) %>%
  ggplot(aes(x = k, y = time, col = alg)) +
  geom_point() +
  geom_line(aes(y = mean_time)) +
  theme_bw() +
  labs(title = 'Time Comparision between Partitioning Algorithms',
       x = 'Sample Size', y = 'Time (seconds)', col = 'Algorithm') +
  scale_color_manual(values = scales::hue_pal()(3), labels = c('Initial R coded partition algorithm', 'Initial algorithm with C++ Lifting', 'Final algorithm with saved critical values.')) +
  scale_y_continuous(breaks = 0:10) +
  scale_x_continuous(breaks = 2:24) +
  theme(legend.position = 'top',
        legend.text = element_text(size = 12),
        legend.title = element_blank())
p1


ggsave('figures/partition_times.png', p1, width = 12, height = 4)


# Getting new values with new chi squared approximated distribution instead of empirical.

k = time_data$k

new_time = numeric(nrow(time_data))

for (i in 1:length(k)) {
  array.size = k[i]
  test = generate_indices(array.size)
  new_time[i] = system.time(partition(test))[3]
}

time_data2 = time_data %>%
  cbind(new_time) %>%
  as_tibble() %>%
  select(-4) %>%
  rename('part_fast.time' = new_time)
time_data = time_data2


# write_csv(time_data, 'fig_gen/partition_times.csv')
