# A test file to test our package out.

library(SK4FGA)
library(tidyverse)

ri <- generate_indices(20, .sd_multi = 5)

ri

part = partition(ri)
plot(part)

test.data = tibble(glass)
prepare_data(test.data)
undebug(prepare_data)

part = partition.multi(test.data)

plot(part)# Testing

# Data import
# data = read.csv('glass.csv')
data(glass)
data = glass

# undebug(prepare_data)
data.prep = prepare_data(data)

# Test 1

data.test = data.prep[1:3]

# debug(partition.multi)
result = partition.multi(data.test)

# undebug(ungroup.partition)
# ungroup.partition(result$tree)

plot(result)


# Test 2

data.test = prepare_data(data, c('item', 'fragment'))[1:4]

# debug(partition.multi)
result = partition.multi(data.test)

# undebug(plot)
plot(result)

# Test 3

set.seed(123)
data.test = data.prep[sample(1:200, 10)]
result = partition.multi(data.test)
plot(result)


# Test 4
# This is dangerous since testing between fragments from the same sample can have issues with high correlation and hence very small covariance.


data.prep = data %>% prepare_data(1:2)
# sam = '677 678 679 680 545 546 547 548 653 654 655 656' %>% str_split('[:space:]') %>% unlist() %>% as.numeric() # Confuses 1
# sam = '301 302 303 304 249 250 251 252 561 562 563 564' %>% str_split('[:space:]') %>% unlist() %>% as.numeric() # Confuses 2, cant find significance between two. Can create more splits by decreasing alpha.
# sam = '149 150 151 152  81  82  83  84 161 162 163 164' %>% str_split('[:space:]') %>% unlist() %>% as.numeric() # Confuses 1
sam = rep(sample(1:200, 3)*4 - 3, each = 4) + 0:3
data.test = data.prep[sam]

result = partition.multi(data.test, alpha = 0.05)
result$groups
plot(result)






# Test 5
# This test involves simulating data that has been estimated from the .csv file that was given to me.

library(tidyverse)
data %>%
  pivot_longer(cols = 3:9, names_to = 'chemical', values_to = 'value') %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~chemical, scales = 'free_x')
# Data seems to be bimodal normal. Best to perhaps generate data by bootstrap sampling data from within items.



# It would be cool to put this into it's own .R file eventually within the package.
get_fragment_dist <- function(d){
  d.split = split(d, factor(d$fragment))
  get_info <- function(e){
    means = apply(e[,-(1:2)], 2, mean)
    sds = apply(e[,-(1:2)], 2, sd)
    list(means, sds)
  }
  append(lapply(d.split, get_info), list(item = d$item[1]))
}

data.fragment_normal = data %>%
  group_by(item) %>%
  group_split() %>%
  map(get_fragment_dist)


generate_data <- function(n = 10){
  # Create the sample
  sam = sample(1:length(data.fragment_normal), n)
  data.sam_normal = data.fragment_normal[sam]
  data.test = vector('list', n)



  for (i in 1:n){
    current = data.fragment_normal[[sam[i]]]

    test.dat = c(
      rnorm(3 * 7, current$f1[[1]], current$f1[[2]]),
      rnorm(3 * 7, current$f2[[1]], current$f2[[2]]),
      rnorm(3 * 7, current$f3[[1]], current$f3[[2]]),
      rnorm(3 * 7, current$f4[[1]], current$f4[[2]])
    )

    test.dat = matrix(test.dat, nrow = 12, byrow = T)

    colnames(test.dat) = names(current$f1[[1]])

    data.test[[i]] = cbind(item = paste0('t', i), fragment = paste0('f', rep(1:4, each = 3)), as.data.frame(test.dat))
  }

  names(data.test) = paste0('t', 1:n)

  list(data = data.test, ref.items = sapply(data.sam_normal, function(p) p$item))
}


# debug(generate_data)
data.test = generate_data(5)
data.test$ref.items

result = partition.multi(data.test$data)

plot(result)

# Now when combined with the data that was used to generate the parametric bootstrapped data.

data.test2 = data.prep[data.test$ref.items]

data.test3 = append(data.test$data, data.test2)

# See how similar?
data.test3[c(1, 6)]
data.test$ref.items[1]

result = partition.multi(data.test3)
table(result$groups)
plot(result)

# It is very sensitive. It shouldn't be discriminating between the two samples.








# Test 6
# Using the same data twice.
data.prep = prepare_data(glass, 1)
data.test = rep(data.prep[sample(length(data.prep), 3)], each = 2)
names(data.test) = paste0(names(data.test), '.', 1:2)
data.test

result = partition.multi(data.test)
plot(result)

# It only seems to discriminate when the data matches entirely. This is not ideal.
# This is a result of a stupid assumption that the data is ordered and hence will assume two likely equal items to be different. Observe following example.

# Random sample based on before
data.test = data.test[sample(6)]
result = partition.multi(data.test)
plot(result)

# Notice now that because the two datasets are not next to eachother in the list, they are discriminated apart and hence considered different.














data

prepare_data <- function(data, label = NA){
  if (length(label) < 2) if (is.na(label)) {
    warning('No column assigned to be the label. First column is used.')
    label = 1L
  }
  # Convert to numeric (just easier)
  if (typeof(label) == 'character') label = which(colnames(data) %in% label)
  if (length(label) > 1) {
    items = apply(data[,label], 1, paste, collapse = '.')
    data = cbind(items, data[,-label])
  }
  else items = factor(data[,label])

  split(data[,which(sapply(data, class) == 'numeric')], items)
}

data = glass
data = prepare_data(data, 'item')

data.test = data[sample(1:length(data), 3)]

part = partition.multi(data.test)
plot(part)
part$groups

ri = generate_indices(.sd_multi = 5)
ri
Q
part = partition(ri)
plot(part)

# undebug(prepare_data)
data(glass)
data.multi = prepare_data(glass, 1)[1:3]
part = partition.multi(data.multi)
plot(part)

