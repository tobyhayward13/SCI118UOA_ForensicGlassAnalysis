# Second test file for SK4FGA

# Testing to see if ranking the values by euclidean distance to the overall mean is a better way to order if we are thinking of pursuing this method of ranking before splitting.
# Theory here is that by ordering the items this way will group potentially matching items and hence they will appear together after splitting.


# Instead of coding two partition.multi algorithms, just pass a sorted one and an unsorted one to the current partition.multi and estimate success.

library(SK4FGA)

data(glass)
data = prepare_data(glass)

order_euclid <- function(alist){
  # Sorts a list of split dataframes as outputted by the prepare_data() function.
  # Calculate overall mean for each group.
  all.data = do.call(rbind, alist)[, -(1:2)]
  mid = apply(all.data, 2, mean)

  # Average values for each feature within those in list
  points = lapply(alist, function(p) apply(p[, -(1:2)], 2, mean))

  # Calculate distances
  points.d = sapply(points, function(v) sum((v - mid)^2))

  # Return data in order of smallest euclidean distance to largest.
  indices = sort(points.d, index.return = T)$ix
  alist[indices]
}

data.test = data[1:3]

order_euclid(data.test)

# Cool that works! Now would be good to test on some data.




# Test 1 Testing with the first three items.

data.test = data[1:3]

result = partition.multi(data.test)
result.sort = partition.multi(order_euclid(data.test))
plot.tree(result)
plot.tree(result.sort)

# Similar trees with only rearranged values but still discriminating which is good.




# Test 2 Testing within the fragments of a single item.

data.test = data[[1]]
data.test = split(data.test, factor(data.test$fragment))

result = partition.multi(data.test)
result.sort = partition.multi(order_euclid(data.test))
plot.tree(result)
plot.tree(result.sort)

# Likewise. Same result as before. This is promising that it isn't messing with the algorithms ability to split.




# Test 3 Testing with a random selection of items.

set.seed(123)
data.test = data[sample(1:200, 10)]
result = partition.multi(data.test)
result.sort = partition.multi(order_euclid(data.test))
plot.tree(result)
plot.tree(result.sort)
table(result.sort$groups)
table(result$groups)

# Splitting every item up with unique single leaves.
# However I noticed that the algorithm would group some together when taking a random sample. This is in my mind a case where two different items have very similar chemical features.

set.seed(17)
data.test = data[sample(1:200, 10)]
result = partition.multi(data.test)
result.sort = partition.multi(order_euclid(data.test))
plot.tree(result)
plot.tree(result.sort)
table(result$groups)
table(result.sort$groups)

# For example in this case we have both s60 and s34 with, according to hotellings T^2, are close enough in space to be considered the same.
# However, when not ordered, they are considered different due to the splitting procedure and crap assumption of ordering. Perhaps this is a sign that this method somewhat works?
# Is my algorithm meant to discriminate between all items in the glass dataset?






# Test 4 Testing with a random group of items within fragments.

# This is dangerous since testing between fragments from the same sample can have issues with high correlation and hence very small covariance.
set.seed(13)
data.test = data[sample(1:200, 3)]
# data.test = data[c(120, 131, 93)]
data.test = do.call(rbind, data.test)
data.test = transform(data.test, item = paste0(item, fragment))
data.test = prepare_data(data.test)


result = partition.multi(data.test)
result.sort = partition.multi(order_euclid(data.test))
plot.tree(result)
plot.tree(result.sort)
table(result$groups)
table(result.sort$groups)

# Doesn't correctly group fragments together when they are ordered using our function. This would lead you to believe that perhaps this is a result of a poor sorting algorithm
# choice but actually it is because the items are presorted this way when running this test. A better way to do it is to shuffle it beforehand and view the results (later, see below).
# Noticed that it throws an error frequently when using a random selection of items. (Set seed)
# > Error in solve.default(S) :
#   system is computationally singular: reciprocal condition number = 1.11994e-20
# I believe this is because the covariance matrix between two item sets is too small and hence the machine throws a precision error.
# This occurs when the data is too closely related, and hence it cannot calculate the inverse of an essentially null (0) matrix.

# Another type of error occurs sometimes:
# > Error in if (pvalue < alpha) { : missing value where TRUE/FALSE needed
# I figured this out to be a bug in my code where I was calculating my number of parameters to be 9 instead of 7. (ncol(data))
# Upon fixing this, the error doesn't return again.

data.test = data[c(71, 41, 104)]
# data.test = data[sample(1:200, 3)]
data.test = do.call(rbind, data.test)
data.test = transform(data.test, item = paste0(item, fragment))
data.test = prepare_data(data.test)

result = partition.multi(data.test)
# undebug(partition.multi)
result.sort = partition.multi(order_euclid(data.test))
plot.tree(result)
plot.tree(result.sort)
table(result$groups)
table(result.sort$groups)

# Fixing the bug yielded some interesting results.
# By ordering the fragments by distance to mean, the algorithm is now computing some fragments from other items to be statistically significant with fragments from other items.


# Now if we shuffle the fragments before putting them in

# set.seed(19)
# data.test = data[c(120, 131, 93)]
# data.test = do.call(rbind, data.test)
# data.test = transform(data.test, item = paste0(item, fragment))
# data.test = prepare_data(data.test)
#
# result = partition.multi(data.test)
# # Shuffle
# data.test.shuffle = data.test[sample(length(data.test))]
# result.shuffle = partition.multi(data.test.shuffle)
# result.sort = partition.multi(order_euclid(data.test))
# plot.tree(result)
# plot.tree(result.shuffle)
# plot.tree(result.sort)
# table(result$groups)
# table(result.shuffle$groups)
# table(result.sort$groups)

# Couldn't get around the machine error.







# Test 5 This test involves simulating data that has been estimated from the .csv file that was given to me.

library(tidyverse)

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

data.fragment_normal = do.call(rbind, data) %>%
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


# Test 5.1 Testing generated data

# debug(generate_data)
set.seed(123)
data.test = generate_data(5)
data.test$ref.items

result = partition.multi(data.test$data)
result.sort = partition.multi(order_euclid(data.test$data))
plot.tree(result)
plot.tree(result.sort)
table(result$groups)
table(result.sort$groups)

# As expected.


# Now when combined with the data that was used to generate the parametric bootstrapped data.

data.test2 = data[data.test$ref.items]

data.test3 = append(data.test$data, data.test2)

# See how similar?
data.test3[c(1, 6)]
data.test$ref.items[1]

# Shuffle data
data.test3 = data.test3[sample(length(data.test3))]

result = partition.multi(data.test3)
result.sort = partition.multi(order_euclid(data.test3))
plot.tree(result)
plot.tree(result.sort)
table(result$groups)
table(result.sort$groups)
data.test$ref.items

# As opposed to just shuffling the data, it is a little more successful.
# This is promising but not totally convincing. From the seed above, the ordering seems to have done most of the job of correctly grouping those test items with the fake items that
# were simulated from them, but is just narrowly "under discriminating". This may be somewhat fixed with a lower alpha value (leniency to split), however it still find s61 and s94
# to be very similar.

result.sort = partition.multi(order_euclid(data.test3), alpha = 0.001)
plot.tree(result.sort)





# Should be noted that with other samples, it discriminates fine and successfully groups them well.

set.seed(12)
data.test = generate_data(5)

# Now when combined with the data that was used to generate the parametric bootstrapped data.

data.test2 = data[data.test$ref.items]

data.test3 = append(data.test$data, data.test2)

# Shuffle data
data.test3 = data.test3[sample(length(data.test3))]

# See how similar?
data.test3[c(1, 6)]
data.test$ref.items[1]

result = partition.multi(data.test3)
result.sort = partition.multi(order_euclid(data.test3))
plot.tree(result)
plot.tree(result.sort)
table(result$groups)
table(result.sort$groups)
data.test$ref.items





# Lets test this success rate as opposed  to not sorting.
# We want the result of the grouping to match those simulated. If all of them match, then it is successful.
# We want to quantify the success over just randomly shuffling the data, but for now I just want to know how many times it correctly groups them by sorting.

n = 10000

successes = logical(n)

start.time = Sys.time()

for (i in 1:n){
  # Progress
  if (i %% 100 == 0) print(paste('Test', i))


  data.test = generate_data(5)
  data.test2 = data[data.test$ref.items]
  data.test3 = append(data.test$data, data.test2)

  result.sort = partition.multi(order_euclid(data.test3))

  # If the sorted result is not of length 5, reject.
  if (length(unique(result.sort$groups)) > 5) {next}  # Is false.

  matches.sort = t(apply(matrix(names(order_euclid(data.test3)), nrow = 5, byrow = T), 1, sort))

  is.match = data.test$ref.items[as.numeric(substring(matches.sort[, 2], 2, 2))] == matches.sort[,1]

  successes[i] = all(is.match)
}

end.time = Sys.time()

# Time to compute.
(duration = end.time - start.time)

# Success rate:
sum(successes) / n

# Not a great success rate












