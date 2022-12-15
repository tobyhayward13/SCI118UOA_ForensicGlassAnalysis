# This is a file I am using to understand and create the hotellings t-squared test between two multivariate samples.

# Source:   https://www.youtube.com/watch?v=RmktDvMrhGw

# Sample data from video
treatments = c(rep('Viral', 6), rep('Bacterial', 6))
crp = c(40.0, 11.1, 30.0, 21.4, 10.7, 3.4, 42.0, 31.1, 50.0, 60.4, 45.7, 17.3)
temp = c(36.0, 37.2, 36.5, 39.4, 39.6, 40.7, 37.6, 42.2, 38.5, 39.4, 38.6, 42.7)

data = data.frame(
  treatments,
  crp,
  temp
)
data

# Means within groups
crp_viral_mu = mean(crp[treatments == 'Viral'])
temp_viral_mu = mean(temp[treatments == 'Viral'])
crp_bacterial_mu = mean(crp[treatments == 'Bacterial'])
temp_bacterial_mu = mean(temp[treatments == 'Bacterial'])

# Sizes of each group (6 each for this example)
n_viral = sum(treatments == 'Viral')
n_bacterial = sum(treatments == 'Bacterial')

# Number of parameters
p = ncol(data) - 1


# Mahalanobis Distance Squared

# Pooled within groups cov matrix

cov_viral = cov(data[treatments == 'Viral',-1])
cov_bacterial = cov(data[treatments == 'Bacterial',-1])

S = ((n_viral-1)*cov_viral + (n_bacterial-1)*cov_bacterial) / (n_viral + n_bacterial - 2)
S_inv = solve(S)

#

md_squared =
  t(c(crp_viral_mu, temp_viral_mu) - c(crp_bacterial_mu, temp_bacterial_mu)) %*%
  S_inv %*%
  (c(crp_viral_mu, temp_viral_mu) - c(crp_bacterial_mu, temp_bacterial_mu))


# T-Squared

T_squared = (n_viral * n_bacterial) / (n_viral + n_bacterial) * md_squared


# F-statistic

F_stat = (n_viral + n_bacterial - p - 1) / (p * (n_viral + n_bacterial - 2)) * T_squared

# P-value

df1 = p
df2 = n_viral + n_bacterial - p - 1

p_value = pf(F_stat, df1, df2, lower.tail = F)






