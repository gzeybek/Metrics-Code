install.packages("ggplot2")
library(ggplot2)

#a)
n <- 10000
mu <- 0
sigma <- 1
x <- rnorm(n, mu, sigma)
hist(x)

#b)
n <- 10000
min_y <- -1
max_y <- 1
y <- runif(n, min_y, max_y)
hist(y)

#9)
#b)
my_rbernoulli <- function(n, p) {
  U <- runif(n, min = 0, max = 1)
  x <- U <= p
  # Return draws
  return(x) }
}#MY_RBERNOULLI

# Test the custom Bernoulli generator function
x <- my_rbernoulli (10000 , 0.5)
length(x) == 10000 # should return TRUE
mean(x) # should a number near 0.5

#c)
my_rbinomial <- function(n, p, m) {
  y <- rep(0,n)
  for (i in 1:n) {
    x <- my_rbernoulli(m, p)
    y[i] <- sum(x)
  }
  return(y)
}#MY_RBINOMIAL

# Test the custom Binomial generator function
x <- my_rbinomial (10000 , 0.5, 10)
length(x) == 10000 # should return TRUE
mean(x) # should a number near 5
