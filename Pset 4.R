#Worked with Olafur
#6)
dat <- read.csv("~/Downloads/bw06.csv")
dat <- as.matrix(dat)

y <- dat[, "birthweight"]
w <- dat[, "cigsdaily"]
x <- cbind(1, dat[, c("boy", "age", "highschool",
                       "somecollege", "college")])

x_tld <- dat[, "married"]

#a)
beta =(1/9800)*sum(y%*%w)/((1/9800)*sum(w%*%t(w)))
beta
#[1] 0.1930429
alpha = mean(y) - mean(x)*beta
alpha
#[1] 3265.281
#The approximate change in expected birthweight associated with 
#an increase of one cigarette per day smoked by a mother is 
#0.1930429 grams.

#b)
y_t = y - mean(x)*((1/9800)*sum(y%*%x))/((1/9800)*sum(x%*%t(x)))
w_t = w - mean(x)*((1/9800)*sum(w%*%x))/((1/9800)*sum(x%*%t(x)))

b_w_cov = (1/9800)*sum(y_t*w_tilda) - (1/9800)*sum(w_t)*(1/9800)*sum(y_t)
b_w_var = (1/9800)*sum(w_t^2) - ((1/9800)*sum(w_t))^2

b_w_two = b_w_cov/b_w_var
b_w_two
#-12.77238

#Conditional on the age and education level of the mother and sex of 
#the child, the approximate change in expected birth weight of a child 
#associated with an increase of one cigarette per day smoked by 
#their mother is -12.8 g.

#c)It differs. In part b, we condition education level, age of mother, etc.
#Consequently, the last coefficient is more precise as it boils down the group
#we are analyzing.

#d)
#This statement assumes causality. 
#A better statement is: one cigarette per day is associated with an approximate
#decrease in birthweight of babies of -12.8 g.

#e)
#SO assumes smoking daily is independent of factors other than 
#infant's gender, mother's age, and education level.

#CS assumes there are both treated/untreated mothers with the same X

#f)
X[X[,'college']==1 & X[,'age']==44 & X[,'boy']==0,]

CS_1 <- subset(data, cigsdaily == 0, data$boy == 1, select = 
                  c(age))

CS_2 <- subset(data, cigsdaily == 0, data$boy == 1, select = 
                  c(highschool, somecollege, college, boy))

CS_3 <- subset(data, cigsdaily == 10, data$boy == 1, select = 
                  c(age))

CS_4 <- subset(data, cigsdaily == 10, data$boy == 1, select = 
                  c(highschool, somecollege, college, boy))

CS_5 <- subset(data, cigsdaily == 20, data$boy == 1, select = 
                  c(age))

CS_6 <- subset(data, cigsdaily == 20, data$boy == 1, select = 
                  c(highschool, somecollege, college, boy))

#We cannot validate CS but we cannot reject CS either.
#In practice, CS cannot hold for the entire sample
#For this set of x, it only corresponds with w=0
#however there are 25 unique(w), and so CS is not validated

#7)
my_coef <- function(y, X) {
  x_inv <- solve(t(X) %*% X)
  xy <- t(X) %*% xy
  return(beta)
}
coef <- my_coef(y, X)
coef

#b)
my_blp <- function(coef, x){
  blp_yX <- x %*% coef
  return(blp_yX)
}

mean(y-my_blp(coef, X))
#1.241753e-11

#d)
,y_teststat <- function(beta, se) {
  t <- abs(beta/se)
  p <- 2*(1-pnorm(t))
  return(list(t,p))
}
my_teststat(coef,se)



