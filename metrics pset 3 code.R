# Load the ak91.csv data
df <- read.csv("data/ak91.csv")
n <- nrow(df)
# Store years of education and the weekly wage in separate variables
yrs_educ <- df$YRS_EDUC
wkly_wage <- df$WKLY_WAGE

#b)
beta <- (mean(wkly_wage*yrs_educ) - mean(wkly_wage)*mean(yrs_educ))/
  (mean(yrs_educˆ2)-mean(yrs_educ)ˆ2)

alpha <- mean(wkly_wage) - mean(yrs_educ)*beta

cat(paste0("beta: ",round(beta,2),"\nalpha: ",round(alpha,2)))

#beta: 29.62
#alpha: 61.2

#β: the approximate change in weekly change associated with a unit change in 
#years of education.

#c)

alpha + 16*beta

#535.15
#The result is different, in particular lower. The value in this pset is produced
#from BLP estimates, whereas in the previous pset it was an analog estimator and only included
#those with 16 years of education.

#d)

se_num <- sqrt(mean((resˆ2)*((yrs_educ-mean(yrs_educ))ˆ2)))
se_den <- sqrt(n)*var(yrs_educ)
resid <- wkly_wage-(alpha+beta*yrs_educ)
se <- se_num/se_den
se

#0.2102

#e)

t <-  abs((beta-31)/se)
t

#6.5547

#f)
pnorm(t,lower.tail = F)

#2.787 e^-11

#g)
#p<0.01 so we reject the null hypothesis, i.e. change in weekly wage
#associated with a one year increase in education is not $31.

#7)
#a)
my_simplecoef <- function(y,x){ 
  beta <- cov(y,x)/var(x)
  alpha <- mean(y) - mean(x)*beta 
  return(c(alpha,beta))
}
coef <- my_simplecoef(wkly_wage,yrs_educ)
coef

#61.19537 29.62240

#b)
my_simpleblp <- function(coef,x){
  return(coef[1] + coef[2]*x)
  }
mean(wkly_wage) - mean(mean(my_simpleblp(coef,yrs_educ)))

#0

#c)
my_simplese <- function(coef, y, x){
  eps <- y-(coef[1]+x*coef[2])
  se_den <- sqrt(length(x))*var(x) 
  se_num <- sqrt(mean((resˆ2)*((x-mean(x))ˆ2))) 
  return(se_num/se_den)
}
se <- my_simplese(coef,wkly_wage,yrs_educ)
se

#0.2101695

#d)

my_simpleteststat <- function(b,s){ 
  t_n <- abs(b/s)
  p_value <- pnorm(t_n,lower.tail = F) return(c(t_n,p_value))
}
my_simpleteststat(coef[2]-31,se)

#6.554693e+00 2.787823e-11

#e)
my_simpleols <- function(y,x){
  a_b <- my_simplecoef(y,x)
  be <- a_b[2]
  ols_se <- my_simplese(a_b,y,x)
  test <- my_simpleteststat(be-31,ols_se) return(c(be,ols_se,test[1],test[2]))
}
my_simpleols(wkly_wage,yrs_educ)

#2.962240e+01 2.101695e-01 6.554693e+00 2.787823e-11


