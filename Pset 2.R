#Problem 5
# Load the ak91.csv data
df <- read.csv ("./data/ak91.csv")

# Store years of education and the weekly wage in separate variables
yrs_educ <- df$YRS_EDUC
wkly_wage <- df$WKLY_WAGE

# Find college graduates
has_college_degree <- yrs_educ == 16

# a
X <- ifelse(yrs_educ==16,1,0)

#sample analogue calculation
sum(X)/length(X) # = 0.1084857

# b

mu_college <- (sum(X * wkly_wage)/length(X)) / (sum(X)/length(X)) 
# 594.4866

# c
sd_hat <- ((sum(X*(wkly_wage^2))/sum(X)) - 
             ((sum(X*wkly_wage)/sum(X))^2)) # 184035.5

p_hat <- sum(X)/length(X) # 0.1084857

se_college <- (1/sqrt(length(X))) * sqrt(sd_hat/p_hat) # 2.268982


# d
z <- qnorm(0.05/2,lower.tail=F) # 1.959964

#CI
mu_college - z*se_college # 590.0395
mu_college + z*se_college # 598.9337

# e
# 600 is not within the CI, we reject the null hypothesis that mu = 600 at the
# 95% CI. Intuitively, this means that it is not likely for a person's expected
# weekly wage to be $600/week if they got a college education.
 

# Part (f) 
# Similar to (e), we. Given that 595 is within the CI, we fail to reject the 
# null hypothesis that mu = 595. Intuitively, this suggests that it is 
# likely for a person's expected weekly wage would be $595/week if 
# they got a college education. 
