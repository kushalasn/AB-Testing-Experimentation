#Answer:1
#The null hypothesis is that there is no difference 
#in the proportion of callbacks received between resumes 
#with white-sounding names and African American names,
#Alternate hypothesis is that there is difference in the proportion of callbacks received between resumes
#i.e., H0: p1 - p2 = 0

#Answer:2
#The difference in means for the two groups is 0.12 - 0.04 = 0.08.

#Answer:3
# Define the sample proportions and sample sizes
p1 <- 0.12
p2 <- 0.04
n1 <- 6000
n2 <- 6000

# Calculate the standard error of the difference in proportions
SE <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)

# Calculate the t-statistic
t_statistic <- (p1 - p2) / SE
print(t_statistic)

# Calculate the p-value
p_value <- 2 * pt(abs(t_statistic), df = n1 + n2 - 2, lower.tail = FALSE)
print(p_value)

###Using t.test()
# Define the sample proportions and sample sizes
p1 <- 0.12
p2 <- 0.04
n1 <- 6000
n2 <- 6000

# Run the t-test
result <- t.test(x = c(rep(1, n1 * p1), rep(0, n1 * (1 - p1))), 
                 y = c(rep(1, n2 * p2), rep(0, n2 * (1 - p2))), 
                 var.equal = TRUE)

# Extract the t-statistic and p-value from the result
t_statistic <- result$statistic
print(t_statistic)
p_value <- result$p.value
p_value

#Answer:d
#In order to reject the null, 
#we have to compare the p-value to a significance level alpha(5%), 
#If the p-value is less than alpha, we reject the null and conclude that there is evidence of a 
#difference in the proportions of callbacks received.

#Answer:e
#Based on the result of the hypothesis test,it can be concluded that there is 
#evidence of discrimination in labor markets against African American names in the study.

#ANSWER-5
library(pwr)
pwr.2p.test(h = 0.03,n=, sig.level = 0.05, power = 0.8, alternative = "greater")
#Therefore, the sample size needed is 13739 for both the groups.






