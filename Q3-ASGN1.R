# Define the sample proportions
p1 <- 0.12
p2 <- 0.04

# Define the sample sizes
n1 <- 6000
n2 <- 6000

# Calculate the standard error
SE <- sqrt((p1 * (1 - p1) / n1) + (p2 * (1 - p2) / n2))
p=(p1+p2)/(n1+n2)

#SE1<-sqrt(p*(1-p)*(1/n1+1/n2))

# Calculate the t-statistic
t <- (p1 - p2) / SE



# Print the results
cat("t-statistic:", t, "\n")
t <- 16.32
df <- n1 + n2 - 2
p_value <- 2*pt(t, df, lower.tail = FALSE)
p_value






