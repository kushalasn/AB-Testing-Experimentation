#Impact of website design (A or B) on sales

# load the data
df <- read.csv("C:/Users/kusha/OneDrive/Desktop/Spring 23/Casual Analytics and AB Testing/exp_sim_data.csv") 
df

# run a t-test
t.test(df$sales[df$website=="B"],df$sales[df$website=="A"]) 

# regress design on sales
summary(lm(sales~website,data=df)) 

# regress design on log sales
summary(lm(log(sales)~website,data=df)) 


#Impact of years of education on hourly wage

# load the data
df <- read.csv("C:/Users/kusha/OneDrive/Desktop/Spring 23/Casual Analytics and AB Testing/wages_1985.csv") 
df

#regress wage on education 
summary(lm(wages ~ education, data = df))

# regress wage on education while controlling for individual characteristics
summary(lm(wages ~ education + workexp + unionmember + south + occupation + female, data = df))
#regress wage on education with an interaction term (education*south)
summary(lm(wages ~ education + education*south, data = df))



print("hello")
