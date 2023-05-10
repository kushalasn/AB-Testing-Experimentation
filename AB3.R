library(haven)
library(pROC)
mydata <- read_sas("C:/Users/kusha/OneDrive/Desktop/Spring 23/Predictive with SAS/heinzhunts.sas7bdat")


mydata$LogPriceRatio <- log(mydata$PRICEHEINZ/mydata$PRICEHUNTS)

set.seed(10)
n <- nrow(mydata)
train <- sample(seq_len(n), size = round(0.7 * n))
test <- setdiff(seq_len(n), train)

train_data <- mydata[train, ]
test_data <- mydata[test, ]


# Estimate a logit probability model using the training data
model <- glm(HEINZ ~ LogPriceRatio + DisplHeinz + FeatHeinz + DisplHunts + 
               FeatHunts + DisplHeinz:FeatHeinz + DisplHunts:FeatHunts, 
             data = train_data, family = binomial(link = "logit"))

# Print the summary of the model
summary(model)



pr<-predict(model,newdata = test_data)
pr
probs<-plogis(pr)
probs

actualvalue=test_data$HEINZ
roc_data <- roc(actualvalue,probs,levels = c(0,1))
roc_data$thresholds
auc(roc_data)
thresholds <- seq(0, 1, 0.01)

false_positives <- (1 - roc_data$specificities) * sum(test_data$HUNTS == 0)
false_negatives <- roc_data$sensitivities * sum(test_data$HUNTS == 1)
total_cost <- false_positives * 1.5 + false_negatives * 0.25
total_cost
optimal_threshold <- thresholds[which.min(total_cost)]
optimal_threshold

data=read.csv("C:/Users/kusha/OneDrive/Desktop/Test/data_science_challenge/data_science_challenge/scatter.csv")
data

linear_model <- lm(y ~ x*(-0.1), data=data)

# view summary of linear model
summary(linear_model)









