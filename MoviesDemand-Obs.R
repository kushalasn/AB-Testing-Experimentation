library(data.table)
library(stargazer)
library(ggplot2)

#*** Load Dataset ***#

MyData <- read.csv('C:/Users/kusha/OneDrive/Desktop/Spring 23/Casual Analytics and AB Testing/MovieData-Obs.csv')
MyData

#*** Regress leases on price (using linear demand for sake of simplicity) ***#

ols <-lm(leases ~ price+likes+factor(year_release), data = MyData)
ols
stargazer(ols,title="OLS leases on price",type="text",column.labels=c("price"))

#*** Plot estimated demand curve ***#

plot(MyData$leases, MyData$price, xlab="leases", ylab="price", xlim=c(0,400), ylim=c(1.00,4.50), cex.axis=1.5, cex.lab=1.5)
abline(lm(price~ leases , data = MyData))


#*** Regress leases on price and likes (linear and log) ***#

olslikes <-lm(leases ~ price + likes, data = MyData)
stargazer(ols,olslikes,title="OLS leases on price and likes",type="text",column.labels=c("price","incl. likes"))

#*** Regress leases on price and likes and year release ***#

olsyear <-lm(leases ~ price + likes + year_release, data = MyData)
stargazer(ols,olslikes,olsyear,title="OLS leases on price, likes and year release",type="text",column.labels=c("price","",""))


#*** Regress leases on price and likes and year release dummies ***#

olsyeardummies <-lm(leases ~ price + likes + factor(year_release), data = MyData)
stargazer(ols,olslikes,olsyeardummies,title="OLS leases on price, likes and year release dummies",
          type="text",column.labels=c("price","","year release dummies"),omit="year_release")


#*** Plot Results in terms of Demand Curve ***#
plot(MyData$leases, MyData$price, xlab="leases", ylab="price", xlim=c(0,400), ylim=c(1.00,4.50), cex.axis=1.5, cex.lab=1.5)
abline(lm(price~ leases + likes + factor(year_release), data = MyData))



olsyeardummies <-lm(price ~ leases + likes + factor(year_release), data = MyData)
stargazer(ols,olslikes,olsyeardummies,title="OLS leases on price, likes and year release dummies",
          type="text",column.labels=c("-","","year release dummies"),omit="year_release")
