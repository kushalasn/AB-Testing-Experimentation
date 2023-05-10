#*** Causal Analytics and AB Testing Course at UT Dallas***#
#*** Compiled by Amit Mehra ***#
#***ITT and LATE***#
#***Note that this code does not show cluster corrected SE. However, this correction does not create any meaningful difference***#

install.packages('AER')
library(AER)
library(data.table)
library(stargazer)


#*** Load Synthetized Dataset from a Randomized Experiment with TSTV ***# 



MyData <- fread("C:/Users/kusha/OneDrive/Desktop/Spring 23/Casual Analytics and AB Testing/TSTV-Exp.csv")
MyData


#*** BASIC DESCRIPTIVE STATISTICS ***# 
min(MyData$week)
max(MyData$week)
unique(MyData$week)
length(unique(MyData$id))
length(unique(MyData$id[MyData$treated_tstv==TRUE]))
length(unique(MyData$id[MyData$treated_tstv==FALSE]))
min(unique(MyData$week[MyData$after==TRUE]))
max(unique(MyData$week[MyData$after==FALSE]))

#*** check balance ***#
t.test(MyData$view_time_total_hr[MyData$treated_tstv==0 & MyData$after==0], MyData$view_time_total_hr[MyData$treated_tstv==1 & MyData$after==0], alternative = c("two.sided"))
t.test(MyData$view_time_live_hr[MyData$treated_tstv==0 & MyData$after==0], MyData$view_time_live_hr[MyData$treated_tstv==1 & MyData$after==0], alternative = c("two.sided"))
t.test(MyData$view_time_tstv_hr[MyData$treated_tstv==0 & MyData$after==0], MyData$view_time_tstv_hr[MyData$treated_tstv==1 & MyData$after==0], alternative = c("two.sided"))

#*** ITT Effect***#
itt.total.tv <- lm(view_time_total_hr ~ treated_tstv + factor(week), data = MyData[after==TRUE])
itt.live.tv <- lm(view_time_live_hr ~ treated_tstv + factor(week), data = MyData[after==TRUE])
itt.tstv.tv <- lm(view_time_tstv_hr ~ treated_tstv + factor(week), data = MyData[after==TRUE])

#*** Show Results

stargazer(
  itt.total.tv, itt.live.tv, itt.tstv.tv,
  omit = c('week'),
  dep.var.labels = c( 'Total TV', 'Live TV', 'Time-Shift TV'),
  type = "text",
  omit.stat=c("f", "ser", "rsq")
)


#*** Using treatment assignment as IV for treatment compliance yields LATE***#

#*** Check Compliance Levels (with using 90 minutes) ***#
MyDataComp<-MyData[after==TRUE, length(unique(id)), by = list(treated_tstv, used90)][order(treated_tstv, used90)]
names(MyDataComp) <- c('treated_tstv', 'used90','n')

#*** FIRST STAGE JUST TO CONFIRM THAT TREATMENT ASSIGNMENT LEADS TO TSTV USAGE ***#
late.first <-lm(used90 ~ treated_tstv + factor(week), data = MyData[after==TRUE]) 


stargazer(
  late.first,
  omit = c('week'),
  type = "text",
  omit.stat=c("f", "ser", "rsq")
)

#*** Regression with IV in the after period to get LATE ***#
late.total.tv <- ivreg(view_time_total_hr ~ used90 + factor(week) | treated_tstv + factor(week), data = MyData[after==TRUE])
late.live.tv <- ivreg(view_time_live_hr  ~ used90 + factor(week) | treated_tstv + factor(week), data = MyData[after==TRUE])
late.tstv.tv <- ivreg(view_time_tstv_hr  ~ used90 + factor(week) | treated_tstv + factor(week), data = MyData[after==TRUE]) 



#*** Show Results
stargazer(
  itt.total.tv, itt.live.tv, itt.tstv.tv,
  late.total.tv,
  late.live.tv,
  late.tstv.tv,
  omit = c('week'),
  type = "text",
  omit.stat=c("f", "ser", "rsq")
)

