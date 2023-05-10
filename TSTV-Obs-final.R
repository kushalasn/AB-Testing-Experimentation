#*** A/B Testing, Analysis and Implementation ***#
#*** Causal Analytics and AB Testing Course at JSOM, Univ of Texas Dallas ***#
#*** Developed by Amit Mehra, updated Oct/2020 ***#


rm(list=ls());gc()
library(data.table)
library(stargazer)
library(plm)

install.packages('MatchIt')
library(MatchIt)


#**** LOAD OBSERVATIONAL DATASET ***#
MyData<-fread(input='/Users/AMehra/Dropbox/PROFESSIONAL/TEACHING/UTD/MSIS/CausalAnalyticsSpring2023Online/Lectures/Session 10/TSTV-Obs-Dataset.csv')

#*** CREATE A SUMMARY DATASET BEFORE AND AFTER TSTV IS AVAILABLE ***#
MyDataSummary <- MyData[,list(
                              premium = premium[1],
                             view_time_total_hr = mean(view_time_total_hr),
                              view_time_live_hr  = mean(view_time_live_hr),
                              view_time_tstv_hr = mean(view_time_tstv_hr)),by=list(id, after)]

#*** BASIC DESCRIPTIVE STATISTICS ***# 

min(MyData$week)
max(MyData$week)
length(unique(MyData$id[MyData$premium==TRUE]))
length(unique(MyData$id[MyData$premium==FALSE]))
min(unique(MyData$week[MyData$after==TRUE]))
max(unique(MyData$week[MyData$after==FALSE]))

# average stats before and after TSTV
MyTempData <- MyDataSummary[,lapply(list(view_time_total_hr=view_time_total_hr,
                                  view_time_live_hr=view_time_live_hr,
                                  view_time_tstv_hr=view_time_tstv_hr),
                             function(x) mean(x[!is.nan(x) &!is.na(x)])),
                     by=list(premium,after)]

# Differences in differences summary dataset
did.view_time_total_hr.after.premium.sum       <- lm(view_time_total_hr ~ premium + premium:after + after,  data=MyDataSummary)
did.view_time_live_hr.after.premium.sum        <- lm(view_time_live_hr ~ premium + premium:after + after,  data=MyDataSummary)
did.view_time_tstv_hr.after.premium.sum        <- lm(view_time_tstv_hr ~ premium + premium:after + after,  data=MyDataSummary)

# Output differences in differences from summary dataset
stargazer(
  did.view_time_total_hr.after.premium.sum,
  did.view_time_live_hr.after.premium.sum,
  did.view_time_tstv_hr.after.premium.sum,
  title="View Time as a function of TSTV",
  dep.var.labels=c('Total', 'Live', 'TS'),
  covariate.labels=c("Premium", "After","Premium*After"),
  type="text")

# Differences in differences weekly panel
did.view_time_total_hr.after.premium       <- lm(view_time_total_hr ~ premium + premium:after + factor(week),  data=MyData)
did.view_time_live_hr.after.premium        <- lm(view_time_live_hr ~ premium + premium:after + factor(week),  data=MyData)
did.view_time_tstv_hr.after.premium        <- lm(view_time_tstv_hr ~ premium + premium:after + factor(week),  data=MyData)


# Differences in differences weekly panel
did.view_time_total_hr.after.premium       <- plm(view_time_total_hr ~ premium + premium:after + factor(week), data=MyData, index=c("week"), model="within")
did.view_time_live_hr.after.premium        <- plm(view_time_live_hr ~ premium + premium:after + factor(week),  data=MyData, index=c("week"), model="within")
did.view_time_tstv_hr.after.premium        <- plm(view_time_tstv_hr ~ premium + premium:after + factor(week),  data=MyData, index=c("week"), model="within")


# Output differences in differences with summary dataset and weekly panel
stargazer(
  did.view_time_total_hr.after.premium.sum,
  did.view_time_live_hr.after.premium.sum,
  did.view_time_tstv_hr.after.premium.sum,
  did.view_time_total_hr.after.premium,
  did.view_time_live_hr.after.premium,
  did.view_time_tstv_hr.after.premium,
  title="View Time as a function of TSTV",
  omit.stat=c("ser", "f", "adj.rsq"),
  dep.var.labels=c('Total', 'Live', 'TS', 'Total', 'Live', 'TS'),
  covariate.labels=c("Premium", "After","Premium*After"),
  omit = c('factor'),
  omit.labels = c('Week Dummies'),
  type="text")

# Differences in differences using two fixed effects for household and week - note that the time variable is specified in the second place for the index option

fe.view_time_total_hr.after.premium <- plm(view_time_total_hr ~ premium + premium:after + factor(week), data=MyData, index=c("id", "week"), model="within")
fe.view_time_live_hr.after.premium  <- plm(view_time_live_hr ~  premium + premium:after + factor(week), data=MyData, index=c("id", "week"), model="within")
fe.view_time_tstv_hr.after.premium  <- plm(view_time_tstv_hr ~  premium + premium:after + factor(week),  data=MyData, index=c("id", "week"), model="within")


#another way of writing two fixed effects
#fe.view_time_total_hr.after.premium <- plm(view_time_total_hr ~ premium + premium:after , data=MyData, index=c("id", "week"), model="within", effect="twoways")
#fe.view_time_live_hr.after.premium  <- plm(view_time_live_hr ~  premium + premium:after , data=MyData, index=c("id", "week"), model="within", effect="twoways")
#fe.view_time_tstv_hr.after.premium  <- plm(view_time_tstv_hr ~  premium + premium:after , data=MyData, index=c("id", "week"), model="within", effect="twoways")

# Output differences in differences and fixed effects
stargazer(
  fe.view_time_total_hr.after.premium,
  fe.view_time_live_hr.after.premium,
  fe.view_time_tstv_hr.after.premium,
  did.view_time_total_hr.after.premium,
  did.view_time_live_hr.after.premium,
  did.view_time_tstv_hr.after.premium,
  se=list(
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
  ),
  title="View Time as a function of TSTV",
  omit.stat=c("ser", "f", "adj.rsq"),
  dep.var.labels=c('Total', 'Live', 'TS', 'Total', 'Live', 'TS'),
  covariate.labels=c("Premium", "Premium*After"),
  omit = c('factor'),
  omit.labels = c('Week Dummies'),
  column.labels = c('Total FE','Live FE','TSTV FE','Total DiD','Live DiD','TSTV DiD'),
  type="text")


#*** Propensity Score Matching ***#

#*** SET A FIXED SEED IN ORDER TO OBTAIN ALWAYS THE SAME RESULTS ***#
set.seed(1979)

#*** MATCH TREATED AND CONTROL HOUSEHOLDS ON LIKELIHOOD OF TREATMENT (being premium) ***#
# Note: the matchit command may take a long time to run with large datasets
Match <- matchit(premium ~ view_time_total_hr , data = MyDataSummary[after == FALSE], method = 'nearest', caliper=0.001)

#*** SUMMARIZE QUALITY OF THE MATCHED SAMPLE ***#
summary(Match)

#*** IDENTIFY MATCHED SAMPLE ***#
MyDataSummary.match <- data.table(match.data(Match))
Matched.ids        <- MyData$id %in% MyDataSummary.match$id
MyData[, match := Matched.ids]

#*** RUN DIFFERENCES IN DIFFERENCES ONLY IN THE MATCHED SAMPLE ***#
did.view_time_total_hr.after.premium.psm <- lm(view_time_total_hr ~ premium + premium:after + factor(week), data=MyData[match == TRUE])
did.view_time_live_hr.after.premium.psm <- lm(view_time_live_hr  ~ premium + premium:after + factor(week), data=MyData[match == TRUE])
did.view_time_tstv_hr.after.premium.psm <- lm(view_time_tstv_hr  ~ premium + premium:after + factor(week), data=MyData[match == TRUE])

#*** did.fe.view_time_total_hr.after.premium.psm <- plm(view_time_total_hr ~ premium + premium:after + factor(week), data=MyData[match == TRUE]) for a within model***#

#*** SHOW RESULTS FROM DIFFERENCE IN DIFFERENCE WITHOUT PSM AND WITH PSM ***#
stargazer(
  did.view_time_total_hr.after.premium,
  did.view_time_live_hr.after.premium,
  did.view_time_tstv_hr.after.premium,
  did.view_time_total_hr.after.premium.psm,
  did.view_time_live_hr.after.premium.psm,
  did.view_time_tstv_hr.after.premium.psm,
  se=list(
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
  ),
  title="View Time as a function of TSTV",
  omit.stat=c("LL","ser","f", "adj.rsq"),
  dep.var.labels=c('Total', 'Live', 'Time-shift', 'Matched Total', 'Matched Live', 'Matched Time-shift'),
  covariate.labels=c("Premium", "Premium * After"),
  notes.align="c",
  omit = c('factor'),
  omit.labels = c('Week Dummies'),
  type="text")
