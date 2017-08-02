# **************************************************************************************
# File        : guestfirst-case-b.R  <--- this should be the exact name of THIS document
# Author      : Kristina Tobio 
# Created     : 02 Aug 2017
# Modified    : 02 Aug 2017
# Description : R file for guestfirst Case Part B 
# *************************************************************************************/

# This clears R's data memory so new data can be loaded

rm(list = ls())

# log file
sink(file = paste0("./logs/guestfirst-case-b_", Sys.Date(),".log"), append = FALSE, type = c("output", "message"), split = TRUE)

# NA: In R, working directory set by use of R project. 
# Will use following to list working directory and contents: 

getwd()
list.files()

#install.packages("dplyr")
library(dplyr)

#install.packages('Hmisc')
library('Hmisc')

#install.packages("haven")
library(haven)

#install.packages("readstata13")
library("readstata13")

install.packages("data.frame")
library("data.frame")

guestfirstB.case.data <- read.csv("data/guestfirst-case-data-part-b.csv")
save("guestfirstB.case.data", file = "data/guestfirstB_data")

load("data/guestfirstB_data")

#Testing for Lagged Effects at GuestFirst
guestfirstB.case.data$LoyalLag<-shift(guestfirstB.case.data$Loyal, n=1L, fill=NA, type=c("lag"), give.names=FALSE)


#dt<-data.frame(guestfirstB.case.data)
#dt[, shift(guestfirstB.case.data$Loyal, 1, 0, "lead")]
dt <- data.table(guestfirstB.case.data)
dt[,Loyal_lag2:=shift(Loyal, 1), by=Property]
#dt[,Loyal_lag1:=shift(Loyal, 1)]
#lagpad <- function(x, k) c(rep(NA, k), x)[1:length(x)] 
#dt[,indpct_slow:=(ind/lagpad(ind, 1))-1, by=entity]

dt[,mpg_forward1:=shift(mpg, 1, type='lead')]
head(dt)

#dt[, shift(guestfirstB.case.data$Loyal, n=1L, fill=NA, type=c("lag"))]
#dt[, shift(guestfirstB.case.data$Loyal, n=1L, fill=NA, type=c("lag")), by=guestfirstB.case.data$Property]


#Walking through the Teaching Notes
#Part 1: Relationship Between Loyalty (loyal) and Revenue per Available Room (revpar) in Each Year
#We could run four separate regressions
fit <- lm(RevPar ~ Loyal,  data = guestfirstB.case.data[guestfirstB.case.data$Year == 1996,])
summary(fit)

fit <- lm(RevPar ~ Loyal,  data = guestfirstB.case.data[guestfirstB.case.data$Year == 1997,])
summary(fit)

fit <- lm(RevPar ~ Loyal,  data = guestfirstB.case.data[guestfirstB.case.data$Year == 1998,])
summary(fit)

fit <- lm(RevPar ~ Loyal,  data = guestfirstB.case.data[guestfirstB.case.data$Year == 1999,])
summary(fit)

fit <- lm(RevPar ~ Loyal,  data = guestfirstB.case.data[guestfirstB.case.data$Year == 2000,])
summary(fit)

# Do things change if we control for the competitors' revenue?
fit <- lm(RevPar ~ Loyal + CompRevPar,  data = guestfirstB.case.data[guestfirstB.case.data$Year == 1996,])
summary(fit)

fit <- lm(RevPar ~ Loyal + CompRevPar,  data = guestfirstB.case.data[guestfirstB.case.data$Year == 1997,])
summary(fit)

fit <- lm(RevPar ~ Loyal + CompRevPar,  data = guestfirstB.case.data[guestfirstB.case.data$Year == 1998,])
summary(fit)

fit <- lm(RevPar ~ Loyal + CompRevPar,  data = guestfirstB.case.data[guestfirstB.case.data$Year == 1999,])
summary(fit)

fit <- lm(RevPar ~ Loyal + CompRevPar,  data = guestfirstB.case.data[guestfirstB.case.data$Year == 2000,])
summary(fit)

#What happens when we combine all four years of data?
fit <- lm(RevPar ~ Loyal + CompRevPar,  data = guestfirstB.case.data)
summary(fit)

#What if we add dummy variables for each site to our regression?
#Note: This dataset already includes site dummies, but if they didn't, we could create them using the following code:

guestfirstB.case.data$Property = factor(guestfirstB.case.data$Property)
fit <- lm(RevPar ~ Loyal + CompRevPar + Property,  data = guestfirstB.case.data)
summary(fit)

# This clears R's data memory so new data can be loaded

rm(list = ls())

# The following command redirects output from the log file back to the console, if necessary.

sink()





    

