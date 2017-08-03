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
dt = dt[order(dt$Year)]
dt[, Loyal_lag3:=shift(Loyal, 1), by=Property]
#dt[,Loyal_lag1:=shift(Loyal, 1)]
#lagpad <- function(x, k) c(rep(NA, k), x)[1:length(x)] 
#dt[,indpct_slow:=(ind/lagpad(ind, 1))-1, by=entity]


#dt[, shift(guestfirstB.case.data$Loyal, n=1L, fill=NA, type=c("lag"))]
#dt[, shift(guestfirstB.case.data$Loyal, n=1L, fill=NA, type=c("lag")), by=guestfirstB.case.data$Property]

# Step 4: Run the regression analysis with and without the lagged variables
# Creating the dummy variables for each property
guestfirstB.case.data$Property = factor(guestfirstB.case.data$Property)
fit <- lm(RevPar ~ Loyal + CompRevPar + Property,  data = guestfirstB.case.data)
summary(fit)
# Adding the lagged variable
fit <- lm(RevPar ~ Loyal + CompRevPar + Property + Loyal_lag3,  data = guestfirstB.case.data)
summary(fit)


# This clears R's data memory so new data can be loaded

rm(list = ls())

# The following command redirects output from the log file back to the console, if necessary.

sink()





    

