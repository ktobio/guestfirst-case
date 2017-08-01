# **************************************************************************************
# File        : guestfirst-case-a.R  <--- this should be the exact name of THIS document
# Author      : Kristina Tobio 
# Created     : 31 Jul 2017
# Modified    : 01 Aug 2017
# Description : R file for guestfirst Case Part A 
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

guestfirstA.case.data <- read.csv("data/guestfirst-case-data-part-a.csv")
save("guestfirstA.case.data", file = "data/guestfirstA_data")

# The following command reads the PA_Store24B_data data frame, previously saved to file in the 
# "store24-prep.r" script.

load("data/guestfirstA_data")

contents(guestfirstA.case.data)

# The following command lists the first 5 rows of the guestfirstA.case.data".

head("data/guestfirstA_data",n=5L)

guestfirstA.case.data <- cbind(guestfirstA.case.data, "observation"=1:nrow(guestfirstA.case.data)) 

#install.packages("reshape")
library("reshape")
md <- melt(mydata, id=(c("id", "time")))
guestfirstA.transpose.case.data = reshape(data = guestfirstA.case.data,
                                          idvar = "observation",
                                          varying = c("proper1997", "year1997", "revpar1997", "comprev1997", "loyal1997",
                                                      "proper1998", "year1998", "revpar1998", "comprev1998", "loyal1998",
                                                      "proper1999", "year1999", "revpar1999", "comprev1999", "loyal1999",
                                                      "proper2000", "year2000", "revpar2000", "comprev2000", "loyal2000"),
                                          sep = "",
                                          timevar = "year",
                                          times = c(1997,1998,1999,2000),
                                          direction = "long")
                                  

guestfirstB.case.data <- read.csv("data/guestfirst-case-data-part-b.csv")
save("guestfirstB.case.data", file = "data/guestfirstB_data")

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





    

