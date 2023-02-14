# package with data
library(AER)
#setwd("/home/trhierons/Documents/GitHub/ECON-21020/R-scripts/")
setwd("~/GitHub/ECON-21020/R-scripts/")

# load the data
wagedf <- read.csv("wagedata.csv")
wagedf$union <- wagedf$union == "yes"

# run ols
fit_lm <- lm ( wage ~ 1 + education + union , data = wagedf )
summary ( fit_lm ) # get coefficient values and standard errors

# to get robust standard errors:

# install packages only the first time .
install.packages( c ("sandwich" , "lmtest"))

library(sandwich)
library(lmtest)

coeftest (fit_lm , vcov = vcovHC (fit_lm , type = "HC1"))
