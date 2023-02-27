# uncomment this line if you haven't installed readxl yet
# install.packages("readxl")
library(readxl)
library(AER) # for instrumental variables regression
library(lmtest) # for hypothesis testing
library(sandwich) # for robust standard errors

# be sure to set working directory
# setwd("/Users/samuelhigbee/GitHub/ECON-21020/R-scripts/")
setwd("/home/trhierons/Documents/GitHub/ECON-21020/R-scripts/")

# read in the data
df <- read_excel("birthweight_smoking.xlsx")

#------------------------------------------------------------------------------#
# Testing linear restrictions
#------------------------------------------------------------------------------#

# linear regression
fit_lm <- lm(birthweight ~ 1 + smoker + alcohol + nprevist, data=df)
summary(fit_lm)

# robust standard errors
coeftest(fit_lm, vcov=vcovHC(fit_lm, type="HC1"))

# testing linear restriction
lht(fit_lm, 
    hypothesis.matrix=c(0, 1, 0, 8),
    rhs=0,
    test="F",
    vcov=vcovHC(fit_lm, type="HC1"))

# for help (remember google as well)
?coeftest
?lht

#------------------------------------------------------------------------------#
# Instrumental variables
#------------------------------------------------------------------------------#

# Y = birthweight
# D = smoker (endogenous) 
# Z = educ (instrument)
# X = age (control)

# estimate first stage-- is it relevant?
fs <- lm(smoker ~ 1 + educ + age, data=df)
summary(fs)
# is it exogenous?

# extract predicted smoker
df$smoker_hat <- fs$fitted.values

# regress birthweight on smoker_hat
ss <- lm(birthweight ~ 1 + smoker_hat + age, data=df)
summary(ss)

# much easier to use ivreg-- plus standard errors will be correct
fit_iv <- ivreg(birthweight ~ 1 + smoker + age | educ + age, data=df)
summary(fit_iv)
# coefficient is the same-- good

coeftest(fit_iv, vcov=vcovHC(fit_iv, type="const")) # same as above
coeftest(fit_iv, vcov=vcovHC(fit_iv, type="HC1")) 

# for help
?ivreg

#------------------------------------------------------------------------------#
# FWL 
#------------------------------------------------------------------------------#

# regress birthweight on X_1, X_2, X_3, and a constant
lm_full <- lm(birthweight ~ 1 + smoker + alcohol + nprevist, data=df)
# extract coefficient
beta <- lm_full$coefficients["smoker"]

# regress X_1 on X_2, X_3 and a constant
lm_smoker <- lm(smoker ~ 1 + alcohol + nprevist, data=df)
smoker_resid <- lm_smoker$residual
# let's add it as a comlumn of the data frame
df$smoker_resid <- smoker_resid

#regress birthweight on smoker_resid
lm_resid <- lm(birthweight ~ 1 + smoker_resid, data=df)
beta_resid <- lm_resid$coefficients["smoker_resid"]

# NOT an asymptotic argument-- holds exactly in finite samples!
# It's just linear algebra




