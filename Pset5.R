# install.packages( c ("sandwich" , "lmtest"))

library("readxl")
library(AER)
library(dplyr)
library(tidyr)
library(ggplot2)

library(sandwich)
library(lmtest)

# ***question 4***
df <- read_xlsx("cps04.xlsx")

# part (a)
# regress AHE on Female, College, and HS
fit_lm <- lm ( AHE ~ Female + College + HS, data = df )
# robust standard errors
coeftest (fit_lm , vcov = vcovHC (fit_lm , type = "HC1"))

# calculating test statistic for part (iii)
beta_hat <- coef(fit_lm)
# covariance matrix
cov_mat <- vcovHC(fit_lm, type = "HC1")
c <- 0
r <- c(0, 0, 1, -1)

# test statistic
(t(r) %*% beta_hat - c) / sqrt(t(r) %*% cov_mat %*% r)

# part (b)
# regress AHE on Female, College, and HS+College
df$hsandcollege=df$HS+df$College
fit_lm2 <- lm ( AHE ~ Female + College + hsandcollege, data = df )
# robust standard errors
coeftest (fit_lm2 , vcov = vcovHC (fit_lm2 , type = "HC1"))

# part (c)
# regress AHE on Female, College, Female x College, HS, and Female x HS
df$femtimescollege = df$Female*df$College
df$femtimeshs = df$Female*df$HS
fit_lm3 <- lm ( AHE ~ Female + College + femtimescollege + HS + femtimeshs, data = df )
# robust standard errors
coeftest (fit_lm3 , vcov = vcovHC (fit_lm3 , type = "HC1"))

# testing hypothesis that there are no interactions between sex and education
c2 <- matrix(c(0,0), nrow = 2)
r2 <- matrix(c(0,0,0,0,0,0,1,0,0,0,0,1), nrow = 2)

linearHypothesis(fit_lm3, r2, c2, vcov = vcovHC (fit_lm3 , type = "HC1"), test = "Chisq")

# ***question 5***
df2 <- read_xlsx("fertility.xlsx")

# part (a)
# regress weeksworked on morekids
fit_lm4 <- lm(weeksm1 ~ morekids, data=df2)
# robust standard errors
coeftest (fit_lm4 , vcov = vcovHC (fit_lm4 , type = "HC1"))

# part (c)
# regress morekids on samesex
fit_lm5 <- lm(morekids ~ samesex, data=df2)
# robust standard errors
coeftest (fit_lm5 , vcov = vcovHC (fit_lm5 , type = "HC1"))

# part (e)
# We will use ivreg to regress weeksworked on morekids using the instrument
# samesex
iv_regsamesex <- ivreg(weeksm1 ~ morekids | samesex, data=df2)
# robust standard errors
coeftest (iv_regsamesex, vcov = vcovHC (iv_regsamesex , type = "HC1"))

# part (f)
# Include agem1, black, hispan, and othrace as control variables in 
# the iv regression
# We will use ivreg to run this regression
iv_regcontrols <- ivreg(weeksm1 ~ morekids + agem1 + black + hispan + othrace 
                       | samesex + agem1 + black + hispan + othrace, data=df2)
# robust standard errors
coeftest (iv_regcontrols, vcov = vcovHC (iv_regcontrols , type = "HC1"))


