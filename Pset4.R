#install.packages( c ("sandwich" , "lmtest"))

library("readxl")
library(AER)
library(dplyr)
library(tidyr)
library(ggplot2)

library(sandwich)
library(lmtest)

df <- read_xlsx("GitHub/Econ-21020/birthweight_smoking.xlsx")

# part (a)
# regress Birthweight on Smoker
fit_lm <- lm ( birthweight ~ smoker, data = df )
summary ( fit_lm ) # get coefficient values and standard errors

# part (b)
# regress Birthweight on Smoker, Alcohol, and Nprevist
fit_lm2 <- lm ( birthweight ~ smoker + alcohol + nprevist, data = df )
summary ( fit_lm2 )

# robust standard errors
coeftest (fit_lm2 , vcov = vcovHC (fit_lm2 , type = "HC1"))

# part (c) Frisch-Waugh Theorem Verification
# regress Smoker on Alcohol and Nprevist
fit_lm3 <- lm ( smoker ~ alcohol + nprevist, data = df )
# obtain the residuals
smoker_resid <- resid(fit_lm3)

# regress Birthweight on Alcohol and Nprevist
fit_lm4 <- lm ( birthweight ~ alcohol + nprevist, data = df )
# obtain the residuals
birthweight_resid <- resid(fit_lm4)

# regress birthweight_resid on smoker_resid
fit_lm5 <- lm (birthweight_resid ~ smoker_resid)
coef(fit_lm5) # coefficient for smoker_resid matches 217.580 from part (b)


