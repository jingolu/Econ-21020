# R script for a single linear restriction 
# Tom Hierons (with thanks to Thomas Wiemann)

# Preliminaries ----------------------------------------------------------------

# Packages
library(lmtest)
library(sandwich)
library(AER)

# Simulate some data 
nobs <- 1000
X <- matrix(rnorm(3 * nobs), nobs, 3)
y <- 2 + X %*% c(1, 2, 3) + rnorm(nobs, 0, 1)

# Regression and Test ----------------------------------------------------------

# ols of y on a constant, X.1, X.2, and X.3
ls_fit <- lm(y ~ X)
summary(ls_fit)

# test the hypothesis that 2 * \beta_1 = \beta_2.
c <- 0
r <- c(0, 2, -1, 0)
linearHypothesis(ls_fit, r, c, vcov = vcovHC(ls_fit, type = "HC1"),
                 test = "Chisq")

# for a one sided test, need to calculate the test statistic manually
beta_hat <- ls_fit$coefficients
cov_mat <- vcovHC(ls_fit, type = "HC1")
T_n <- (t(r) %*% beta_hat - c) / sqrt(t(r) %*% cov_mat %*% r)
T_n

# You can then compare T_n to the (one or two sided) critical values 
# for a standard normal. 

# Notice that the linearHypothesis function returns the Chi-squared test 
# statistic, not the Z-test statistic. For one-sided testing you will need 
# the Z-test statistic as the sign of T_n matters! 
# The two are related! In particular the following will give the 
# same number as T_n:

sqrt(linearHypothesis(ls_fit, r, c, vcov = vcovHC(ls_fit, type = "HC1"),
                      test = "Chisq")$Chisq[2])

