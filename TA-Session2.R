### Statistics in R ---------------------------------------------------

# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("tidyverse")

library(AER)
library(dplyr)
library(tidyr)
library(ggplot2)

# cheatsheets-- very very very useful
# https://www.rstudio.com/resources/cheatsheets/

# working directory
getwd()
list.files()
setwd("/Documents/GitHub/ECON-21020") # autocomplete
# setwd("/home/trhierons/Documents/GitHub/ECON-21020/R-scripts/")
list.files()

# generating random samples
x <- rnorm(1000, mean=0, sd=1)
x <- runif(500, min=0, max=10)
x <- rbinom(100, 1, 0.5)

# calculating basic statistics
mean(x)
var(x)
sd(x)
quantile(x, probs=c(0.05, 0.5, 0.95))

# test whether a coin is unbiased
n <- 1000
p <- 0.5
x <- rbinom(n=n, size=1, prob=p)

# calculate test statistic
ts <- sqrt(n) * (mean(x) - 0.55) / sd(x)
2 * pnorm(q = -abs(ts), mean=0, sd=1)

# using dataframes
data("CPS1985") # included in AER package
write.csv(CPS1985, file="wagedata.csv")

df <- read.csv("wagedata.csv")
unique(df$union)
df$union <- df$union == "yes"

# summary statistics
summary(df)
min(df$wage) # in case you want to do it manually
mean(df[df$union==1, "wage"])
meanbyunion <- df %>% group_by(union) %>% summarise(meanwage = mean(wage))

# visualization
hist(df[df$union==1, "wage"]) # ugly and lazy!
hist(df[df$union==0, "wage"])

wagehist <- ggplot(df, aes(x=wage,
                           fill=union)) + 
  geom_histogram(binwidth=2,
                 position="identity",
                 color="white", alpha=0.45) +
  scale_fill_manual(name="Union",
                    values=c("orange", "steelblue"),
                    labels=c("No", "Yes")) +
  xlab("Wage") +
  ylab("Frequency") # use the cheatsheet!

wagehist
ggsave("wagehist.png")

wagedens <- ggplot(df, aes(x=wage,
                           fill=union)) + 
  geom_density(position="identity",
               color="white", alpha=0.45) +
  scale_fill_manual(name="Union", 
                    values=c("orange", "steelblue"),
                    labels=c("No", "Yes")) +
  xlab("Wage") +
  ylab("Density") 

wagedens

# two-sample test for equality of means
n1 <- length(df[df$union==1, "wage"])
n0 <- length(df[df$union==0, "wage"])
m1 <- mean(df[df$union==1, "wage"])
m0 <- mean(df[df$union==0, "wage"])
s1 <- sd(df[df$union==1, "wage"])
s0 <- sd(df[df$union==0, "wage"])
ts <- (m1 - m0) / sqrt(s1^2/n1 + s0^2/n0)
2 * pnorm(q = -abs(ts), mean=0, sd=1)

# Simulation-- coverage of confidence intervals
cisim <- function(S, N, mu, sigma, plotS) {
  # S: number of simulations to run
  # N: number of draws each simulation
  # mu: data drawn from normal with this mean
  # sigma: data drawn from normal with this sd
  # plotS: number of draws to plot. Must be <= S
  cis <- array(data=NA, dim=c(S,3))
  for (i in 1:S) {
    x <- rnorm(N, mean=mu, sd=sigma)
    xbar <- mean(x)
    upper <- xbar + 1.96 * sd(x) /sqrt(N)
    lower <- xbar - 1.96 * sd(x) /sqrt(N)
    cis[i,] <- c(lower, xbar, upper)
  }
  cis <- data.frame(lower=cis[,1],
                    xbar=cis[,2],
                    upper=cis[,3])
  coverage = mean( (cis$upper > mu) & (cis$lower < mu))
  print(paste0(coverage*100,"% of CIs covered the true mean"))
  ciplot <- ggplot(cis[1:plotS,], aes(x=1:plotS, y=xbar, ymax=upper, ymin=lower)) +
    geom_point() +
    geom_errorbar() +
    xlab("Simulation") +
    ylab("Estimated Mean") +
    geom_abline(aes(intercept=mu, slope=0), color="red")
  return(ciplot)
}

cisim(10000, 100, 10, 4, 50)
