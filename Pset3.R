# install.packages("readxl")
library("readxl")
library(AER)
library(dplyr)
library(tidyr)
library(ggplot2)

df <- read_xlsx("GitHub/Econ-21020/caschool.xlsx")

# part a
nrow(df)

# defining new variable income and comparing avginc with income (part b)
income <- df$avginc * 1000
mean(df$avginc)
sd(df$avginc)
mean(income)
sd(income)

# below is part c
# calculating mean math score
mean(df$math_scr)

# calculating statistics for class size <= 20
smallclass <- df %>% filter(str<=20)
nrow(smallclass) / nrow(df)
avgx <- mean(smallclass$math_scr)
n <- nrow(smallclass)
sdx <- sd(smallclass$math_scr)

# calculating statistics for class size > 20
largeclass <- df %>% filter(str>20)
nrow(largeclass) / nrow(df)
avgy <- mean(largeclass$math_scr)
m <- nrow(largeclass)
sdy <- sd(largeclass$math_scr)

# test statistic
teststatistic <- abs((avgx-avgy)/(sqrt((sdx^2/n)+(sdy^2/m))))

# comparing covariance and correlation of avginc and income with mean math score
cov(df$avginc, df$math_scr)
cov(income, df$math_scr)

cor(df$avginc, df$math_scr)
cor(income, df$math_scr)