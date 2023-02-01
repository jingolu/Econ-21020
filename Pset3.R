# install.packages("readxl")
library("readxl")
library(AER)
library(dplyr)
library(tidyr)
library(ggplot2)

df <- read_xlsx("GitHub/Econ-21020/caschool.xlsx")
income <- df$avginc * 1000
mean(df$avginc)
sd(df$avginc)
mean(income)
sd(income)
mean(df$math_scr)

smallclass <- df %>% filter(str<=20)
nrow(smallclass) / nrow(df)
avgx <- mean(smallclass$math_scr)
n <- nrow(smallclass)
sdx <- sd(smallclass$math_scr)

largeclass <- df %>% filter(str>20)
nrow(largeclass) / nrow(df)
avgy <- mean(largeclass$math_scr)
m <- nrow(largeclass)
sdy <- sd(largeclass$math_scr)

teststatistic <- abs((avgx-avgy)/(sqrt((sdx^2/n)+(sdy^2/m))))

cov(df$avginc, df$math_scr)
cov(income, df$math_scr)

cor(df$avginc, df$math_scr)
cor(income, df$math_scr)