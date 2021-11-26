# Financial analysis for project proposal
# Date: Oct 26, 2021
# Filename: project_proposal_analysis.R
# Author: Pravesh Agarwal

## Description
# Portfolio includes Salesforce and Home Depot

rm(list = ls())

# importing library
library(dplyr)
library(quantmod)
library(ggplot2)

# importing data
symbols <- c("MSFT","NVDA")
getSymbols(symbols, from = '2019-10-01',
           to = "2021-11-18",  periodicity = "monthly",src="yahoo",
           na.omit=TRUE)

rawdata <- cbind(MSFT$MSFT.Close,NVDA$NVDA.Close)

head(rawdata)
tail(rawdata)
colnames(rawdata) <- c("hd", "crm")


# Plotting closing prices
# plot(HD$HD.Close,
#      main = "Closing Price of Home Depot (HD)")
# plot(CRM$CRM.Close,
#      main = "Closing Price of Salesforce (CRM)")

# lagging data and to calculate return between each month using HPR formula
returns <- (rawdata-stats::lag(rawdata))/stats::lag(rawdata)[-1,]
rp_returns <- returns
head(returns)
# the mean of every column
colMeans(rp_returns)

# Testing -----------------------
# # , na.rm = TRUE)
# ## to make sure the data we use is matrix data type
# # r <- as.matrix(colMeans(rp_returns, na.rm = TRUE))*100
# r <- as.matrix(colMeans(rp_returns))
# r
# # use var() function
# # why are we taking percent of returns?
# # returns_percent <- returns*100
# # vmatrix<- var(returns_percent)
# # vmatrix<- var(returns_percent, na.rm = TRUE)
# # ?var
# vmatrix <- var(returns)
# # vmatrix <- var(returns, na.rm=TRUE)
# vmatrix
# --------------------------------

## to make sure the data we use is matrix data type
r <- as.matrix(colMeans(rp_returns))*100

# use var() function
returns_percent <- returns*100
vmatrix<- var(returns_percent)


# Getting optimal weights of CRM and HD in portfolio
# Asset 1 is HD, Asset 2 is CRM

# expected return asset 1
r1 <- r[1]
# standard deviation asset 1
s1 <-  vmatrix[1,1]

r2 <-  r[2]
s2 <-  vmatrix[2,2]

# rho is the corelation
rho <-  cor(returns)[2,1]

# weight on asset 1
w1 <-  seq(0, 1, 0.001)
# weight on asset 2
w2 <-  1 - w1
rp <- w1 * r1 + w2 * r2
vp <- w1 ^ 2 * s1 ^ 2 +
  w2 ^ 2 * s2 ^ 2 +
  2 * w1 * w2 * rho * s1 * s2
sp <- sqrt(vp)

# define a matrix
? matrix

# length(sp): number of rows
# 2: number of columns
mvf <- matrix(c(sp, rp), length(sp), 2)

head(mvf)
max(mvf[,2])
min(mvf)
# ylim and xlim specify the range of y axis. # mvf starts from 0 so we specify x
# and y axis to start from 0
# pch is the type of the points on the figure
?plot
plot(
  mvf,
  # ylim = c(0, 3),
  # xlim = c(0, 115),
  pch = ".",
  main = "Optimal Portfolio",
  xlab = "Risk",
  ylab = "E(r)"
)

# sharpe ratio = risk premium/risk
# risk free rate is 0.04
# risk premium/sd = (expected return-risk free rate)/ standard deviation
# - (second list in the mvf-0.04)/first list in  the mvf)
sharpe <- (mvf[, 2] - 0.04) / mvf[, 1]

# maximized sharpe ratio is th slope of the CAL
# risk free rate is the intercept
# y = a+bx
# CAL: y = 0.04+sharpe_max*x
abline(0.04, max(sharpe))

# find the x and y that will give us the maximized sharpe
sharpe == max(sharpe)
x <- sp[sharpe==max(sharpe)]
y <- rp[sharpe==max(sharpe)]
points(x,y,col="red", pch=16)

# Finding the optimal weight of asset 1 HD
x
y
rp[which(rp == y)]
rp
length(rp)
length(w1)
optimal_w1 <- w1[which(rp == y)]
optimal_w2 <- 1-optimal_w1
# checking if our weights are correct
optimal_w1*r1+optimal_w2*r2 == y
