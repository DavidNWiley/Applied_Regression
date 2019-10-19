
# Lab 6

#Reading in the information for Weight and Symbolic BP
dat=read.csv("C:\\Users\\Nick\\Documents\\0_Spring 2019\\Applied Regression\\Labs_HW\\Data_Sets\\Chapter 2\\Problems\\data-prob-2-10.csv", header = T)
y = dat[,2]
x = dat[,1]
n = length(x)

#plot(x, y, xlab = "Symbolic BP", ylab = "Weight")


Sxx = sum(x^2) - sum(x)^2 / n
Sxy = sum(x*y) - sum(x)*sum(y) / n

B1H = Sxy / Sxx
B0H = mean(y) - mean(x)*B1H

yH = B0H + sort(x) * B1H
#lines(sort(x), sort(yH))

SSr = B1H * Sxy
SSres = sum((y - mean(y))^2) - SSr
  
SSt = SSr + SSres
MSres = SSres / (n-2)

seB1H = sqrt(MSres/Sxx)
seB0H = sqrt(MSres * (1 / n + mean(x)^2 / Sxx))

MSr = SSr/1

F0 = MSr / MSres


# Estimating the correlation coefficient
r = Sxy / sqrt(Sxx * SSt)
r
R2 = SSr / SSt
R2

# Testing Hypothesis p = 0
t0 = r*sqrt(n-2) / sqrt(1-r^2)
t0


# Testing Hypothesis p = .6
z = atanh(r)
z

p = .6
z0 = (atanh(r) - atanh(p)) * sqrt(n-3)
z0


#z0 !> z_a2 so we fail to reject the H0

# Setting alpha
alp = .05

# Setting up the %95 CI

# Lower bound
LB = tanh(atanh(r) - qnorm(1-alp/2) / sqrt(n-3))
LB 

# Upper bound
UB = tanh(atanh(r) + qnorm(1-alp/2) / sqrt(n-3))
UB 


summary(lm(y~x-1))
12.6^2


