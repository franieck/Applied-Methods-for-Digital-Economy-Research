

#Question 1

set.seed(420711)
Y <- rnorm(n=5347)
X1 <- rnorm(n=5347)
X2 <- rnorm(n=5347)
Z1 <- rnorm(n=5347)
Z2 <- rnorm(n=5347)
print(Y)
print(X1)
print(X2)
print(Z1)
print(Z2)

#covariance of y and y
covariance_yy <- cov(Y, Y)
covariance_yy
#covariance of y and x1
covariance_yx1 <- cov(Y, X1)
covariance_yx1
#covariance of y and x2
covariance_yx2 <- cov(Y, X2)
covariance_yx2
#covariance of y and z1
covariance_yz1 <- cov(Y, Z1)
covariance_yz1
#covariance of y and z2
covariance_yz2 <- cov(Y, Z2)
covariance_yz2
#covariance of x1 and x1
covariance_x1x1 <- cov(X1, X1)
covariance_x1x1
#covariance of x1 and y
covariance_x1y <- cov(X1, Y)
covariance_x1y
#covariance of x1 and x2
covariance_x1x2 <- cov(X1, X2)
covariance_x1x2
#covariance of x1 and z1
covariance_x1z1 <- cov(X1, Z1)
covariance_x1z1
#covariance of x1 and z2
covariance_x1z2 <- cov(X1, Z2)
covariance_x1z2

#covariance of x2 and y
covariance_x2y <- cov(X2, Y)
covariance_x2y
#covariance of x2 and x1
covariance_x2x1 <- cov(X2, X1)
covariance_x2x1
#covariance of x2 and x2
covariance_x2x2 <- cov(X2, X2)
covariance_x2x2
#covariance of x2 and z1
covariance_x2z1 <- cov(X2, Z1)
covariance_x2z1
#covariance of x2 and z2
covariance_x2z2 <- cov(X2, Z2)
covariance_x2z2
#covariance of z1 and z1
covariance_z1z1 <- cov(Z1, Z1)
covariance_z1z1
#covariance of z1 and z2
covariance_z1z2 <- cov(Z1, Z2)
covariance_z1z2
#covariance of z2 and z2
covariance_z2z2 <- cov(Z2, Z2)
covariance_z2z2

VarX1 <- var(X1)
VarX1 #0.9843107
VarX2 <- var(X2)
VarX2 #1.005453
VarY <- var(Y)
VarY #1.038601
VarZ1 <- var(Z1)
VarZ1 #1.013636
VarZ2 <- var(Z2)
VarZ2 #1.016727


#Question 2
#a)
RegCoeffYonX1 <- covariance_yx1/VarX1
RegCoeffYonX1 #-0.01741639
#b)
modelYonX1only <- lm(Y ~ X1)

summary(modelYonX1only)

#Question 3 (a+b)
set.seed(420711)
Y <- rnorm(n=5347)
X1 <- rnorm(n=5347)
X2 <- rnorm(n=5347)
dataMultiQ3 <- data.frame(X1, X2, Y)
cov_matrix <- cov(dataMultiQ3)
cov_matrix
RegCoeffYonX1 <- covariance_yx1/VarX1 #beta 1
RegCoeffYonX1 #-0.01741639
RegCoeffYonX2 <- covariance_yx2/VarX2 #beta 2
RegCoeffYonX2 #-0.01020655

#Question 3 Creating the linear regression model
set.seed(420711)
X1 <- rnorm(n = 5347)
X2 <- rnorm(n = 5347)

Y <- RegCoeffYonX1 * X1 + RegCoeffYonX2 * X2 + rnorm(n = 5347)
dataMultiQ3c <- data.frame(X1, X2, Y)
model <- lm(Y ~ X1 + X2, data = dataMultiQ3c)
summary(model)

#Question 5 - Since it wasn't specified, I'm assuming the OLS model we're talking about here is the same as in the previous exercise: Y = constant + b1*X1 + b2*X2 + error
set.seed(420711)
X1 <- rnorm(n = 5347)
X2 <- rnorm(n = 5347)

Y <- 1+ RegCoeffYonX1 * X1 + RegCoeffYonX2 * X2 + rnorm(n = 5347) #forced constant = 1 here.
dataMultiQ5 <- data.frame(X1, X2, Y)
dataMultiQ5 #finished generating the model here.

hist(X1, main = "Histogram of X", xlab = "X-axis label", col = "lightblue", border = "black") #the distribution of X1 follows quite a normal distribution. Therefore, I will discretize them into 7 categories (how many standard deviations the datapoints are from the mean)
mean_X1 <- mean(X1)
sd_X1 <- sd(X1)

breaks <- c(-Inf, mean_X1 - 2 * sd_X1, mean_X1 - sd_X1, mean_X1 + sd_X1, mean_X1 + 2 * sd_X1, Inf)
labels <- c(1, 2, 3)
X1_category <- cut(X1, breaks = breaks, labels = labels, include.lowest = TRUE) # This didn't work, I consistently get the error "Error in cut.default(X1, breaks = breaks, labels = labels[-length(breaks)],  : number of intervals and length of 'labels' differ", tried for 1h30 but couldn't find a solution.

#Therefore, I just attempted to discretize X1 across 3 categories (didn't work  either, so I created the equation for Y with the regular random generated X1):
set.seed(420711)
X1 <- rnorm(n = 5347)
X2 <- rnorm(n = 5347)
X1_discrete <- cut(dataMultiQ5$X1, breaks = 3)

Y <- 1+ RegCoeffYonX1 * X1 + RegCoeffYonX2 * X2 + rnorm(n = 5347) #forced constant = 1 here. I tried using the X1_discrete here but I can't perform any analysis when I do that because the variable becomes a factor, not allowing me to calculate the variance and subsequent statistics. 
dataMultiQ5 <- data.frame(X1, X2, Y)
dataMultiQ5 #finished generating the model here.


# Now we can compute the OLS model
dataMultiQ5 <- data.frame(X1, X2, Y)

modelQ5 <- lm(Y ~ X1 + X2, data = dataMultiQ5) #to get the coefficients for b1 and b2 (b0 should be 1). Note that in this model X1 wasn't discretized.
modelQ5 #  b1 (estimated coefficient of X1) = -0.007469; b2 (estimated coefficient of X2) = 0.014249; b0 is close to 1 (since I forced it)

#Now that we have the estimated coefficients for b0,b1, and b2, we can compute the CEF for Y|X1:
CEF_Y_on_X1 <- 1 + RegCoeffYonX1 * dataMultiQ5$X1
CEF_Y_on_X1

plot(dataMultiQ5$X1, CEF_Y_on_X1, col = "blue", main = "CEF of Y on X1", xlab = "X1", ylab = "Y")
points(dataMultiQ5$X1, dataMultiQ5$Y, col = "red", pch = 20)
