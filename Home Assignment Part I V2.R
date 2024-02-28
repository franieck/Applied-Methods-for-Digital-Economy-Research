#PART I
#a. Do the simulations that are shown from page 20 onwards to illustrate OVB.Can you replicate the bias ?
#Without endogeneity problem 
ssize <- 1000
x1 <- rnorm(n=ssize,sd=3)
x2 <- rnorm(n=ssize,sd=5)
y <- 2+3*x1+5*x2+rnorm(n=ssize, sd=5)

out.y.full <- lm(y ~x1 + x2)
out.y.x1.om <- lm(y ~x1)
out.y.x2.om <- lm(y ~x2)

cor.test(x =x1,y=x2) #results show no significant correlation between x1 and x2, so the error term converges to 0, meaning that ^b1  should = ~b1

stargazer(out.y.full, out.y.x1.om, out.y.x2.om,
          type='text',omit.stat= c('f', 'ser'), no.space=T)

#with endogeneity problem 
ssize <- 1000
x1 <- rnorm(n=ssize,sd=3)
x2 <- rnorm(n=ssize,mean=x1,sd=5)
y <- 2+3*x1+5*x2+rnorm(n=ssize, sd=5)

out.y.full <- lm(y ~x1 + x2)
out.y.x1.om <- lm(y ~x1)
out.y.x2.om <- lm(y ~x2)

cor.test(x =x1,y=x2) #results show a positive and significant correlation between x1 and x2, so our error term is different from 0, meaning that ^b1 will differ from ~b1, leading to a biased estimate.

stargazer(out.y.full, out.y.x1.om, out.y.x2.om,
          type='text',omit.stat= c('f', 'ser'), no.space=T)

# checking for whether ~b1 is equal to ^b1 + ^b2̃δ
install.packages("stargazer")
y <- 2+3*x1+5*x2+rnorm(n=1000, sd=5)
out.y.full <- lm(y ~x1 + x2)
out.y.incomp.x1 <- lm(y ~x1)
out.y.incomp.x2 <- lm(y ~x2)
out.x1.parti <- lm(x1~x2)
out.x2.parti <- lm(x2~x1)
stargazer(
  out.x1.parti,
  out.x2.parti,
  out.y.full,
  out.y.incomp.x1,
  out.y.incomp.x2,
  type='text',omit.stat=c('f','ser')) # I get the following error with this syntax: "Error in if (is.na(s)) { : the condition has length > 1"

#Therefore, I imported the data in .txt to see if this time around it worked.
setwd("C:\\Users\\lucas\\Documents\\NOVA SBE\\PhD\\Methodology\\Applied Methods for Digital Economy Research\\Week 2\\Lab and Practice\\Home Assignment")
data <- read.csv("OVBData.txt")



out.y.full <- lm(y ~x1 + x2,data=data)
out.y.incomp.x1 <- lm(y ~x1,data=data)
out.y.incomp.x2 <- lm(y ~x2,data=data)
out.x1.parti <- lm(x1~x2,data=data)
out.x2.parti <- lm(x2~x1,data=data)
stargazer(
  out.x1.parti,
  out.x2.parti,
  out.y.full,
  out.y.incomp.x1,
  out.y.incomp.x2,
  type='text',omit.stat=c('f','ser')) # Same error, so I tried a different approach, shown below

data <- data.frame(
  y = 2 + 3 * x1 + 5 * x2 + rnorm(n = ssize, sd = 5),
  x1 = x1,
  x2 = x2
)
str(data)
summary(data)

out.y.full <- lm(y ~ x1 + x2, data = data)
out.y.incomp.x1 <- lm(y ~ x1, data = data)
out.y.incomp.x2 <- lm(y ~ x2, data = data)
out.x1.parti <- lm(x1 ~ x2, data = data)
out.x2.parti <- lm(x2 ~ x1, data = data)

stargazer(
  out.x1.parti,
  out.x2.parti,
  out.y.full,
  out.y.incomp.x1,
  out.y.incomp.x2,
  type = 'text', omit.stat = c('f', 'ser'), header = TRUE, title = "Regression Results" # Same error. No idea what's going on.
)

coef(out.y.full)['x1']+coef(out.y.full)['x2']*coef(out.x2.parti)['x1']
coef(out.y.full)['x2']+coef(out.y.full)['x1']*coef(out.x1.parti)['x2']

ssize <- 1000
x1 <- rnorm(n=ssize, sd=3)
x2 <- rnorm(n=ssize,mean=x1, sd=5)
y <- 2+3*x1+5*x2+rnorm(n=ssize,sd=5)
out.y.full <- lm(y ~x1 + x2)
out.parti.x2 <- lm(x1 ~x2)
out.y.x1 <- lm(y ~residuals(out.parti.x2))
out.y.x1.om <- lm(y ~ x1)
stargazer(
  out.y.full,
  out.x2.parti,
  out.parti.x2,
  out.y.x1.om,
  out.y.x1,
  type='text',
  omit.stat=c('f','ser'), no.space=T)

# Instead of simply copying everything, try to center x2 around 1.33*x1. Can you predict/compute the bias you will get now? What bias do you get?



ssize <- 1000
x1 <- rnorm(n=ssize, sd=3)
x2 <- rnorm(n=ssize,mean=x1*1.33, sd=5)
y <- 2+3*x1+5*x2+rnorm(n=ssize,sd=5)
out.y.full <- lm(y ~x1 + x2)
out.parti.x2 <- lm(x1 ~x2)
out.y.x1 <- lm(y ~residuals(out.parti.x2))
out.y.x1.om <- lm(y ~ x1)
stargazer(
  out.y.full,
  out.x2.parti,
  out.parti.x2,
  out.y.x1.om,
  out.y.x1,
  type='text',
  omit.stat=c('f','ser'), no.space=T)

out.y.full
predicted_y_full <- predict(out.y.full) #Not sure if this is the correct syntax to calculate the bias.
bias_full <- mean(y - predicted_y_full)
bias_full

#c. Instead of omitting x2, try omitting x1. Can you estimate x2 without bias?

ssize <- 1000
x1 <- rnorm(n = ssize, sd = 3)
x2 <- rnorm(n = ssize, mean = x1 * 1.33, sd = 5)
y <- 2 + 3 * x1 + 5 * x2 + rnorm(n = ssize, sd = 5)
out.y.full <- lm(y ~ x2)
out.y.full
summary(out.y.full) # X2 = 5.87
conf_intervals <- confint(out.y.full)
conf_intervals # CI [5.79, 5.95]
#Ommitted variables in multivariate models (not part of the home assingment, but I figured out too late so decided to keep it here, it was a good practice.)

setwd("C:\\Users\\lucas\\Documents\\NOVA SBE\\PhD\\Methodology\\Applied Methods for Digital Economy Research\\Week 2\\Lab and Practice\\Home Assignment")
data <- read.csv("OVBData.txt")
df.OVBdat <- read.table("OVBData.RData")
dt.OVBdat <- data.table(df.OVBdat)
summary(dt.OVBdat)
cor(dt.OVBdat$y2, dt.OVBdat$x1)
DT <- dt.OVBdat
cor_matrix <- cor(DT) #ok, this is visually abhorrent.
print(cor_matrix)
install.packages("corrplot") #googled and found this, much easier on the eyes.
library(corrplot)
corrplot(cor_matrix, method = "color") # stong correlations: y2 and y3; x1 and x3; x3 and x5; weaker correlations: x2 and x4. Damn, there are many correlations.
# 1.) try to predict/estimate y2, omitting 
## first x2 
## then  x4
out.y2.x2ommitted <- lm(y2 ~ x1+x3+x4+x5,data.table(DT)) #estimation of y2 whilst omitting x2.
out.y2.x2ommitted

out.y2.x4ommitted <- lm(y2 ~ x1+x2+x3+x5,data.table(DT)) #estimation of y2 whilst omitting x4.
out.y2.x4ommitted
# 2.)  now try predicting/estimating y3, omitting
## first x2 and 
## then omitting x5
out.y3.x2ommitted <- lm(y2 ~ x1+x3+x4+x5,data.table(DT)) #estimation of y3 whilst omitting x2.
out.y3.x2ommitted

out.y3.x5ommitted <- lm(y2 ~ x1+x2+x3+x4,data.table(DT)) #estimation of y3 whilst omitting x5.
out.y3.x5ommitted

#PART II in sepparate Syntax file

