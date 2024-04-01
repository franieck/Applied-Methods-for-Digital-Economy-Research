alpha = 0.05
z.half.alpha = qnorm(1-alpha/2)
c(-z.half.alpha, z.half.alpha)
xbar=1000/25
mu0=45
sigma= sqrt(500)
n= 25
z= (xbar-mu0)/(sigma/sqrt(n))
z

pnorm(z,lower.tail = FALSE)
pnorm(z,lower.tail = TRUE)

pval= 2*pnorm(z)
pval

t.alpha=0.05
t.half.alpha= qt(1-alpha/2,25-1)
c(-t.half.alpha, t.half.alpha)
xbar=1000/25
mu0=45
s=sqrt(400)
n=25
t= (xbar-mu0)/(s/sqrt(n))
t

pt(t,df=25-1,lower.tail = FALSE)
pt(t,df=25-1,lower.tail = TRUE)
pval=2*pt(t,df=25-1)
pval

setwd("C:\\Users\\lucas\\Documents\\NOVA SBE\\PhD\\Methodology\\Applied Methods for Digital Economy Research\\Week 2\\Lab and Practice\\Lab_02a_recap_hypothesis_testing")
dt.stocks <- data.table(read.csv("C:\\Users\\lucas\\Documents\\NOVA SBE\\PhD\\Methodology\\Applied Methods for Digital Economy Research\\Week 2\\Lab and Practice\\Lab_02a_recap_hypothesis_testing\\data_r.csv"))
dt.stocks <-setnames(dt.stocks, tolower(names(dt.stocks)))
head(dt.stocks)

xbar <- dt.stocks[, mean(idjcomp, na.rm=TRUE)]
s <- dt.stocks[, sd(idjcomp, na.rm=TRUE)]
n <- dt.stocks[, length(which(!is.na(idjcomp)))]
error <- qnorm(0.975)*s/sqrt(n)
left <- xbar-error
right <- xbar+error
left
right

dt.stocks[, t.test(isp500)]
dt.stocks[, t.test(isp500, alternative = c("greater"), mu=0.5, conf.level= 0.99)]
dt.stocks[, t.test(idjcomp,inasdaq,paired=TRUE)]

dt.stocks[ ,postcrisis:=ifelse(year>2008,1,0)]
dt.stocks[, t.test(idjcomp ~ postcrisis)]

dt.stocks[,t.test(idjcomp,inasdaq,var.equal=TRUE)]
dt.stocks[,t.test(idjcomp,inasdaq,var.equal=FALSE)]
