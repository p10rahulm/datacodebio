rm(list=ls())
install.packages("UsingR")
library(UsingR)
library(rafalib)
x <- father.son$fheight

#-----------------------------------histograms---------------------
hist(x,breaks =seq( floor(min(x)),ceiling(max(x))),main="mytitle",
     xlab="height in inches",
     ylab="frequency")


#------------------emperical cumulative distribution percent-------

xs = seq(floor(min(x)),ceiling(max(x)),0.1)
plot(xs,ecdf(x = x)(xs),type = "l",main = "my ecdf",xlab = "height in inches")

#------------------now check if distribution may be normal---------
#-------------by comparing ecdf plot with normal for some height---
#abline(70,5)
mean(x>70)
1-pnorm(70,mean(x),sd(x))

#--plot now the normal distribution with our obtained distribution--
xs = seq(0.5,99.5,1)
givenpl <- quantile(x,xs/100)
normalpl <- qnorm(xs/100,mean(x),sd(x))
qqplot(x = givenpl,y = normalpl,
       main="QQ plot of data with normal",ylab="normal",xlab="given")
abline(0,1)

#-----same thing with automated function qqnorm---------------------

qqnorm(x)
qqline(x)

#-------------------------------------------------------------------

#t distributions
dfs <- c(3,6,12,30)
mypar(2,2)
for(df in dfs){
        x <- rt(1000,df)
        qqnorm(x,xlab="t quantiles",main=paste0("d.f=",df),ylim=c(-6,6))
        qqline(x)
}


#using box plots
mypar(1,2)
hist(exec.pay) ##in UsingR package
qqnorm(exec.pay)
qqline(exec.pay)
mypar()
boxplot(exec.pay, ylab="10,000s of dollars", ylim=c(0,400),breaks=100)

#practice qqnorm
load("data/skew.RData")
dim(dat)
mypar(mfrow=c(3,3)) # mfrow means filled row by row
for (i in 1:9) {
        qqnorm(dat[,i])
}
# 4,9 are skewed
mypar(1,2)
hist(dat[,4])
hist(dat[,9])

#practice boxplots
head(InsectSprays)
mypar(1,2)
boxplot(split(InsectSprays$count, InsectSprays$spray))
boxplot(InsectSprays$count ~ InsectSprays$spray)

library(dplyr)
data(nym.2002, package="UsingR")
head(nym.2002)
boxplot(nym.2002$time~nym.2002$gender)
hist(nym.2002[nym.2002$gender=="Male","time"])
hist(nym.2002[nym.2002$gender=="Female","time"])
