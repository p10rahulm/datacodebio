# General principles
# The aims of good data graphics is to display data accurately and clearly. According to Karl, some
# rules for displaying data badly are:
#         • Display as little information as possible.
# • Obscure what you do show (with chart junk).
# • Use pseudo-3D and color gratuitously.
# • Make a pie chart (preferably in color and 3D).
# • Use a poorly chosen scale.
# Ignore significant figures

#good plots
library("downloader")
filename <- "data/fig1.RData"
url <- "https://github.com/kbroman/Talk_Graphs/raw/master/R/fig1.RData"
if (!file.exists(filename)) download(url,filename)
load(filename)
library(rafalib)

mypar()
dat <- list(Treatment=x,Control=y)
boxplot(dat,xlab="Group",ylab="Response",cex=0)
stripchart(dat,vertical=TRUE,method="jitter",pch=16,add=TRUE,col=1)

#----------------------------------------------------------------------------

rm(list=ls())
#correlation
library(UsingR)
data("father.son")
x=father.son$fheight
y=father.son$sheight
plot(x,y,xlab="Father's height in inches",
     ylab="Son's height in inches",main=paste("correlation =",signif(cor(x,y),2)))

#stratified box plots
groups <- split(y,round(x))
boxplot(groups)
#are each of the stratas normally distributed?
groups <- split(y,round(x))
mypar(2,2)
for(i in c(5,8,11,14)){
        qqnorm(groups[[i]],main=paste0("X=",names(groups)[i]," strata"),
               ylim=range(y),xlim=c(-2.5,2.5))
        qqline(groups[[i]])
}

#standardize data for correlation/covariance
x <- (x-mean(x))/sd(x)
y <- (y-mean(y))/sd(y)
means=tapply(y, round(x*4)/4, mean)
fatherheights=as.numeric(names(means))
mypar(1,1)
plot(fatherheights, means, ylab="average of strata of 
     son heights", ylim=range(fatherheights))
abline(0, cor(x,y))


#correlation can increase due to a single point
set.seed(1)
a=rnorm(100);a[1]=25
b=rnorm(100);b[1]=26
plot(a,b,main=paste("correlation = ",signif(cor(a,b),2)))



#exercises in correlation
data(nym.2002, package="UsingR")
males = nym.2002[nym.2002$gender=="Male",c("age","time")]
females = nym.2002[nym.2002$gender=="Female",c("age","time")]
cor(males$age,males$time,method = "pearson")
cor(females$age,females$time,method = "pearson")
plot(males$age,males$time)
plot(females$age,females$time)
boxplot(females$time ~ females$age)
boxplot(males$time ~ males$age)
time = sort(nym.2002$time)

#should we use logs or normal time
min(time)/median(time)
max(time)/median(time)
mypar(1,2)
plot(time/median(time), ylim=c(1/4,4))
abline(h=c(1/2,1,2))
plot(log2(time/median(time)),ylim=c(-2,2))
abline(h=-1:1)
