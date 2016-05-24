set.seed(1)
x=c(rnorm(100,0,1)) ##real distribution
x[23] <- 100 ##mistake made in 23th measurement
boxplot(x)

#instead use median
median(x)
#and instead of standard dev use median absolute dev
MAD <- 1.4826 * median(abs(x - median(x)))
# The number 1.4826 is a scaling factor such that the MAD is an 
# unbiased estimate of the standard deviation. Notice how much 
# closer we are to 1 with the MAD:
mad(x)

mypar(1,2)
set.seed(1)
x=rnorm(100,0,1)
y=rnorm(100,0,1)
x[23]=100
y[23]=88
library(rafalib)
plot(x,y,main=paste("cor=",signif(cor(x,y),3)))
abline(0,1)
plot(rank(x),rank(y),main=paste("spearman cor=",signif(cor(x,y,method="spearman"),3)))
abline(median(rank(y)),cor(x,y,method="spearman"))

#exercises on robust stats
data(ChickWeight)
plot( ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)
chick = reshape(ChickWeight, idvar=c("Chick","Diet"), timevar="Time",
                direction="wide")
head(chick)
View(chick)
chick = na.omit(chick)
mean(c(chick$weight.4,3000))/mean(chick$weight.4)
median(c(chick$weight.4,3000))/median(chick$weight.4)
sd(c(chick$weight.4,3000))/sd(chick$weight.4)
mad(c(chick$weight.4,3000))/mad(chick$weight.4)
plot(chick$weight.21,chick$weight.4,col=chick$Diet)
a <- cor(chick$weight.21,chick$weight.4,method="pearson")
b <- cor(c(3000,chick$weight.21),c(3000,chick$weight.4),method="pearson")
b/a
a1 <- cor(chick$weight.21,chick$weight.4,method="spearman")
b1 <- cor(c(3000,chick$weight.21),c(3000,chick$weight.4),method="spearman")
b1/a1

#logratios
x <- 2^(rnorm(100))
y <- 2^(rnorm(100))
ratios <- x / y
mypar(1,2)
hist(ratios)
logratios <- log2(ratios)
hist(logratios)
plot(x,y)
plot(log(x),log(y))




#wilcoxon Rank sum test
set.seed(779) ##779 picked for illustration purposes
N=25
x<- rnorm(N,0,1)
y<- rnorm(N,0,1)
x[1] <- 5
x[2] <- 7
cat("t-test pval:",t.test(x,y)$p.value)
cat("Wilcox test pval:",wilcox.test(x,y)$p.value)


library(rafalib)
mypar(1,2)
stripchart(list(x,y),vertical=TRUE,ylim=c(-7,7),ylab="Observations",pch=21,bg=1)
abline(h=0)
xrank<-rank(c(x,y))[seq(along=x)]
yrank<-rank(c(x,y))[-seq(along=y)]
stripchart(list(xrank,yrank),vertical=TRUE,ylab="Ranks",pch=21,bg=1,cex=1.25)
ws <- sapply(x,function(z) rank(c(z,y))[1]-1)
text( rep(1.05,length(ws)), xrank, ws, cex=0.8)


W <-sum(ws)


n1<-length(x);n2<-length(y)
Z <- (mean(ws)-n2/2)/ sqrt(n2*(n1+n2+1)/12/n1)
print(Z)


# In other words:
# Z-score = ((U/n1)-(n2/2))/sqrt(n2*(n1+n2+1)/(12*n1))
