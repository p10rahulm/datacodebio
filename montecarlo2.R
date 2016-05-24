set.seed(1)
ps=rnorm(5)
tstat=sqrt(5)*mean(ps)/sd(ps)
tstat
B=1000
set.seed(1)
givetstat <- function(N) {
        ps=rnorm(N)
        tstat=sqrt(N)*mean(ps)/sd(ps)
        tstat   
}
brad <- replicate(B,givetstat(5))
mean(brad>2)
1-pt(2,df=4)

#quantile quantile plots to compare
B=100; 
ps = seq(1/(B+1), 1-1/(B+1),len=B)

qt = qt(ps,df=20) %>% unlist
qqnorm(qt)

qt = qt(ps,df=4) %>% unlist
qqnorm(qt)

library(rafalib)
mypar(3,2)

Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
        ts <- replicate(B, {
                X <- rnorm(N)
                sqrt(N)*mean(X)/sd(X)
        })
        ps <- seq(1/(B+1),1-1/(B+1),len=B)
        qqplot(qt(ps,df=N-1),ts,main=N,
               xlab="Theoretical",ylab="Observed",
               xlim=LIM, ylim=LIM)
        abline(0,1)
} 

library(rafalib)
mypar(3,2)

Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
        ts <- replicate(B, {
                X <- rnorm(N)
                sqrt(N)*mean(X)/sd(X)
        })
        ps <- seq(1/(B+1),1-1/(B+1),len=B)
        qqplot(qt(ps,df=N-1),ts,main=N,
               xlab="Theoretical",ylab="Observed",
               xlim=LIM, ylim=LIM)
        abline(0,1)
} 


Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
        ts <- replicate(B,{
                x <- rnorm(N)
                y <- rnorm(N)
                t.test(x,y, var.equal = TRUE)$stat
        })
        ps <- seq(1/(B+1),1-1/(B+1),len=B)
        qqplot(qt(ps,df=2*N-2),ts,main=N,
               xlab="Theoretical",ylab="Observed",
               xlim=LIM, ylim=LIM)
        abline(0,1)
} 


set.seed(1)
N <- 1000
B <- 10000
tstats <- replicate(B,{
        X <-  sample(c(-1,1), N, replace=TRUE)
        sqrt(N)*mean(X)/sd(X)
})
qqnorm(tstats)
abline(0,1)
#With N=1000, CLT kicks in and the t-statistic is approximated with normal 0,1
##Furthermore, t-distribution with df=999 and normal are practically the same
# Question:Is the following statement true or false ? If instead of generating 
# the sample with X=rnorm(N) with N=1000, we generate the data with binary data 
# X= sample(c(-1,1), N, replace=TRUE), then the t-statistic sqrt(N)*mean(X)/sd(X) 
# is approximated by a t-distribution with 999 degrees of freedom.


set.seed(1)
N <- 15
B <- 10000
tstats <- replicate(B,{
        X <- sample(c(-1,1), N, replace=TRUE)
        sqrt(N)*mean(X)/sd(X)
})
ps=seq(1/(B+1), 1-1/(B+1), len=B) 
qqplot(qt(ps,N-1), tstats, xlim=range(tstats))
abline(0,1)
#The population data is not normal thus the theory does not apply.
#We check with a Monte Carlo simulation. The qqplot shows a large tail. 
#Note that there is a small but positive chance that all the X are the same.
##In this case the denominator is 0 and the t-statistics is not defined



# We can derive approximation of the distribution of the sample average or 
# the t-statistic theoretically. However, suppose we are interested in the 
# distribution of a statistic for which a theoretical approximation is not 
# immediately obvious.
# 
# Consider the sample median as an example. Use a Monte Carlo to determine 
# which of the following best approximates the median of a sample taken from 
# normally distributed population with mean 0 and standard deviation 1.
# 
# Options:
#         median is approximately normal with mean 0 and SD 1/sqrt(N)
#         median is not approximately normal
#         is t distributed for small sample size, and normal for large sample
#         approx normal with mean 0 and SD ? 1/sqrt(N)
#         



set.seed(1)
N <- 15
B <- 100
Pop <- rnorm(5000, mean=0, sd=1)
tstats <- replicate(B,{
        X <- sample(Pop, N, replace=TRUE)
        median(X)
})
ps=seq(1/(B+1), 1-1/(B+1), len=B) 
qqplot(qt(ps,N-1), tstats, xlim=range(tstats))
qqnorm(tstats)
abline(0,1)
sd(tstats)
mean(tstats)
1/sqrt(25)


set.seed(1)
Ns <- seq(5,45,5)
library(rafalib)
mypar(3,3)
for(N in Ns){
        medians <- replicate(10000, median ( rnorm(N) ) )
        title <- paste("N=",N,", avg=",round( mean(medians), 2) , ", sd*sqrt(N)=", round( sd(medians)*sqrt(N),2) )
        qqnorm(medians, main = title )
        qqline(medians)
}
##there is an asymptotic result that says SD is sqrt(N*4*dnorm(0)^2)