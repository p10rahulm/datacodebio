rm(list=ls())
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=paste("data/",filename,sep=""))
babies <- read.table("data/babies.txt", header=TRUE)

bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)
set.seed(1)
dat.ns = sample(bwt.nonsmoke,25)
dat.s = sample(bwt.smoke,25)
g = t.test(dat.s,dat.ns)
g
1-(pnorm(2.1209,0,1)-pnorm(-2.1209,0,1))
2*pnorm(-abs(2.1209))
mean(dat.s) - mean( dat.ns)
#99%confidence interval
a2 = sd(dat.ns)^2/length(dat.ns)
b2 = sd(dat.s)^2/length(dat.s)
c2=sqrt(a2+b2)
2.576*c2

 
 #99% confidence interval with t value
N=25
qt(.01/2,    df=2*N-2)
t.test(dat.s,dat.ns,conf.level=0.99)$conf.int
qnorm(0.01/2)
#12.0486 
# 
# Interval using normal distribution is qnorm(0.01/2)
# for t distribution, the same is qt(.01/2,    df=2*N-2)
# This has to be multiplied by se to get the interval. 
# se = sqrt(sd(a)^2/Na + sd(b)^2/Nb)
# Therefore interval is se *qt in this case
sdt=sqrt(sd(dat.ns)^2/length(dat.ns) + sd(dat.s)^2/length(dat.s))

 
set.seed(1)
dat.ns = sample(bwt.nonsmoke,5)
dat.s = sample(bwt.smoke,5)
t.test(dat.ns,dat.s)

#Power assignment
babies <- read.table("data/babies.txt", header=TRUE)

bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)
set.seed(1)
dat.ns = sample(bwt.nonsmoke,5)
dat.s = sample(bwt.smoke,5)
pval = t.test(dat.ns,dat.s)$p.value




N <- 5
alpha <- 0.05
B <- 10000

set.seed(1)
reject <- function(N,alpha=0.05) {
        hf <- sample(bwt.smoke,N)
        control <- sample(bwt.nonsmoke,N)
        pval <- t.test(hf,control)$p.value
        pval < alpha
}
rejections <- replicate(B,reject(N))
mean(rejections)

Ns <-  c(5,30,60,90,120)
alphap <- 0.01
power <- sapply(Ns,function(N) {
        rejections <- replicate(B,reject(N,alphap))
        mean(rejections)
}       
)

plot(Ns,power,type="b")




set.seed(1)
chowPopulation <- read.csv("data/femaleControlsPopulation.csv")
chowPopulation <- unlist(chowPopulation)
mu_chow <- mean(chowPopulation)
print(mu_chow)
N <- 30
chow <- sample(chowPopulation,N)
print(mean(chow))


se <- sd(chow)/sqrt(N)
mean(chow) - mean(chowPopulation) #has mean 0
sd(chow)/sqrt(12) #and standard deviation sd(chow)/sqrt(12)


Z=(mean(chow) - mean(chowPopulation)) / (sd(chow)/sqrt(12)) # has mean 0 and std dev =1
#is same as
Z=(mean(chow) - mean(chowPopulation)) / se

Q <- qnorm(1-0.05/2)
Q
# With 95%confidence, Z lies between -Q and Q
# With 95%confidence,(mean(chow) - mean(chowPopulation))  lies between -Q*se and Q*se
# With 95%confidence, mean(chowPopulation)  lies between mean(chow) + Q*se and mean(chow) - Q*se
interval <- c(mean(chow)-Q*se,mean(chow)+Q*se)


#Plotting intervals

library(rafalib)
B <- 250
mypar(2,1)
plot(mean(chowPopulation)+c(-7,7),c(1,1),type="n",
     xlab="weight",ylab="interval",ylim=c(1,B))
abline(v=mean(chowPopulation))
for (i in 1:B) {
        chow <- sample(chowPopulation,N)
        se <- sd(chow)/sqrt(N)
        interval <- c(mean(chow)-Q*se, mean(chow)+Q*se)
        covered <- mean(chowPopulation) <= interval[2] & mean(chowPopulation) >= interval[1]
        color <- ifelse(covered,1,2)
        lines(interval, c(i,i),col=color)
}

plot(mean(chowPopulation)+c(-7,7),c(1,1),type="n",
     xlab="weight",ylab="interval",ylim=c(1,B))
abline(v=mean(chowPopulation))
Q <- qnorm(1- 0.05/2)
N <- 5
for (i in 1:B) {
        chow <- sample(chowPopulation,N)
        se <- sd(chow)/sqrt(N)
        interval <- c(mean(chow)-Q*se, mean(chow)+Q*se)
        covered <- mean(chowPopulation) <= interval[2] & mean(chowPopulation) >= interval[1]
        color <- ifelse(covered,1,2)
        lines(interval, c(i,i),col=color)
}



mypar()
plot(mean(chowPopulation) + c(-7,7), c(1,1), type="n",
     xlab="weight", ylab="interval", ylim=c(1,B))
abline(v=mean(chowPopulation))
##Q <- qnorm(1- 0.05/2) ##no longer normal so use:
Q <- qt(1- 0.05/2, df=4)
N <- 5
for (i in 1:B) {
        chow <- sample(chowPopulation, N)
        se <- sd(chow)/sqrt(N)
        interval <- c(mean(chow)-Q*se, mean(chow)+Q*se )
        covered <- mean(chowPopulation) <= interval[2] & mean(chowPopulation) >= interval[1]
        color <- ifelse(covered,1,2)
        lines(interval, c(i,i),col=color)
}



#Power calculations
dat =read.csv("data/mice_pheno.csv")
controlpop <- filter(dat,Sex=="F" & Diet == "chow") %>% select(Bodyweight) %>% unlist
hfpop <- filter(dat,Sex=="F" & Diet == "hf") %>% select(Bodyweight) %>% unlist
mu_hf <- mean(hfpop)
mu_control <- mean(controlpop)
print(mu_hf - mu_control)

set.seed(1)
N <- 5
hf <- sample(hfpop,N)
control <- sample(controlpop,N)
t.test(hf,control)$p.value
#don't reject null hypothesis that numbers are different
N <- 12
hf <- sample(hfpop,N)
control <- sample(controlpop,N)
t.test(hf,control)$p.value


N <- 12
alpha <- 0.05
B <- 2000


reject <- function(N,alpha=0.05) {
        hf <- sample(hfpop,N)
        control <- sample(controlpop,N)
        pval <- t.test(hf,control)$p.value
        pval < alpha
}
rejections <- replicate(B,reject(N))
mean(rejections)

Ns <-  seq(5,50,5)
power <- sapply(Ns,function(N) {
        rejections <- replicate(B,reject(N))
        mean(rejections)
}       
)

plot(Ns,power,type="b")
