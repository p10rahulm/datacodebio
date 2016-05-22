install.packages("devtools")
filename <- "data/femalemiceweights.csv"
dat <- read.csv(filename)
head(dat)

library(dplyr)
control <- filter(dat,Diet=="chow") %>% select(Bodyweight) %>% unlist
treatment <- filter(dat,Diet == "hf") %>% select(Bodyweight) %>% unlist
print(mean(treatment))
print(mean(control))
obsdiff <- mean(treatment)-mean(control)
print(obsdiff)


#starting with downloading
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=paste("data/",filename,sep=""))
x <- unlist( read.csv(paste("data/",filename,sep="")) )


#Question 1: chance of 1 gram difference
p1 <- vector("numeric",1000)
set.seed(1)
for (i in 1:1000){
        p1[i]=mean(sample(x,5,replace=F))
}
mean(abs(p1-mean(x))> 1)


#nothing much changes when we increase number of computes from 1000 to 10000
p2 <- vector("numeric",10000)
set.seed(1)
for (i in 1:10000){
        p2[i]=mean(sample(x,5,replace=F))
}
mean(abs(p2-mean(x))> 1)

#watch as we increase sample size from 5 to 50, how the probability drops!
p3 <- vector("numeric",1000)
set.seed(1)
for (i in 1:1000){
        p3[i]=mean(sample(x,50,replace=F))
}
mean(abs(p3-mean(x))> 1)
mean(p3<25)-mean(p3<23)
hist(p1)
hist(p3)
pnorm(25,23.9,0.43)-pnorm(23,23.9,0.43)







#       If you have the population data, then find probability that
#       you can randomly get the difference you observed. 
#       this is called the p value

filename <- "data/femaleControlsPopulation.csv"
population <- read.csv(filename)
population <- unlist(population)
centraltomean <- function(n){
        obsdiff=vector("numeric",length=n)
        for (i  in (1:n)) {
                control <- sample(population,12)
                treatment <- sample(population,12)
                obsdiff[i]=mean(control)-mean(treatment)
        }
        obsdiff
}
pvalue <- function(nval,n=10000){
        obsdiff=vector("numeric",length=n)
        for (i  in (1:n)) {
                control <- sample(population,12)
                treatment <- sample(population,12)
                obsdiff[i]=mean(control)-mean(treatment)
        }
        sum(abs(obsdiff)>=nval)/n
}
for (i in c(1,10,100,1000,10000,100000)) {
print(paste("mean for " , i,"observations = ",mean(centraltomean(i))))

}





#Mice Phone Data
dat <- read.csv("data/mice_pheno.csv") 
dat <- na.omit (dat)
head(dat)
p1 = subset(dat,dat$Sex=="M" & dat$Diet=="chow")$Bodyweight
p2 = filter(dat,dat$Sex=="M" & dat$Diet=="chow")$Bodyweight
p3 = select(filter(dat,dat$Sex=="M" & dat$Diet=="chow"),Bodyweight)
p4=dat[dat$Sex=="M" & dat$Diet=="chow",]$Bodyweight
p5=dat[dat$Sex=="M" & dat$Diet=="chow",3]
p6=select(dat[dat$Sex=="M" & dat$Diet=="chow",],Bodyweight)

library(rafalib)
popsd(p5)
set.seed(1)
mean(sample(p5,25))
mean(sample(dat,25))
set.seed(1)
mean(subset(dat,dat$Sex=="M" & dat$Diet=="hf")$Bodyweight)

set.seed(1)
mean(sample(dat[dat$Sex=="M" & dat$Diet=="hf","Bodyweight"],25))


popsd(dat[,"Bodyweight"])
6.256706
sd(dat[,"Bodyweight"])
6.260429
popsd(subset(dat,dat$Sex=="M" & dat$Diet=="hf")$Bodyweight)


set.seed(1)
f2 = mean(sample(dat[dat$Sex=="F" & dat$Diet=="hf","Bodyweight"],25))
f3 = mean(sample(dat[dat$Sex=="F" & dat$Diet=="chow","Bodyweight"],25))
f3
(mean(dat[dat$Sex=="F" & dat$Diet=="chow",3])-23.1692)-
        (mean(subset(dat,dat$Sex=="F" & dat$Diet=="hf")$Bodyweight)-26.2812)

p = dat[dat$Sex=="M" & dat$Diet=="chow",3]
sdt = popsd(p)
mnt = mean(p)
mean(p<mnt+sdt) - mean(p<mnt-sdt)
mean(p<mnt+2*sdt) - mean(p<mnt-2*sdt)
mean(p<mnt+3*sdt) - mean(p<mnt-3*sdt)


qqnorm(p)
abline(0,1)




#drawing quantile quantile plots to check our distribution with the normal distribution

mypar(2,2)
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)



#Using replicate to get repeat sample averaging many times
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)
mean(avgs)
popsd(avgs)
