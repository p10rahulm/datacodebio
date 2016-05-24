rm(list=ls())
dat <- read.csv("data/femaleMiceWeights.csv")
control <- dat[dat$Diet=="chow","Bodyweight"]
treatment <- dat[dat$Diet=="hf","Bodyweight"]
diff <- mean(treatment) - mean(control)
#Standard Error = Sigma/Sqrt(n)
#standard errors of diff = sqrt(sum (standard errors of each)^2)

SEcontrol = sd(control)/sqrt(length(control))
SEtreat = sd(treatment)/sqrt(length(treatment))
SEdiff = sqrt(SEcontrol^2 + SEtreat^2)
tstat <- diff/SEdiff
righttail <- 1 - pnorm(abs(tstat))
lefttail <- pnorm(-abs(tstat))
pval <- lefttail + righttail
print(pval)

# As explained in the previous section, the CLT tells us that for large sample sizes, both sample averages
# mean(treatment) and mean(control) are normal. Statistical theory tells us that the difference of two
# normally distributed random variables is again normal, so CLT tells us that tstat is approximately
# normal with mean 0 (the null hypothesis) and SD 1 (we divided by its SE).

getnull <- function(N = 10) {
        population <- dat$Bodyweight
        n <- 10000
        nulls <- vector("numeric",n)
        for (i in 1:n){
                control <- sample(population,N)
                treatment <- sample(population,N)
                se <- sqrt(var(treatment)/N+var(control)/N)
                nulls[i] <- (mean(treatment)-mean(control))/se
        }
        return(nulls)
}

library(rafalib)
mypar(2,2)
qqnorm(getnull(12))
abline(0,1)
qqnorm(getnull(9))
abline(0,1)
qqnorm(getnull(6))
abline(0,1)
qqnorm(getnull(3))
abline(0,1)


#running the ttest
ttest <- t.test(treatment,control)
ttest
mypar(1,2)
qqnorm(control)
qqline(control)
qqnorm(treatment)
qqline(treatment)




#Exercise
dat <- read.csv("data/femalemiceweights.csv")
getz <- function(p,n) {
        set.seed(1)
        # n=100
        # p=1/6
        q = replicate(10000,{
        x <- sample(1:round(1/p,0), n, replace=TRUE)
        z <-  (mean(x==round(1/p,0)) - p) / sqrt(p*(1-p)/n)
        z
        })
        q
}
mypar(2,2)
qqnorm(getz(0.5,5))
qqline(getz(0.5,5))
qqnorm(getz(0.5,30))
qqline(getz(0.5,30))
qqnorm(getz(0.01,30))
qqline(getz(0.01,30))
qqnorm(getz(0.01,100))
qqline(getz(0.01,100))
head(getz(0.5,5))
qqnorm(getz(0.01,1000))
qqline(getz(0.01,1000))
qqnorm(getz(0.05,300))
qqline(getz(0.05,300))
qqnorm(getz(0.15,75))
qqline(getz(0.15,75))



X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist
mean(X)
#popultaion sd
popsd(X)*sqrt(12)
2/sd(X)
