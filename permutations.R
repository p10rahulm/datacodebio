rm(list=ls())

babies <- read.table("data/babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obs <- mean(smokers) - mean(nonsmokers)
N <- 10
set.seed(1)
avgdiff <- replicate(1000, {
        all <- sample(c(smokers,nonsmokers))
        newcontrols <- all[1:N]
        newtreatments <- all[(N+1):(2*N)]
        return(mean(newtreatments) - mean(newcontrols))
})
mypar()
hist(avgdiff)
abline(v=obs, col="red", lwd=2)
pval <- (sum(abs(avgdiff) > abs(obs)) + 1) / (length(avgdiff) + 1)


#same as above for median
obs <- median(smokers) - median(nonsmokers)
N <- 10
set.seed(1)
avgdiff <- replicate(1000, {
        all <- sample(c(smokers,nonsmokers))
        newcontrols <- all[1:N]
        newtreatments <- all[(N+1):(2*N)]
        return(median(newtreatments) - median(newcontrols))
})
mypar()
hist(avgdiff)
abline(v=obs, col="red", lwd=2)
pval <- (sum(abs(avgdiff) > abs(obs)) + 1) / (length(avgdiff) + 1)
