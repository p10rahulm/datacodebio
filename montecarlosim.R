set.seed(1)

controlPopulation <-  read.csv("data/femaleControlsPopulation.csv")
controlPopulation <- unlist(controlPopulation)
#controlPopulation <-  filter(dat,Sex=="M" & Diet == "chow") %>% select(Bodyweight) %>% unlist
ttestgenerator <- function(n) {
        #note that here we have a false "high fat" group where we actually
        #sample from the nonsmokers. this is because we are modeling the *null*
        cases <- sample(controlPopulation,n)
        controls <- sample(controlPopulation,n)
        tstat <- (mean(cases)-mean(controls)) /
                sqrt( var(cases)/n + var(controls)/n )
        return(tstat)
}
set.seed(1)

ttests <- replicate(1000, ttestgenerator(10))

mypar(1,2)
hist(ttests)
qqnorm(ttests)
abline(0,1)



#trying with size 3 of sample
ttests1 <- replicate(1000, ttestgenerator(3))
qqnorm(ttests1)
abline(0,1)


ps <- (seq(0,999)+0.5)/1000
qqplot(qt(ps,df=2*3-2),ttests,xlim=c(-6,6),ylim=c(-6,6))
abline(0,1)

qqnorm(controlPopulation)
qqline(controlPopulation)


#generate controls population
set.seed(1)
controls<- rnorm(5000, mean=24, sd=3.5)
ttestgenerator2 <- function(n,mean=24,sd=3.5){
        cases <- rnorm(n,mean,sd)
        controls <- rnorm(n,mean,sd)
        tstat <- (mean(cases)-mean(controls))/sqrt((var(cases)/n)+(var(controls)/n))
        return(tstat)
}
ttests2 <- replicate(1000,ttestgenerator2(3))
qqnorm(ttests2)
abline(0,1)

ps <- (seq(0,999)+0.5)/1000
qqplot(qt(ps,df=2*3-2),ttests2,xlim=c(-6,6),ylim=c(-6,6))
abline(0,1)
