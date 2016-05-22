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



