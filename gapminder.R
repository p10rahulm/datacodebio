install.packages("gapminder")
library(gapminder)
data(gapminder)
head(gapminder)
lifex1952=as.numeric(gapminder[gapminder[,3]==1952,4]$lifeExp)
#lifex1952 = as.numeric(lifex1952$lifeExp)
hist(lifex1952)
class(lifex1952)
mean(lifex1952<40)


# Suppose we want to plot the proportions of countries with life expectancy 
# 'q' for a range of different years. R has a built in function for this, 
# plot(ecdf(x)), but suppose we didn't know this. The function is quite easy 
# to build, by turning the code from question 1.1 into a custom function, 
# and then using sapply(). Our custom function will take an input variable 'q', 
# and return the proportion of countries in 'x' less than or equal to q. The 
# curly brackets { and }, allow us to write an R function which spans multiple lines:


prop = function(q) {
        mean(lifex1952 <= q)
}
qs = seq(from=min(lifex1952), to=max(lifex1952), length=20)
props = sapply(qs, prop)
plot(qs, props)
