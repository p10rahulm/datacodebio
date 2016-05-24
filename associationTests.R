#Lady can test whether tea was poured before or after milk.
#4 sets of 2 Teas where tea is poured before in one and after in another
#what is probability of picking 3 of these four sets correctly?
# approximated by 4 correct and 4 incorrect answers where one picks 4 of these 8

tab <- matrix(c(3,1,1,3),2,2)
rownames(tab)<-c("Poured Before","Poured After")
colnames(tab)<-c("Guessed before","Guessed after")
tab

fisher.test(tab,alternative="greater")
##
## Fisher's Exact Test for Count Data
##
## data: tab
## p-value = 0.2429
## alternative hypothesis: true odds ratio is greater than 1
## 95 percent confidence interval:
## 0.3135693 Inf
## sample estimates:
## odds ratio
## 6.408309


library(downloader)
url="https://studio.edx.org/c4x/HarvardX/PH525.1x/asset/assoctest.csv"
filename=basename(url)
dirnm = "data/"
p = download.file(url,paste(dirnm,filename,sep=""),method = "curl")
d = read.csv("data/assoctest.csv")
head(d)
tab <- table(d$allele,d$case)
tab
chisq.test(tab)
fisher.test(tab)
