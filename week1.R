dat <- read.csv("Data/femaleMiceWeights.csv")
dat
length(dat$Diet)
dat[,1,drop=F]=="hf"
mean(subset(dat,Diet =="hf")[,2])
set.seed(1)
?sample
sample(13:24,1)
dat[sample(13:24,1),2]
install.packages("rafalib")
install.packages("downloader")
library(rafalib);library(downloader)
install.packages("dplyr")
library(dplyr)
dat <- read.csv("Data/femaleMiceWeights.csv")
View(dat)
p <- dat[dat$Diet=="hf",]
controls <- filter(dat,Diet=="chow")
controlsbw<-select(controls,Bodyweight)
unlist(controlsbw)
controls <- filter(dat,Diet=="chow") %>% select(Bodyweight) %>% unlist()
mean(controls)
dplyr::summarise(filter(msleep, order=="Primates"),mean(sleep_total))
summarise(filter(msleep, order=="Primates"),mean(sleep_total))


#Data linked to Mamalian sleep
url="https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- basename(url)
dirname = "data/"
download(url,paste(dirname,filename,sep=""))
msleep = read.csv("data/msleep_ggplot2.csv")
View(msleep)
class(msleep)
filter(msleep, order=="Primates") %>% nrow()
filter(msleep, order=="Primates") %>% class()
filter(msleep, order=="Primates") %>% select(sleep_total) %>%class
filter(msleep, order=="Primates") %>% select(sleep_total) %>%unlist %>% mean
filter(msleep, order=="Primates") %>% summary
dplyr::summarise(filter(msleep, order=="Primates"),mean(sleep_total))
summarise(filter(msleep, order=="Primates"),mean(sleep_total))
summarise(filter(msleep, order=="Primates"),sleeptm=mean(sleep_total))
