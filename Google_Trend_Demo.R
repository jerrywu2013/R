setwd("D:\\2016-03-05 成大醫學院演講\\Data")
getwd()
x<-read.table(file("20151202_GunControlGunShopPostSanBernardino.csv", encoding = "UTF-8"), header = TRUE, sep = ",")
x<-x[-1,]
colnames(x)<-c("state","value")

tab1<-table(x$value)[1:2]
barplot(tab1, legend=TRUE)
barplot(tab1, legend=TRUE, col= rainbow(10))
barplot(tab1, legend=TRUE, col= c("blue","red"))


