setwd("C:/Users/ing12709/Desktop/Deepika/")

list.of.packages=c("randomForest", "caret","dummies","arules")

new.packages<-list.of.packages[!list.of.packages %in% rownames(installed.packages())]
if(length(new.packages)) install.packages(new.packages)
library(list.of.packages)

rm(list=ls())

ClickStream <- read.delim("ClickStream.txt",stringsAsFactors=FALSE,header = T)
ProductCat <- read.delim("ProductCategoryData.txt",stringsAsFactors=FALSE,header = T)
UserProfile <- read.delim("UserProfileData.txt",stringsAsFactors=FALSE,header = T)

ClickStream <- ClickStream[,1:7]

cat("Clickstream data set property, number of rows: ", nrow(ClickStream), ", number of columns: ", ncol(ClickStream))
ClickStream_ProductCat <- merge(ClickStream, ProductCat, by = "URL",all.x = T)
cat("ClickStream_ProductCat data set property, number of rows: ", nrow(ClickStream_ProductCat), ", number of columns: ", ncol(ClickStream_ProductCat))

ClickStream_ProductCat_UserProfile=merge(ClickStream_ProductCat, UserProfile, by = "User.ID",all.x = T)
cat("ClickStream_ProductCat_UserProfile data set property, number of rows: ", nrow(ClickStream_ProductCat_UserProfile), ", number of columns: ", ncol(ClickStream_ProductCat_UserProfile))

ClickStream_ProductCat_UserProfile_Cleaned <- ClickStream_ProductCat_UserProfile[,c(-2,-4,-5)]
cat("ClickStream_rmuw data set property, number of rows: ", nrow(ClickStream_rmuw), ", number of columns: ", ncol(ClickStream_rmuw))

summary(is.na(ClickStream_ProductCat_UserProfile_Cleaned))

ClickStream_ProductCat_UserProfile_Cleaned[ClickStream_ProductCat_UserProfile_Cleaned$College.Education == "Unknown"]=NA

sum(ClickStream_ProductCat_UserProfile_Cleaned == "Unknown",na.rm = T)

ClickStream_ProductCat_UserProfile_Cleaned$newCol=1

combineData <- reshape(ClickStream_ProductCat_UserProfile_Cleaned[],timevar = "Category",idvar = c("User.ID"),direction = "wide",v.names = "newCol")

combinedData <- spread(ClickStream_ProductCat_UserProfile_Cleaned,Test,Result)
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(combineData,2,pMiss)

combineData <- combineData[,c(-5,-6)
combineData[is.na(combineData)] <- 0

toFactor <= function(x){as.factor(x)}
apply(combineData[,5:17],2,toFactor)

