setwd("C:/Users/ing12709/Desktop/Deepika/CrossSelling")

list.of.packages <- c("randomForest","caret","dummies","arules", "tidyr","Matrix")

new.packages<-list.of.packages[!list.of.packages %in% rownames(installed.packages())]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, library, character.only=T)

rm(list=ls())

ClickStream <- read.csv("ClickStream.txt",stringsAsFactors=FALSE,header = T)
ProductCat <- read.delim("ProductCategoryData.txt",stringsAsFactors=FALSE,header = T)
UserProfile <- read.delim("UserProfileData.txt",stringsAsFactors=FALSE,header = T)

Devide date and Time
ClickStreamNew=separate(data = ClickStream, col = Timestamp, into = c("Day","Time"), sep = " ")

for(i in nrow(ClickStreamNew))
{
  if(!all(is.na(as.Date(as.character(ClickStreamNew[i,1]),format="%d/%m/%y"))))
  {#"9/3/2012"
    ClickStreamNew[i,1] <- as.character(as.Date(ClickStreamNew[i,1],format ="%d/%m/%Y"))
  }
  else
  {#"22-03-2012"
    ClickStreamNew[i,1] <- as.character(as.Date(ClickStreamNew[i,1],format ="%d-%m-%Y"))
  }
}

cat("Clickstream data set property, number of rows: ", nrow(ClickStreamNew), ", number of columns: ", ncol(ClickStreamNew))
ClickStream_ProductCat <- merge(ClickStreamNew, ProductCat, by = "URL",all.x = T)
cat("ClickStream_ProductCat data set property, number of rows: ", nrow(ClickStream_ProductCat), ", number of columns: ", ncol(ClickStream_ProductCat))

ClickStream_ProductCat_UserProfile=merge(ClickStream_ProductCat, UserProfile, by = "User.ID",all.x = T)
cat("ClickStream_ProductCat_UserProfile data set property, number of rows: ", nrow(ClickStream_ProductCat_UserProfile), ", number of columns: ", ncol(ClickStream_ProductCat_UserProfile))

ClickStream_ProductCat_UserProfile_Cleaned <- ClickStream_ProductCat_UserProfile[,-5]
cat("ClickStream_ProductCat_UserProfile_Cleaned data set property, number of rows: ", nrow(ClickStream_ProductCat_UserProfile_Cleaned), ", number of columns: ", ncol(ClickStream_ProductCat_UserProfile_Cleaned))


ClickStream_ProductCat_UserProfile_Cleaned$newCol=1

combineData <- reshape(ClickStream_ProductCat_UserProfile_Cleaned[],timevar = "Category",idvar = c("User.ID"),direction = "wide",v.names = "newCol")

combineData[,10:22][combineData[,10:22]==1] <- "TRUE"
combineData[,10:22][is.na(combineData[,10:22])] <- "FALSE"

rules<-apriori(combineData[,10:22],parameter = list(minlen=1,maxlen=2,conf=.1))
rules.sub <- subset(rules, subset = rhs %pin% "TRUE" & lhs %pin% "TRUE")
inspect(head(sort(rules, by="lift"),3));
