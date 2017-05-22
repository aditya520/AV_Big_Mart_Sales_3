rm(list = ls())
setwd("C:/data/Big-Mart/")
train = read.csv("Train_UWu5bXk.csv",na.strings = c("","NA"))
test = read.csv("Test_u94Q5KV.csv",na.strings = c("","NA"))
summary(train)
summary(test)
train_sales = train$Item_Outlet_Sales
library(ggplot2)
#plot(train)

train$Item_Outlet_Sales = NULL

full = rbind(train,test)
summary(full)

summary(train$Item_Weight)
library(mice)
md.pattern(train)

ggplot(train,aes(x = Item_Weight)) + geom_density()
ggplot(train,aes(x = Item_Type,y = Item_Weight)) + geom_bar(stat = "identity") + 
          theme(axis.text.x = element_text(angle = 60,hjust = 1))

mean_ident = aggregate(full$Item_Weight,list(full$Item_Identifier),mean,na.rm =T)

colnames(mean_ident) = c("Item_Identifier","Item_Weight")
train$Item_Weight = NULL

train = merge(train,mean_ident,by = "Item_Identifier")
head(train)
test = merge(test,mean_ident,by = "Item_Identifier",all.x = T)

train = train[!is.na(train$Item_Weight),]

summary(test)
test$Item_Weight.x = NULL
test$Item_Weight = test$Item_Weight.y
test$Item_Weight.y = NULL

#imputed Size with small using excel

summary(train)
summary(test)

''
