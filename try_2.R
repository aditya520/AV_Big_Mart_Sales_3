rm(list = ls())
#setwd("C:/data/Big-Mart/")
setwd("D:/Practice/R/AnalyticsVidya/Big Marts Sales 3/")
train = read.csv("train.csv",na.strings = c("","NA"),stringsAsFactors = F)
test = read.csv("test.csv",na.strings = c("","NA"),stringsAsFactors = F)
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

#imput Size with small (Done analysis using excel)
train$Outlet_Size[is.na(train$Outlet_Size)] = "Small"
test$Outlet_Size[is.na(test$Outlet_Size)] = "Small"



summary(train)
summary(test)

length(train$Item_Visibility[train$Item_Visibility == 0])

#Feature Engineering

train$Item_Outlet_Sales = train_sales
Item_quant_per_outlet = train$Item_Outlet_Sales / train$Item_MRP

train$Item_quant_per_outlet = Item_quant_per_outlet
item_mean = aggregate(train$Item_quant_per_outlet,list(train$Item_Identifier),mean)
colnames(item_mean) = c("Item_Identifier","Item_mean_sales")

train = merge(train,item_mean,by = "Item_Identifier")
test = merge(test,item_mean,by = "Item_Identifier",all.x = T)
summary(train)
summary(test)

store_mean = aggregate(train$Item_quant_per_outlet,list(train$Outlet_Identifier),mean)
colnames(store_mean) = c("Outlet_Identifier","Outlet_Mean")

train = merge(train,store_mean,by = "Outlet_Identifier")
test = merge(test,store_mean,by = "Outlet_Identifier",all.x = T)

submit = read.csv("Sample_sub.csv")
library(xgboost)


train$Item_quant_per_outlet = NULL
train_sales = train$Item_Outlet_Sales
train$Item_Outlet_Sales = NULL

train$Outlet_Identifier = NULL
train$Item_Identifier = NULL
test$Item_Identifier = NULL
test$Outlet_Identifier = NULL

train$Item_Fat_Content[train$Item_Fat_Content == "LF"] = 1
train$Item_Fat_Content[train$Item_Fat_Content == "Low Fat"] = 1
train$Item_Fat_Content[train$Item_Fat_Content == "low fat"] = 1
train$Item_Fat_Content[train$Item_Fat_Content == "reg"] = 2
train$Item_Fat_Content[train$Item_Fat_Content == "Regular"] = 2
train$Item_Fat_Content = as.integer(train$Item_Fat_Content)

test$Item_Fat_Content[test$Item_Fat_Content == "LF"] = 1
test$Item_Fat_Content[test$Item_Fat_Content == "Low Fat"] = 1
test$Item_Fat_Content[test$Item_Fat_Content == "low fat"] = 1
test$Item_Fat_Content[test$Item_Fat_Content == "reg"] = 2
test$Item_Fat_Content[test$Item_Fat_Content == "Regular"] = 2
test$Item_Fat_Content = as.integer(test$Item_Fat_Content)

train$Item_Visibility = as.integer(train$Item_Visibility)
test$Item_Visibility = as.integer(test$Item_Visibility)
