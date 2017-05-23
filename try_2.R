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

#Item_type
train$Item_Type[train$Item_Type == "Baking Goods"] = 1
test$Item_Type[test$Item_Type == "Baking Goods"] = 1

train$Item_Type[train$Item_Type == "Breads"] = 2
test$Item_Type[test$Item_Type == "Breads"] = 2

train$Item_Type[train$Item_Type == "Breakfast"] = 3
test$Item_Type[test$Item_Type == "Breakfast"] = 3

train$Item_Type[train$Item_Type == "Canned"] = 4
test$Item_Type[test$Item_Type == "Canned"] = 4

train$Item_Type[train$Item_Type == "Dairy"] = 5
test$Item_Type[test$Item_Type == "Dairy"] = 5

train$Item_Type[train$Item_Type == "Frozen Foods"] = 6
test$Item_Type[test$Item_Type == "Frozen Foods"] = 6

train$Item_Type[train$Item_Type == "Fruits and Vegetables"] = 7
test$Item_Type[test$Item_Type == "Fruits and Vegetables"] = 7

train$Item_Type[train$Item_Type == "Hard Drinks"] = 8
test$Item_Type[test$Item_Type == "Hard Drinks"] = 8

train$Item_Type[train$Item_Type == "Health and Hygiene"] = 9
test$Item_Type[test$Item_Type == "Health and Hygiene"] = 9

train$Item_Type[train$Item_Type == "Household"] = 10
test$Item_Type[test$Item_Type == "Household"] = 10

train$Item_Type[train$Item_Type == "Meat"] = 11
test$Item_Type[test$Item_Type == "Meat"] = 11

train$Item_Type[train$Item_Type == "Others"] = 12
test$Item_Type[test$Item_Type == "Others"] = 12

train$Item_Type[train$Item_Type == "Seafood"] = 13
test$Item_Type[test$Item_Type == "Seafood"] = 13

train$Item_Type[train$Item_Type == "Snack Foods"] = 14
test$Item_Type[test$Item_Type == "Snack Foods"] = 14

train$Item_Type[train$Item_Type == "Soft Drinks"] = 15
test$Item_Type[test$Item_Type == "Soft Drinks"] = 15

train$Item_Type[train$Item_Type == "Starchy Foods"] = 16
test$Item_Type[test$Item_Type == "Starchy Foods"] = 16

train$Item_Type = as.integer(train$Item_Type)
test$Item_Type = as.integer(test$Item_Type)

table(train$Item_Type)
table(test$Item_Type)

train$Item_MRP = as.integer(train$Item_MRP)
test$Item_MRP = as.integer(test$Item_MRP)

train$Outlet_Establishment_Year = as.integer(train$Outlet_Establishment_Year)
test$Outlet_Establishment_Year = as.integer(test$Outlet_Establishment_Year)

train$Outlet_Size[train$Outlet_Size == "Small"] = 1
test$Outlet_Size[test$Outlet_Size == "Small"] = 1

train$Outlet_Size[train$Outlet_Size == "Medium"] = 2
test$Outlet_Size[test$Outlet_Size == "Medium"] = 2

train$Outlet_Size[train$Outlet_Size == "High"] = 3
test$Outlet_Size[test$Outlet_Size == "High"] = 3

train$Outlet_Size = as.integer(train$Outlet_Size)
test$Outlet_Size = as.integer(test$Outlet_Size)

table(train$Outlet_Size)
table(test$Outlet_Size)

train$Outlet_Location_Type[train$Outlet_Location_Type == "Tier 1"] = 1
test$Outlet_Location_Type[test$Outlet_Location_Type == "Tier 1"] = 1

train$Outlet_Location_Type[train$Outlet_Location_Type == "Tier 2"] = 2
test$Outlet_Location_Type[test$Outlet_Location_Type == "Tier 2"] = 2

train$Outlet_Location_Type[train$Outlet_Location_Type == "Tier 3"] = 3
test$Outlet_Location_Type[test$Outlet_Location_Type == "Tier 3"] = 3

train$Outlet_Location_Type = as.integer(train$Outlet_Location_Type)
test$Outlet_Location_Type = as.integer(test$Outlet_Location_Type)

table(train$Outlet_Location_Type)
table(test$Outlet_Location_Type)

train$Outlet_Type[train$Outlet_Type == "Grocery Store"] = 1
test$Outlet_Type[test$Outlet_Type == "Grocery Store"] = 1

train$Outlet_Type[train$Outlet_Type == "Supermarket Type1"] = 2
test$Outlet_Type[test$Outlet_Type == "Supermarket Type1"] = 2

train$Outlet_Type[train$Outlet_Type == "Supermarket Type2"] = 3
test$Outlet_Type[test$Outlet_Type == "Supermarket Type2"] = 3

train$Outlet_Type[train$Outlet_Type == "Supermarket Type3"] = 4
test$Outlet_Type[test$Outlet_Type == "Supermarket Type3"] = 4

train$Outlet_Type = as.integer(train$Outlet_Type)
test$Outlet_Type = as.integer(test$Outlet_Type)

table(train$Outlet_Type)
table(test$Outlet_Type)

train$Item_Weight = as.integer(train$Item_Weight)
test$Item_Weight = as.integer(test$Item_Weight)

train$Item_mean_sales = as.integer(train$Item_mean_sales)
test$Item_mean_sales = as.integer(test$Item_mean_sales)

train$Outlet_Mean = as.integer(train$Outlet_Mean)
test$Outlet_Mean = as.integer(test$Outlet_Mean)

