setwd("D:/Practice/R/AnalyticsVidya/Big Marts Sales 3/")
train = read.csv("train.csv")
test = read.csv("test.csv")

test$Item_Outlet_Sales = 1

combi = rbind(train,test)

combi$Item_Weight[is.na(combi$Item_Weight)] = median(combi$Item_Weight,na.rm = T)
combi$Item_Visibility = ifelse(combi$Item_Visibility == 0,median(combi$Item_Visibility),
                               combi$Item_Visibility)
table(combi$Outlet_Size,combi$Outlet_Type)
levels(combi$Outlet_Size)[1] = "Other"

my_data = subset(combi, select = -c(Item_Outlet_Sales,Item_Identifier,
                                    Outlet_Identifier))
colnames(my_data)
str(my_data)
summary(my_data)

library(dummies)

new_my_data <- dummy.data.frame(my_data, names = c("Item_Fat_Content","Item_Type",
                                                   "Outlet_Establishment_Year","Outlet_Size",
                                                   "Outlet_Location_Type","Outlet_Type"))

str(new_my_data)
pca.train = new_my_data[1:nrow(train),]
pca.test = new_my_data[-(1:nrow(train)),]

###PCA
prin_comp = prcomp(pca.train,scale. = T)
names(prin_comp)
prin_comp$center
prin_comp$scale

prin_comp$rotation

prin_comp$rotation[1:5,1:4]

dim(prin_comp$x)

biplot(prin_comp,scale = 0)

std_dev = prin_comp$sdev
pr_var = std_dev ^ 2

pr_var[1:10]

prop_varex = pr_var/sum(pr_var)
prop_varex[1:20]

plot(prop_varex,type = "b")

plot(cumsum(prop_varex),type = "b")

##PRediction

train.data = data.frame(Item_Outlet_Sales = train$Item_Outlet_Sales,prin_comp$x)
train.data = train.data[,1:31]

library(rpart)
library(rpart.plot)
CARTmodel = rpart(Item_Outlet_Sales ~ .,data = train.data,method = "anova")
prp(CARTmodel)

test.data = predict(prin_comp,pca.test)
test.data = as.data.frame(test.data)
test.data = test.data[,1:30]

CARTmodel.pred = predict(CARTmodel,test.data)
sample = read.csv("Sample_sub.csv")
final.sub = data.frame(Item_Identifier = sample$Item_Identifier, Outlet_Identifier = sample$Outlet_Identifier, Item_Outlet_Sales = CARTmodel.pred)
write.csv(final.sub,"pca_rpart.csv",row.names = F)
