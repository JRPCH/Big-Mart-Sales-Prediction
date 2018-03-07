library(rpart)
library(ggplot2)
library(GGally)
library(randomForest)
library(leaps)
library(gam)
library(dplyr)
library(nnet)
library(caret)
library(nnet)
setwd('/Users/swapnilpatil/Study/MS-Bana/Projects/Big Mart Problem')
train<-read.csv('train.csv')
head(train)
dim(train)
sum(duplicated(train$Item_Identifier))
summary(train)
str(train)
hist(train$Item_Weight)
table(train$Item_Identifier)
table(train$Item_Fat_Content)
train$Item_Fat_Content[train$Item_Fat_Content=='LF']<-'Low Fat'
train$Item_Fat_Content[train$Item_Fat_Content=='low fat']<-'Low Fat'
train$Item_Fat_Content[train$Item_Fat_Content=='reg']<-'Regular'
train$Item_Fat_Content<-as.character(train$Item_Fat_Content)
train$Item_Fat_Content<-as.factor(train$Item_Fat_Content)
table(train$Item_Fat_Content)
table(train$Item_Type)
table(train$Outlet_Identifier)
table(train$Outlet_Size)
table(train$Outlet_Identifier,train$Outlet_Size)

table(train$Outlet_Location_Type,train$Outlet_Size)
table(train$Outlet_Location_Type)
table(train$Outlet_Type)
table(train$Outlet_Type,train$Outlet_Size)

### We cannot conclude anything about the outlet size YET
ggplot(data = train,aes(x = Outlet_Size,y = Item_Outlet_Sales))+geom_boxplot()
## the missing outlet looks like 'small'
train$Outlet_Size<-as.character(train$Outlet_Size)
train$Outlet_Size[train$Outlet_Size=='']<-'Small'
train$Outlet_Size<-as.factor(train$Outlet_Size)
table(train$Outlet_Size)

### there are missing values of Item-Weight
# We need to check if the item weight is same for all the item_identifiers
weightTest<-train[complete.cases(train),]%>%group_by(Item_Identifier)%>%summarise(count=n(),weight=mean(Item_Weight),var=var(Item_Weight))
head(weightTest)
summary(weightTest$var)
## we can confirm that the weight is same across the item_identifiers

r <-merge(train, weightTest, by="Item_Identifier",all.x = TRUE)
colSums(is.na(r))
r$Item_Weight <-ifelse(is.na(r$Item_Weight), r$weight, r$Item_Weight)
colSums(is.na(r))

train<-r[,-c(13,14,15)]
colSums(is.na(train))

weightTest<-as.data.frame(weightTest)

## Checking if we have not made any mistake
weightTest<-train[complete.cases(train),]%>%group_by(Item_Identifier)%>%summarise(count=n(),weight=mean(Item_Weight),var=var(Item_Weight))
head(weightTest)
summary(weightTest$var)

### We can add new variable as years of operation
train$YOP<-2017-train$Outlet_Establishment_Year

###EDA

## 1. Checking categorical variables
ggplot(data = train,aes(x = Item_Fat_Content,y = Item_Outlet_Sales))+geom_boxplot()
ggplot(data = train,aes(x = Item_Type,y = Item_Outlet_Sales))+geom_boxplot() ## important
ggplot(data = train,aes(x = Outlet_Identifier,y = Item_Outlet_Sales))+geom_boxplot() ## This one looks important
ggplot(data = train,aes(x = Outlet_Size,y = Item_Outlet_Sales))+geom_boxplot() ## Important
ggplot(data = train,aes(x = Outlet_Location_Type,y = Item_Outlet_Sales))+geom_boxplot()
ggplot(data = train,aes(x = Outlet_Type,y = Item_Outlet_Sales))+geom_boxplot()## Important

## Checking Numeric Variables
ggplot(data = train,aes(x = Item_Weight,y = Item_Outlet_Sales))+geom_point(alpha=0.2)
ggplot(data = train,aes(y = Item_Outlet_Sales,x = Item_Visibility))+geom_point(alpha=0.2)
ggplot(data = train,aes(y = Item_Outlet_Sales,x = Item_MRP))+geom_point(alpha=0.2) ## Possitive Correlation
ggplot(data = train,aes(y = Item_Outlet_Sales,x = YOP))+geom_point(alpha=0.2)

## Exploring the interplay
ggplot(data = train,aes(y = Item_Outlet_Sales,x = Item_MRP,col=Item_Type))+geom_point(alpha=0.4)
ggplot(data = train,aes(y = Item_Outlet_Sales,x = Item_MRP,col=Outlet_Identifier))+geom_point(alpha=0.4) ## Important
ggplot(data = train,aes(y = Item_Outlet_Sales,x = Item_MRP,col=Outlet_Size))+geom_point(alpha=0.4)## Some Relation
ggplot(data = train,aes(y = Item_Outlet_Sales,x = Item_MRP,col=Outlet_Type))+geom_point(alpha=0.4) ## Very Important

## Correlation test
cor.test(train$Item_Outlet_Sales,train$Item_MRP)
cor.test(train$Item_Outlet_Sales,train$YOP)


## Linear Modeling
FullModel<-lm(data = train[,-1],Item_Outlet_Sales~.)
summary(FullModel)

step(FullModel)

## Only relevant variables Linear Regression
model2<-lm(data = train[,-1],Item_Outlet_Sales~Item_MRP+Outlet_Type)
summary(model2)
cor.test(model2$fitted.values,train$Item_Outlet_Sales)
plot(model2$fitted.values,train$Item_Outlet_Sales)
plot(model2$fitted.values,model2$residuals)

## scalled Linear Regression
trainScaled<-train
trainScaled$Item_MRP<-scale(trainScaled$Item_MRP)
scaled.lm<-lm(data = trainScaled[,-1],Item_Outlet_Sales~Item_MRP+Outlet_Type)
summary(scaled.lm)
cor.test(scaled.lm$fitted.values,train$Item_Outlet_Sales)

## Generalized Linear Regression
glm.model<-glm(data = train[,-1],Item_Outlet_Sales~Item_MRP+Outlet_Type)
summary(glm.model)
cor.test(glm.model$fitted.values,train$Item_Outlet_Sales)


## scalled Linear Regression Log
log.lm<-lm(data = train[,-1],log(Item_Outlet_Sales)~log(Item_MRP)+Outlet_Type)
summary(log.lm)
cor.test(exp(log.lm$fitted.values),train$Item_Outlet_Sales)
plot(log.lm$fitted.values,train$Item_Outlet_Sales)

## Cart
cartModel<-rpart(data = train[,-1],Item_Outlet_Sales~Item_MRP+Outlet_Type)
summary(cartModel)
plot(cartModel)
text(cartModel)
cor.test(predict(cartModel),train$Item_Outlet_Sales)

## Random Forest
rf<-randomForest(data = train,Item_Outlet_Sales~Item_MRP+Outlet_Identifier)
summary(rf)
cor.test(rf$predicted,train$Item_Outlet_Sales)
plot(rf$predicted,train$Item_Outlet_Sales)


## Tunning RandomForest

x<-train[,c(6,11)]
y<-train[,12]
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 222
metric <- "RMSE"
set.seed(seed)
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(Item_Outlet_Sales~Item_MRP+Outlet_Type, data=train[,-1], method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)
plot(rf_default)
rf_default$finalModel



## Random Forest 2nd attempt
rf2<-randomForest(data = train,Item_Outlet_Sales~Item_MRP+Outlet_Type,ntree=1000)
summary(rf2)
cor.test(rf2$predicted,train$Item_Outlet_Sales)
plot(rf2$predicted,train$Item_Outlet_Sales)



## GAM
gamModel<-gam(data = train[,-1],Item_Outlet_Sales~Item_MRP)
summary(gamModel)
cor.test(gamModel$fitted.values,train$Item_Outlet_Sales)

plot(rf$predicted,train$Item_Outlet_Sales)


### NNet
colSums(is.na(train))
str(train)
ntrain<-train[,-1]
ntrain$Item_Weight<-(ntrain$Item_Weight-min(ntrain$Item_Weight))/(max(ntrain$Item_Weight)-min(ntrain$Item_Weight))
ntrain$Item_Visibility<-(ntrain$Item_Visibility-min(ntrain$Item_Visibility))/(max(ntrain$Item_Visibility)-min(ntrain$Item_Visibility))
ntrain$Item_MRP<-(ntrain$Item_MRP-min(ntrain$Item_MRP))/(max(ntrain$Item_MRP)-min(ntrain$Item_MRP))
ntrain$Item_Weight<-(ntrain$YOP-min(ntrain$YOP))/(max(ntrain$YOP)-min(ntrain$YOP))

nnmodel<-nnet(data=ntrain,Item_Outlet_Sales~Item_MRP+Outlet_Type,size=10,maxit=100000,decay=0.008,linout=TRUE)
nnmodel$fitted.values
plot(nnmodel$fitted.values,train$Item_Outlet_Sales)
cor.test(nnmodel$fitted.values,train$Item_Outlet_Sales)
### Testing data-
test<-read.csv('test.csv')
dim(test)
colSums(is.na(test))
predicted<-predict(model2,test)
summary(predicted)
submission<-test[,c(1,7)]
submission$Item_Outlet_Sales<-predicted
head(submission)
write.csv(submission,"SwapnilSubmission.csv")




