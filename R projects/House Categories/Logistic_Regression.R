#importing library 
library(ggplot2)
library(dplyr)
library(reshape2)
library(caret)
library(car)
library(foreign)
library(broom)
library(Hmisc)
library(DescTools)
library(caret)
library(e1071)
library(tidyverse)

#Reading  data from file

data=read.spss("C:/Users/sbtha/Desktop/Time Series and Logistic regression/House Categories.sav",to.data.frame = T)
is.data.frame(data)

head(data)
dim(data)
summary(data)

str(data)


# check for  missing values
data<-data %>% replace(.=="NULL", NA) #R work with only Na value.
colSums(is.na(data))

#Histogram of numerical dataset
hist(data[,c(1,2,3,4,5)])

#checking correlation between independent variable
data2<-data[,-c(10,11,12,13)]
data3<-cor(data2)
data4<-melt(data3)

ggplot(data4, aes(x = Var1, y = Var2, fill = value))+
  geom_tile()+ggtitle("Correlation")+
  geom_text(aes(label = round(value, 2)),color = "white")

#Outliers detection and visualization

#Boxplot
par(mar=c(5,4,1,3))
boxplot(data[,-c(10,11,12,13)])

#Ordinal encoding
str(data)
data$fuel<-ifelse(data$fuel=='oil     ',1,ifelse(data$fuel=='gas     ',2,3))
data$waterfront<-ifelse(data$waterfront=='No ',0,1)
data$newConstruction<-ifelse(data$newConstruction=='Yes',1,0)
str(data)

levels(data$PriceCat)<-c("0","1")



#Training and testing data
set.seed(123)
training.samples <- data$PriceCat%>%createDataPartition(p = 0.7, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]

#===============================================================================

#Models Creation using backard stepwise elimination methods
model1<-glm(PriceCat~lotSize+age+landValue+livingArea+pctCollege+bedrooms+fireplaces+bathrooms+rooms+fuel+waterfront+newConstruction,data=train.data,family = binomial)
summary(model1)
plot(cooks.distance(model1))
influenceIndexPlot(model1)

#Examine predicted probabilities and classifications
pred_probs<-predict(model1, type="response")
pred_class<-as.factor(ifelse(pred_probs>0.5,1,0))
str(train.data$PriceCat)

#Use Table function to create confusion matrix
table(train.data$PriceCat,pred_class)

#confusionMatrix function in caret package
confusionMatrix(train.data$PriceCat,pred_class)


#PseudoRSquared Statistics
PseudoR2(model1, which = "all")
vif(model1)
cooks.distance(model3)[abs(cooks.distance(model3))>1]
#===============================================================================

#model2 without outliers
unique_level<-function(x){
  return(length(unique(x)))
}

u=data.frame(sapply(data, unique_level))
u=u%>%rename(Unique_values_count=sapply.data..unique_level.)
u

Outlier<-function(x){
  lower_bound <- quantile(x, 0.01) # I am taking this value because our variance should not show any much variance before and after removing outliers.
  upper_bound <- quantile(x, 0.99)
  outlier_ind <-which(x < lower_bound | x > upper_bound)
  return(outlier_ind)
}

data1<-data[,c(1,2,3,4,5)] #outlier analysis of numerical features.
for(x in names(data1)){
  data<-data[-Outlier(data[,x]),]
}
dim(data)
set.seed(123)
training.samples <- data$PriceCat%>%createDataPartition(p = 0.7, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]

model2<-glm(PriceCat~lotSize+age+landValue+livingArea+pctCollege+bedrooms+fireplaces+bathrooms+rooms+fuel+waterfront+newConstruction,data=train.data,family = binomial)
summary(model2)
plot(cooks.distance(model2))
influencePlot(model2)

#Examine predicted probabilities and classifications
pred_probs<-predict(model2, type="response")
pred_class<-as.factor(ifelse(pred_probs>0.5,1,0))
str(train.data$PriceCat)

#Use Table function to create confusion matrix
table(train.data$PriceCat,pred_class)

#confusionMatrix function in caret package
confusionMatrix(train.data$PriceCat,pred_class)


#PseudoRSquared Statistics
PseudoR2(model2, which = "all")
#==================================================================================

#model3
model3<-glm(PriceCat~lotSize+landValue+livingArea+bathrooms,data=train.data,family = binomial)
summary(model3)

#Examine predicted probabilities and classifications
pred_probs<-predict(model3, type="response")
pred_class<-as.factor(ifelse(pred_probs>0.5,1,0))
str(train.data$PriceCat)

#Use Table function to create confusion matrix
table(train.data$PriceCat,pred_class)

#confusionMatrix function in caret package
confusionMatrix(train.data$PriceCat,pred_class)


#PseudoRSquared Statistics
PseudoR2(model3, which = "all")

#===============================================================================

#model3-pctCollege
model3<-glm(PriceCat~lotSize+landValue+livingArea+bathrooms,data=train.data,family = binomial)
summary(model3)
cooks.distance(model3)[abs(cooks.distance(model3))>1]
coef(model3)
vif(model3)

#Check Linearity of the Logit
LoLTestModel<-glm(PriceCat~landValue+landValue:log(landValue)+livingArea+livingArea:log(livingArea)+bathrooms,data = train.data, family=binomial)
summary(LoLTestModel)

#Examine predicted probabilities and classifications
pred_probs<-predict(model3, type="response")
pred_class<-as.factor(ifelse(pred_probs>0.5,1,0))
str(train.data$PriceCat)

#Use Table function to create confusion matrix
table(train.data$PriceCat,pred_class)

#confusionMatrix function in caret package
confusionMatrix(train.data$PriceCat,pred_class)


#PseudoRSquared Statistics
PseudoR2(model3, which = "CoxSnell")

library("ResourceSelection")
hoslem.test(model3$residuals, fitted(model3), g=3)
#===============================================================================

#model5-lotSize
model5<-glm(PriceCat~landValue+livingArea+bathrooms,data=train.data,family = binomial)
summary(model5)

#Examine predicted probabilities and classifications
pred_probs<-predict(model5, type="response")
pred_class<-as.factor(ifelse(pred_probs>0.5,1,0))
str(train.data$PriceCat)

#Use Table function to create confusion matrix
table(train.data$PriceCat,pred_class)

#confusionMatrix function in caret package
confusionMatrix(train.data$PriceCat,pred_class)


#PseudoRSquared Statistics
PseudoR2(model5, which = "all")


#Interpretation of model
#model4 is the best model
#Check for Multicollinearity using variable inflation factor
vif(model4)

#Check Linearity of the Logit
LoLTestModel<-glm(PriceCat~lotSize+lotSize:log(lotSize)+landValue+landValue:log(landValue)+livingArea+livingArea:log(livingArea)+bathrooms,data = train.data, family=binomial)
summary(LoLTestModel)
#Note: The Age:log(Age) and Price:log(Price) are not significant 
#so the predictors Age and Price are linear with the logit

#Checking the standardised residuals and identifying Influential data points
rstandard(model3)[abs(rstandard(model3))>1.5]
influencePlot(model1)
outlierTest(model3)

par(mar=c(4.5,4.5,1.7,1.8))
par(mfrow=c(2,2)) 
plot(model4)

