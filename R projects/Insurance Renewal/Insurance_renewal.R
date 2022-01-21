#libraries 
library(dplyr)
library(caret)
library(ggplot2)
library(tidyr)
library(mltools)
library(data.table)
library(stats)
library(tidytable)
library(FSinR)
library(FunChisq)
library(doMC)
registerDoMC(cores=4)

#Reading Data

data=read.csv("D:/NCI/DM Project/Insurance/insurance_renewal.csv")

is.data.frame(data)

head(data)

summary(data)

str(data)

dim(data)

#Drop ID Feature from the dataset because it is not contributing any thing in prediction.
data1=select(data,-Customer)
head(data1)

#Dropping the 'Effective To Date' feature since we have 'Months Since Policy Inception'
data1=select(data1,-Effective.To.Date)
head(data1)

#Labeling the Target Variable (1/0)
table(data1$Response)
data1$target=ifelse(data$Response=='Yes',1,0)

ggplot(data1,mapping = aes(x=target,fill=off))+geom_bar(fill='steelblue')


#Drop churn from data1 , retain only target
data1=select(data1,-Response)
head(data1)

#Baseline Renewel Rate
mean(data1$target)

#Split data into categorical and Numerical data
num=data1 %>% select(where(is.numeric))
char=data1 %>% select(where(is.character))
str(num)




str(char)

unique_level<-function(x){
   return(length(unique(x)))
}

u=data.frame(sapply(num, unique_level))
u=u%>%rename(feature_level=sapply.num..unique_level.)
u
#from above observation we said that 'Number.of.Open.Complaints' and 'Number.of.Policies' should be categorical because it's have less unique value which is less than 10.
cat=num%>%select(c(Number.of.Open.Complaints,Number.of.Policies))
num=num%>%select(-c(Number.of.Open.Complaints,Number.of.Policies))

str(num)

#converting cat columns i.e Number.of.Open.Complaints and Number.of.Policies into categorical
str(cat)
cat<-lapply(cat, as.character)
str(cat)

#concating cat and char data frame
char=cbind(char,cat)
str(char)

#Missing Value analysis
num<-num %>% replace(.=="NULL", NA) #R work with only Na value.
colSums(is.na(num))
# Since the data does not contain any missing values imputation processes are not required.

#Outlier Analysis of numerical features
sapply(num[,c(1,2,3,4,5,6)], sd) # standard deviation before removing outlier

Outlier<-function(x){
  lower_bound <- quantile(x, 0.01) # I am taking this value because our variance should not show any much variance before and after removing outliers.
  upper_bound <- quantile(x, 0.99)
  outlier_ind <-which(x < lower_bound | x > upper_bound)
  return(outlier_ind)
}

num1<-num[,c(1,2,3,5,6)] #outlier analysis of numerical features.
for(x in names(num1)){
  num<-num[-Outlier(num[,x]),]
}

#Feature Selection - Numerical Features

#Remove features with 0 variance:
sapply(num, sd) #there is no variable having 0 variance

#Bi-Variate Analysis
#Checking if the features show a slope at all
#A strong slope is indicative of the features' ability to discriminate the event from non event
#making it a good predictor

num$Customer.Lifetime.Value_rank<- cut(rank(num$Customer.Lifetime.Value,ties.method ='first'),breaks=10,labels=c(1,2,3,4,5,6,7,8,9,10))
df1<-num%>% group_by(Customer.Lifetime.Value_rank) %>% summarise(target=mean(target))
head(df1)
ggplot(df1,mapping = aes(x=Customer.Lifetime.Value_rank,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")


num$Income_rank<- cut(rank(num$Income,ties.method ='first'),breaks=10,labels=c(1,2,3,4,5,6,7,8,9,10))
df2<-num%>% group_by(Income_rank) %>% summarise(target=mean(target))
head(df2)
ggplot(df2,mapping = aes(x=Income_rank,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill='brown')


num$Monthly.Premium.Auto_rank<- cut(rank(num$Monthly.Premium.Auto,ties.method ='first'),breaks=10,labels=c(1,2,3,4,5,6,7,8,9,10))
df3<-num%>% group_by(Monthly.Premium.Auto_rank) %>% summarise(target=mean(target))
head(df3)
ggplot(df3,mapping = aes(x=Monthly.Premium.Auto_rank,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill='darkgreen')

num$Months.Since.Last.Claim_rank<- cut(rank(num$Months.Since.Last.Claim,ties.method ='first'),breaks=10,labels=c(1,2,3,4,5,6,7,8,9,10))
df4<-num%>% group_by(Months.Since.Last.Claim_rank) %>% summarise(target=mean(target))
head(df4)
ggplot(df4,mapping = aes(x=Months.Since.Last.Claim_rank,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")


num$Months.Since.Policy.Inception_rank<- cut(rank(num$Months.Since.Policy.Inception,ties.method ='first'),breaks=10,labels=c(1,2,3,4,5,6,7,8,9,10))
df5<-num%>% group_by(Months.Since.Policy.Inception_rank) %>% summarise(target=mean(target))
head(df5)
ggplot(df5,mapping = aes(x=Months.Since.Policy.Inception_rank,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill='brown')


num$Total.Claim.Amount_rank<- cut(rank(num$Total.Claim.Amount,ties.method ='first'),breaks=10,labels=c(1,2,3,4,5,6,7,8,9,10))
df6<-num%>% group_by(Total.Claim.Amount_rank) %>% summarise(target=mean(target))
head(df6)
ggplot(df6,mapping = aes(x=Total.Claim.Amount_rank,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill='darkgreen')

#Select k-best features
direct_search_method <- directSearchAlgorithm('selectKBest', list(k=4))
selected=featureSelection(num,'target',direct_search_method,filterEvaluator('chiSquared'))
selected$bestFeatures

#choosing columns having value is 1 and dropping columns having value is 0 from k-best features.
Y=as.data.frame(selected$bestFeatures)
Y=select(Y,c(Customer.Lifetime.Value,Income,Monthly.Premium.Auto,Total.Claim.Amount))
selected_num=num[,colnames(Y)]
dim(selected_num)
head(selected_num)

#Feature selection - Categorical features
str(char)

#Bi Variate Analysis

d2<-data1%>% group_by(State) %>% summarise(target=mean(target))
head(d2)
ggplot(d2,mapping = aes(x=State,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")

d3<-data1%>% group_by(Coverage) %>% summarise(target=mean(target))
head(d3)
ggplot(d3,mapping = aes(x=Coverage,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")

d4<-data1%>% group_by(Education) %>% summarise(target=mean(target))
head(d4)
ggplot(d4,mapping = aes(x=Education,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")

d5<-data1%>% group_by(EmploymentStatus) %>% summarise(target=mean(target))
head(d5)
ggplot(d5,mapping = aes(x=EmploymentStatus,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")

d6<-data1%>% group_by(Gender) %>% summarise(target=mean(target))
head(d6)
ggplot(d6,mapping = aes(x=Gender,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")

d7<-data1%>% group_by(Location.Code) %>% summarise(target=mean(target))
head(d7)
ggplot(d7,mapping = aes(x=Location.Code,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")

d8<-data1%>% group_by(Marital.Status) %>% summarise(target=mean(target))
head(d8)
ggplot(d8,mapping = aes(x=Marital.Status,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")

d9<-data1%>% group_by(Policy.Type) %>% summarise(target=mean(target))
head(d9)
ggplot(d9,mapping = aes(x=Policy.Type,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")

d10<-data1%>% group_by(Renew.Offer.Type) %>% summarise(target=mean(target))
head(d10)
ggplot(d10,mapping = aes(x=Renew.Offer.Type,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")

d11<-data1%>% group_by(Sales.Channel) %>% summarise(target=mean(target))
head(d11)
ggplot(d11,mapping = aes(x=Sales.Channel,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")

d12<-data1%>% group_by(Vehicle.Class) %>% summarise(target=mean(target))
head(d12)
ggplot(d12,mapping = aes(x=Vehicle.Class,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")

d13<-data1%>% group_by(Vehicle.Size) %>% summarise(target=mean(target))
head(d13)
ggplot(d13,mapping = aes(x=Vehicle.Size,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")

d14<-data1%>% group_by(Number.of.Open.Complaints) %>% summarise(target=mean(target))
head(d14)
ggplot(d14,mapping = aes(x=Number.of.Open.Complaints,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")

d15<-data1%>% group_by(Number.of.Policies) %>% summarise(target=mean(target))
head(d15)
ggplot(d15,mapping = aes(x=Number.of.Policies,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")


#from above visualization we can say that 'State','Coverage','Education','Gender','Policy Type','Number of Policies' are not good predictor.
char=select(char,-c(State,Coverage,Education,Gender,Policy.Type,Number.of.Policies))
head(char)

# Creating dummy features with using One hot encoding.
dummy <- dummyVars(" ~ .", data=char)
char_dum <- data.frame(predict(dummy, newdata = char))
head(char_dum)
dim(char_dum)
Y=as.data.frame((cbind(char_dum,data1[['target']])))

#Select k-best features
direct_search_method <- directSearchAlgorithm('selectKBest', list(k=24))
selected1=featureSelection(Y,'data1[["target"]]',direct_search_method,filterEvaluator('chiSquared'))
selected1$bestFeatures

#choosing columns having value is 1 and dropping columns having value is 0 from k-best features.
Y=as.data.frame(selected1$bestFeatures)
Y=select(Y,-c(EmploymentStatusDisabled,Vehicle.ClassFour.Door.Car,Vehicle.ClassLuxury.SUV,Vehicle.ClassTwo.Door.Car,Vehicle.SizeMedsize,Number.of.Open.Complaints0,Number.of.Open.Complaints1,Number.of.Open.Complaints3,Number.of.Open.Complaints4,Number.of.Open.Complaints5))

selected_char=char_dum[,colnames(Y)]
dim(selected_char)
head(selected_char)

#Creating the Master feature set for model development.
x_all<-cbind(selected_num,selected_char[c(1:nrow(num)),],num['target'])
dim(x_all)
head(x_all)

#Splitting data into train and test
X_train_test_model <- function(data, size = 0.7, train = TRUE) {   #taking 70% train data and 30% test data
  n_row = nrow(x_all)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (x_all[train_sample, ])
  } else {
    return (x_all[-train_sample, ])
  }
}


X_train<-X_train_test_model(x_all,size = 0.7,train = T)
X_test<-X_train_test_model(x_all,size = 0.7,train = F)
dim(X_train)
dim(X_test)

#Using Gradient boosting
library(gbm)

# Simple model with default values
set.seed(123)
gbm_simple<-gbm(target~.,distribution = "bernoulli",data = X_train,n.trees = 1000,interaction.depth = 4,shrinkage = 0.01)
print(gbm_simple)
summary(gbm_simple)

#Best fit model using oob error and cross validation technique
set.seed(123)
gbm_best<-gbm(target~.,distribution = "bernoulli",data = X_train,n.trees = 1500,
                 interaction.depth = 4,shrinkage = 0.01,cv.folds = 10)

par(mar=c(4.5,4.5,1.7,1.8))
par(mfrow=c(2,1))
opt_cv<-gbm.perf(gbm_best,method = "cv")
opt_oob<-gbm.perf(gbm_best,method = "OOB")

print(opt_cv)
print(opt_oob)
#I have used the optimum solution based on the cross validation technique 
#as cross validation usually outperforms the OOB method on large datasets.

# Compute model prediction accuracy rate
pred<-predict(object =gbm_best,newdata = X_test,n.trees = opt_cv,type='response')

pred

# GBM Model performance
#Using Confusion matrix
prediction2<-as.factor(ifelse(pred>0.30,1,0))
cm1<-confusionMatrix(as.factor(X_test$target),prediction2)


par(mar=c(4.5,4.5,1.7,1.8))
par(mfrow=c(2,1))
draw_confusion_matrix(cm1)
draw_details(cm1)

#Using Area Under Curve(AUC)
#Creting ROC Curve
gbm_pred_test<-prediction(as.numeric(prediction2),as.numeric(X_test$target))
gbm_roc_test<-performance(gbm_pred_test,'tpr','fpr')#Generating ROC curve data

#AUC
auc1<-performance(gbm_pred_test,'auc')#Creating AUC data
gbm_auc_test<-as.numeric(auc1@y.values)#Calculating AUC
gbm_auc_test

#Curve
par(mar=c(5,7,3,3))
par(mfrow=c(1,1))
plot(gbm_roc_test)
plot(gbm_roc_test,add=T,col='red')
legend('right', legend=c('GBM',round(gbm_auc_test,2)),col=c('red'),lty=1:2,cex=1)



#Using Xgboosting(Extreme gradient boosting) algorithm
library(xgboost)

#target variable should be numeric
target<-X_train$target
target_ts<-X_test$target
#class(target)
#class(target_ts)
tr<-data.matrix(X_train[,-29])
ts<-data.matrix(X_test[,-29])
#preparing matrix 
train <- xgb.DMatrix(data = tr,label = target) 
test <- xgb.DMatrix(data = ts,label=target_ts)


#default parameters
params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, 
                 max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)

#calculating best nround in this model
xgb_simple <- xgb.cv( params = params, data = train, nrounds = 100, nfold = 10, showsd = T, 
                 stratified = T, print_every_n = 10, early_stop_round = 20, maximize = F)

#best nround is 100, because model return lowest error rate at 100th iteration
#There is no need for gamma value because train_error < test_error

#Best model
xgb_best <- xgb.train (params = params, data = train, nrounds = 100, watchlist = list(val=test,train=train), 
                   print_every_n = 10, early_stop_round = 10, maximize = F , eval_metric = "error")
xgb_best

#view variable importance plot
xgb_imp <- xgb.importance (feature_names = colnames(tr),model = xgb_best)
xgb_imp
xgb.plot.importance (importance_matrix = xgb_imp[1:20]) 


#model prediction
xgbpred <- predict (xgb_best,test)
xgbpred <- ifelse (xgbpred > 0.5,1,0)
xgbpred


#Model Evaluation
#Using confusion Matrix

cm2<-confusionMatrix (as.factor(xgbpred), as.factor(target_ts))

par(mar=c(4.5,4.5,1.7,1.8))
par(mfrow=c(2,1))
draw_confusion_matrix(cm2)
draw_details(cm2)

#Using Area Under Curve(AUC)
#Creting ROC Curve
xgb_pred_test<-prediction(xgbpred,target_ts)
xgb_roc_test<-performance(xgb_pred_test,'tpr','fpr')#Generating ROC curve data

#AUC
auc2<-performance(xgb_pred_test,'auc')#Creating AUC data
xgb_auc_test<-as.numeric(auc2@y.values)#Calculating AUC
xgb_auc_test

#Curve
par(mar=c(5,7,3,3))
par(mfrow=c(1,1))
plot(xgb_roc_test)
plot(xgb_roc_test,add=T,col='red')
legend('right', legend=c('XGB',round(xgb_auc_test,2)),col=c('red'),lty=1:2,cex=1)

#Comparison between gradient boosting and extreme gradient boosting using AUC

par(mar=c(5,7,3,3))
par(mfrow=c(1,1))
plot(gbm_roc_test,col='red')
plot(xgb_roc_test,add=T,col='blue')
legend('right', legend=c('GBM','XGB'),col=c('red','blue'),lty=1:2,cex=1)

