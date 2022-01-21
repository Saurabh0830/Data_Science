#libraries 
library(dplyr)
library(caret)
library(ggplot2)
library(yardstick)
library(tidyr)
library(mltools)
library(data.table)
library(stats)
library(tidytable)
library(FSinR)
library(FunChisq)
library(doMC)
registerDoMC(cores=4)
library(rpart)
library(rpart.plot)
library(ROCR)
#Reading Data

data=read.csv("D:/NCI/DM Project/Telco Churn/Telco_Customer_Churn.csv")

is.data.frame(data)

head(data)

summary(data)

str(data)

dim(data)

#How many rows have missing ID ?
sum(is.null(data$customerID))

#Drop ID Feature from the dataset because it is not contributing any thing in prediction.
data1=select(data,-customerID)
data1

#Label the Churn feature to 1/0
data1$target=ifelse(data$Churn=='Yes',1,0)

#Drop churn from data1 , retain only target
data1=select(data1,-Churn)
data1

#Checking imbalance of data
ggplot(data1,mapping = aes(x=target,fill=off))+geom_bar(fill='steelblue')
table(data1$target)

#Churn Rate
mean(data1$target)

#Split data into categorical and Numerical data
num=data1 %>% select(where(is.numeric))
char=data1 %>% select(where(is.character))

head(num)

##Check whether SeniorCitizon feature is an indicator
table(num$SeniorCitizen)

#Yes it is act as an indicator because it's have only 2 values 0 and 1 which separate data into 2 categories , which is use for prediction.

head(char)

#Dropping the indicator features from num to build a separate DF
ind=select(num,SeniorCitizen)
num=select(num,-SeniorCitizen)
head(num)

#Outlier Analysis of numerical features
sapply(num[,c(1,2,3)], sd) # standard deviation before removing outlier

Outlier<-function(x){
  lower_bound <- quantile(x, 0.01) # I am taking this value because our variance should not show any much variance before and after removing outliers.
  upper_bound <- quantile(x, 0.99)
  outlier_ind <-which(x < lower_bound | x > upper_bound)
  return(outlier_ind)
}

num1<-num[,c(1,2,3)] #outlier analysis of numerical features.
for(x in names(num1)){
  num<-num[-Outlier(num[,x]),]
}

sapply(num[,c(1,2,3)], sd)


#Missing Value analysis
num<-num %>% replace(.=="NULL", NA) #R work with only Na value.
colSums(is.na(num))
# Since the data does not contain any missing values imputation processes are not required.

#Feature Selection - Numerical Features

#Removing features with '0' variance
sapply(num[,c(1,2,3)], var)

#Bi-Variate Analysis
#Checking if the features show a slope at all
#A strong slope is indicate the features' ability to discriminate the event from non event
#making it a good predictor

num$tenure_rank<- cut(rank(num$tenure,ties.method ='first'),breaks=10,labels=c(1,2,3,4,5,6,7,8,9,10))
df1<-num%>% group_by(tenure_rank) %>% summarise(target=mean(target))
head(df1)
ggplot(df1,mapping = aes(x=tenure_rank,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")


num$MonthlyCharges_rank<- cut(rank(num$MonthlyCharges,ties.method ='first'),breaks=10,labels=c(1,2,3,4,5,6,7,8,9,10))
df2<-num%>% group_by(MonthlyCharges_rank) %>% summarise(target=mean(target))
head(df2)
ggplot(df2,mapping = aes(x=MonthlyCharges_rank,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill='brown')


num$TotalCharges_rank<- cut(rank(num$TotalCharges,ties.method ='first'),breaks=10,labels=c(1,2,3,4,5,6,7,8,9,10))
df3<-num%>% group_by(TotalCharges_rank) %>% summarise(target=mean(target))
head(df3)
ggplot(df3,mapping = aes(x=TotalCharges_rank,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill='darkgreen')

num=select(num,-c(tenure,MonthlyCharges,TotalCharges))
head(num)
dim(num)

#Feature selection - Categorical features
str(char)

#Bi Variate Analysis

d1<-data1%>% group_by(gender) %>% summarise(target=mean(target))
head(d1)
ggplot(d1,mapping = aes(x=gender,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")

d2<-data1%>% group_by(Partner) %>% summarise(target=mean(target))
head(d2)
ggplot(d2,mapping = aes(x=Partner,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")

d3<-data1%>% group_by(Dependents) %>% summarise(target=mean(target))
head(d3)
ggplot(d3,mapping = aes(x=Dependents,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")

d4<-data1%>% group_by(PhoneService) %>% summarise(target=mean(target))
head(d4)
ggplot(d4,mapping = aes(x=PhoneService,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")

d5<-data1%>% group_by(MultipleLines) %>% summarise(target=mean(target))
head(d5)
ggplot(d5,mapping = aes(x=MultipleLines,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")

d6<-data1%>% group_by(InternetService) %>% summarise(target=mean(target))
head(d6)
ggplot(d6,mapping = aes(x=InternetService,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")

d7<-data1%>% group_by(OnlineSecurity) %>% summarise(target=mean(target))
head(d7)
ggplot(d7,mapping = aes(x=OnlineSecurity,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")

d8<-data1%>% group_by(OnlineBackup) %>% summarise(target=mean(target))
head(d8)
ggplot(d8,mapping = aes(x=OnlineBackup,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")

d9<-data1%>% group_by(DeviceProtection) %>% summarise(target=mean(target))
head(d9)
ggplot(d9,mapping = aes(x=DeviceProtection,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")

d10<-data1%>% group_by(TechSupport) %>% summarise(target=mean(target))
head(d10)
ggplot(d10,mapping = aes(x=TechSupport,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")

d11<-data1%>% group_by(StreamingTV) %>% summarise(target=mean(target))
head(d11)
ggplot(d11,mapping = aes(x=StreamingTV,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")

d12<-data1%>% group_by(StreamingMovies) %>% summarise(target=mean(target))
head(d12)
ggplot(d12,mapping = aes(x=StreamingMovies,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")

d13<-data1%>% group_by(Contract) %>% summarise(target=mean(target))
head(d13)
ggplot(d13,mapping = aes(x=Contract,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")

d14<-data1%>% group_by(PaperlessBilling) %>% summarise(target=mean(target))
head(d14)
ggplot(d14,mapping = aes(x=PaperlessBilling,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")

d15<-data1%>% group_by(PaymentMethod) %>% summarise(target=mean(target))
head(d15)
ggplot(d15,mapping = aes(x=PaymentMethod,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")


#from above visualization we can say that 'gender','PhoneService','MultipleLines' are not good predictor.
char=select(char,-c(gender,PhoneService,MultipleLines))
head(char)

# Creating dummy features with n-1 levels.
dummy <- dummyVars(" ~ .", data=char)
char_dum <- data.frame(predict(dummy, newdata = char))
head(char_dum)
dim(char_dum)
Y=as.data.frame((cbind(char_dum,data1[['target']])))

#Select k-best features
direct_search_method <- directSearchAlgorithm('selectKBest', list(k=20))
selected=featureSelection(Y,'data1[["target"]]',direct_search_method,filterEvaluator('chiSquared'))
selected$bestFeatures

#choosing columns having value is 1 and dropping columns having value is 0 from k-best features.
Y=as.data.frame(selected$bestFeatures)
Y=select(Y,-c(PartnerNo,PartnerYes,DependentsNo,DependentsYes,InternetServiceDSL,OnlineBackupYes,DeviceProtectionYes,StreamingTVNo,StreamingTVYes,StreamingMoviesNo,StreamingMoviesYes,PaymentMethodBank.transfer..automatic.,PaymentMethodCredit.card..automatic.,PaymentMethodMailed.check))
selected_char=char_dum[,colnames(Y)]
dim(selected_char)
head(selected_char)

#Visualization of numerical indicating features
d16<-data1%>% group_by(SeniorCitizen) %>% summarise(target=mean(target))
head(d16)
ggplot(d16,mapping = aes(x=SeniorCitizen,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill="red")
SeniorCitizen=ind[c(1:nrow(num)),]

#Main features for prediction:
x_all<-cbind(num,selected_char[c(1:nrow(num)),],SeniorCitizen)
dim(x_all)
head(x_all)

x_all$target <- factor(x_all$target)

#Splitting data into train and test
x_train_test_model <- function(data, size = 0.7, train = TRUE) {   #taking 70% train data and 30% test data
  n_row = nrow(x_all)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (x_all[train_sample, ])
  } else {
    return (x_all[-train_sample, ])
  }
}


x_train2<-x_train_test_model(x_all,size = 0.7,train = T)
x_test2<-x_train_test_model(x_all,size = 0.7,train = F)

dim(x_train2)
dim(x_test2)

#Using Decision tree
control=rpart.control(cp=-1,maxdepth =6,minsplit=50)
dtree<- rpart(target~., data=x_train2, method="class",control =control)
rpart.plot(dtree)
summary(dtree)

#Tree pruning
printcp(dtree)  
plotcp(dtree)

dtree2<-prune(dtree,cp=0.0015798)
rpart.plot(dtree2)


#Compute model prediction accuracy rate
prediction1<-predict(dtree2,x_test2, type = 'class')
prediction1

#Model performance of Decision tree
#using confusion matrix

draw_confusion_matrix<-function(cm){
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 445, 'Class1', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 445, 'Class2', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Class1', cex=1.2, srt=90)
  text(140, 335, 'Class2', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
}

draw_details <- function(cm) {
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 88, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 65, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 88, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 65, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 88, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 65, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 88, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 65, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 88, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 65, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 40, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 15, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 40, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 15, round(as.numeric(cm$overall[2]), 3), cex=1.4)
} 

cm1<-confusionMatrix(as.factor(x_test2$target),as.factor(prediction1))


par(mar=c(4.5,4.5,1.7,1.8))
par(mfrow=c(2,1))
draw_confusion_matrix(cm1)
draw_details(cm1)

#Using Area Under Curve(AUC)
#Creting ROC Curve
dtree_pred_test<-prediction(as.numeric(prediction1),as.numeric(x_test2$target))
dtree_roc_test<-performance(dtree_pred_test,'tpr','fpr')#Generating ROC curve data

#AUC
auc1<-performance(dtree_pred_test,'auc')#Creating AUC data
dtree_auc_test<-as.numeric(auc1@y.values)#Calculating AUC
dtree_auc_test #AUC score

#Curve
par(mar=c(5,7,3,3))
par(mfrow=c(1,1))
plot(dtree_roc_test)
plot(dtree_roc_test,add=T,col='red')
legend('right', legend=c('dtree',round(dtree_auc_test,2)),col=c('red'),lty=1:2,cex=1)



#Using Randomforest
library(randomForest)

#Simple model with default ntree=500
set.seed(7)
rf_simple<-randomForest(target~.,data=x_train2,ntree=500)
rf_simple

#checking best ntree using cross validation.
for (ntree in c(500,1000,1500,2000,2500)) {
  set.seed(7)
  rf<-randomForest(target~.,data=x_train2,ntree=ntree,mtry=c(sqrt(ncol(x_train2[,-1]))))
  print(rf)
}

#Here we are choosing ntree=500 because oob error of 500 is less than other ones.
#Now we are checking best mtry using OOB error , while ntree=500
set.seed(7)
bestmtry <- tuneRF(x_train2[,-1], x_train2[,1], stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)

#From above we can say that at ntree=500 and mtry=6 we will get best result.
set.seed(7)
rf_best<-randomForest(target~.,data=x_train2,mtry=6,ntree=500)
rf_best

#Important variables 

importance(rf_best)
imp = as.data.frame(importance(rf_best))
imp = cbind(vars=rownames(imp), imp)
imp = imp[order(imp$MeanDecreaseGini),]
imp$vars = factor(imp$vars, levels=unique(imp$vars))

par(las=2)
par(mar=c(2.5,14,0.1,2))
barplot(imp$MeanDecreaseGini, names.arg=imp$vars,horiz=TRUE)

varImpPlot(rf_best)

#Compute model prediction accuracy rate
pred<-predict(rf_best,newdata = x_test2)
pred

#Model performance of Randomforest
#Using Confusion matrix
cm2<-confusionMatrix(x_test2$target,pred)


par(mar=c(4.5,4.5,1.7,1.8))
par(mfrow=c(2,1))
draw_confusion_matrix(cm2)
draw_details(cm2)

#Using Area Under Curve(AUC)
#Creating ROC Curve
rf_pred_test<-prediction(as.numeric(pred),as.numeric(x_test2$target))
rf_roc_test<-performance(gbm_pred_test,'tpr','fpr')#Generating ROC curve data

#AUC
auc2<-performance(rf_pred_test,'auc')#Creating AUC data
rf_auc_test<-as.numeric(auc2@y.values)#Calculating AUC
rf_auc_test

#Curve
par(mar=c(5,7,3,3))
par(mfrow=c(1,1))
plot(rf_roc_test)
plot(rf_roc_test,add=T,col='red')
legend('right', legend=c('RF',round(rf_auc_test,2)),col=c('red'),lty=1:2,cex=1)


#Comparison between decision tree and random forest using AUC

par(mar=c(5,7,3,3))
par(mfrow=c(1,1))
plot(dtree_roc_test,col='red')
plot(rf_roc_test,add=T,col='blue')
legend('right', legend=c('dtree','RF'),col=c('red','blue'),lty=1:2,cex=1)
