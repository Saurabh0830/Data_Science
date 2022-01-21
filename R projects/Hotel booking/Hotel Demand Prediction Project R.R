#libraries 
library(dplyr)
#install.packages('tidyverse',dependencies = T)
library(caret)
library(ggplot2)
library(tidyr)
library(mltools)
library(data.table)
library(FSinR)
library(FunChisq)
library(doMC)
registerDoMC(cores=4)


#Reading  data from file

data=read.csv("D:/NCI/DM Project/Hotel Booking/hotel_bookings.csv")

is.data.frame(data)

head(data)

summary(data)

str(data)

dim(data)

table(data$is_canceled)

#Selecting target variable
data$target=ifelse(data$is_canceled=='1',1,0)
summary(data)
table(data$target)
#Visualization to check data is balanced or not
ggplot(data,mapping = aes(x=target,fill=off))+geom_bar(width=0.1,fill='steelblue')

#Selecting total data without target variable
#data1=select(data,-is_canceled)
#data1
#summary(data1)

# check for  missing values
data<-data %>% replace(.=="NULL", NA) #R work with only Na value.
colSums(is.na(data))

# Replace missing values:
# agent: If no agency is given, booking was most likely made without one.
# company: If none given, it was most likely private.
data$country<-data$country %>% replace_na("unknown")
data$agent<-data$agent %>% replace_na(0)
data$company<-data$company %>% replace_na(0)
colSums(is.na(data))

# "meal" contains values "Undefined", which is replacing with often use meal.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#getmode(data$meal)
data$meal<-data$meal %>% replace(.=="Undefined",getmode(data$meal))

#variance checking
var1=data %>% summarise_if(is.numeric,var)
var1 # Here is_repeated_guest,previous_cancellations,previous_bookings_not_canceled and required_car_parking_spaces have 0 variance, so they are not contributing for any prediction.


#It doesn't matter which type of hotel they make a reservation, the main objective 
#is to see if they make ANY type of reservation at all or not agent - The agent that 
#got the reservation for us won't matter 
#company - Same logic goes for company as for the agent 
#reservation_status_date - We have other features (like: arrival_date_week_number,arrival_date_day_of_month etc) that gives us the same information.

#Checking relation between independent and dependent variable using visualization methods.
df1<-data %>% group_by(meal) %>% summarise(target=mean(target))
df1
ggplot(df1,mapping = aes(x=meal,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill='red')

df2<-data %>% group_by(is_repeated_guest) %>% summarise(target=mean(target))
df2
ggplot(df2,mapping = aes(x=is_repeated_guest,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill='green')

df3<-data %>% group_by(market_segment) %>% summarise(target=mean(target))
df3
ggplot(df3,mapping = aes(x=market_segment,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill='blue')

df4<-data %>% group_by(arrival_date_month) %>% summarise(target=mean(target))
df4
ggplot(df4,mapping = aes(x=arrival_date_month,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill='skyblue')

df5<-data %>% group_by(stays_in_weekend_nights) %>% summarise(target=mean(target))
df5
ggplot(df5,mapping = aes(x=stays_in_weekend_nights,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.8,fill='lightgreen')

df6<-data %>% group_by(stays_in_week_nights) %>% summarise(target=mean(target))
df6
ggplot(df6,mapping = aes(x=stays_in_week_nights,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill='darkgreen')

#Coverting 
#'stays in week nights' have continuous variable , so I am binning in a some range to find it's proper visualization.
data$stays_in_week_nights_rank<- cut(rank(data$stays_in_week_nights,ties.method ='first'),breaks=5,labels=c(1,2,3,4,5))
df7<-data %>% group_by(stays_in_week_nights_rank) %>% summarise(target=mean(target))
ggplot(df7,mapping = aes(x=stays_in_week_nights_rank,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill='darkgreen')

data %>% group_by(stays_in_week_nights_rank) %>% summarise(stays_in_week_nights=min(stays_in_week_nights))

data$stay_in_week_night_grp<-ifelse((data$stays_in_week_nights_rank)=='1'| (data$stays_in_week_nights_rank)=='2' ,1,ifelse((data$stays_in_week_nights_rank)=='3'| (data$stays_in_week_nights_rank)=='4',2,3))
data %>% group_by(stay_in_week_night_grp) %>% summarise(target=mean(target))

data$prev_cancel_ind<-ifelse((data$previous_cancellations)==0,0,1)
data %>% group_by(prev_cancel_ind) %>% summarise(target=mean(target))
table(data$previous_cancellations)

df8<-data %>% group_by(booking_changes) %>% summarise(target=mean(target))
df8
ggplot(df8,mapping = aes(x=booking_changes,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill='gray')

df9<-data %>% group_by(distribution_channel) %>% summarise(target=mean(target))
df9
ggplot(df9,mapping = aes(x=distribution_channel,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill='pink')


df10<-data %>% group_by(reserved_room_type) %>% summarise(target=mean(target))
df10
ggplot(df10,mapping = aes(x=reserved_room_type,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill='red')

df11<-data %>% group_by(deposit_type) %>% summarise(target=mean(target))
df11
ggplot(df11,mapping = aes(x=deposit_type,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill='yellow')

df12<-data %>% group_by(customer_type) %>% summarise(target=mean(target))
df12
ggplot(df12,mapping = aes(x=customer_type,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill='brown')

df13<-data %>% group_by(total_of_special_requests) %>% summarise(target=mean(target))
df13
ggplot(df13,mapping = aes(x=total_of_special_requests,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill='blue')


data$day_wait_rank<- cut(rank(data$days_in_waiting_list,ties.method ='first'),breaks=5,labels=c(1,2,3,4,5))
df14<-data %>% group_by(day_wait_rank) %>% summarise(target=mean(target))
ggplot(df14,mapping = aes(x=day_wait_rank,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill='darkgreen')

data$day_wait_ind<-ifelse((data$day_wait_rank)=='3',1,0)
data %>% group_by(day_wait_ind) %>% summarise(target=length(target))

data$adr_rank<- cut(rank(data$adr,ties.method ='first'),breaks=10,labels=c(1,2,3,4,5,6,7,8,9,10))
df15<-data %>% group_by(adr_rank) %>% summarise(target=mean(target))
ggplot(df15,mapping = aes(x=adr_rank,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill='darkgreen')


data$lead_time_rank<- cut(rank(data$lead_time,ties.method ='first'),breaks=10,labels=c(1,2,3,4,5,6,7,8,9,10))
df17<-data %>% group_by(lead_time_rank) %>% summarise(target=mean(target))
ggplot(df17,mapping = aes(x=lead_time_rank,y=target,fill=off))+geom_bar(position="dodge", stat="identity",width=0.5,fill='brown')

df18<-data %>% group_by(lead_time_rank) %>% summarise(lead_time=min(lead_time))
df18

#Final features use for predictions.
str(data)
col_num=c('lead_time','adr')
col_char=c('day_wait_ind','total_of_special_requests','customer_type','reserved_room_type',
          'distribution_channel','market_segment','stay_in_week_night_grp')

#Applying One hot encoding on categorical variable 
dmy <- dummyVars(" ~ .", data = select(data,col_char))
X_char_dum <- data.frame(predict(dmy, newdata =select(data,col_char) ))
X_char_dum[['target']]=data[['target']]
dim(X_char_dum)

#Select k-best features
direct_search_method <- directSearchAlgorithm('selectKBest', list(k=16))
selected=featureSelection(X_char_dum,'target',direct_search_method,filterEvaluator('chiSquared'))
selected$bestFeatures

#choosing columns having value is 1 and dropping columns having value is 0 from k-best features.
Y=as.data.frame(selected$bestFeatures)
Y=select(Y,-c(day_wait_ind,customer_typeGroup,reserved_room_typeC,reserved_room_typeD,reserved_room_typeE,reserved_room_typeF,reserved_room_typeL,reserved_room_typeP,market_segmentComplementary,market_segmentGroups))
selected_char=X_char_dum[,colnames(Y)]
dim(selected_char)
head(selected_char)

x_all<-cbind(select(data,col_num),selected_char,select(data,target))
dim(x_all)
str(x_all)

x_all$target[x_all$target == 0] <- 'No'
x_all$target[x_all$target == 1] <- 'Yes'
x_all$target <- factor(x_all$target)


#Creating train and test data set

x_train_test_model <- function(data, size = 0.7, train = TRUE) {   #taking 70% for training data and 30% for testing data
  n_row = nrow(x_all)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (x_all[train_sample, ])
  } else {
    return (x_all[-train_sample, ])
  }
}


x_train1<-x_train_test_model(x_all,size = 0.7,train = T)
x_test1<-x_train_test_model(x_all,size = 0.7,train = F)

#checking dimension of train and test data set
dim(x_train1)
dim(x_test1)


#using knn

trControl <- trainControl(method = "cv",number = 10,classProbs =T,
                          summaryFunction = twoClassSummary)
set.seed(123)
knn_best <- train(target ~ .,data =x_train1,method = 'knn',trControl = trControl,
             preProc = c("center", "scale"),metric='ROC',tuneGrid = expand.grid(k = 1:round(sqrt(NROW(x_train1)))))

knn_best

#knn_best model plot
plot(knn_best)

#Best tune parameter
knn_best$bestTune

#Influence of variables on Model
varImp(knn_best)
plot(varImp(knn_best))

#Compute model prediction accuracy rate
prediction1<- predict(knn_best,newdata = x_test1,k=6)

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


cm1<-confusionMatrix(prediction1, x_test1$target)
par(mar=c(4.5,4.5,1.7,1.8))
par(mfrow=c(2,1))
draw_confusion_matrix(cm1)
draw_details(cm1)

#Using Area Under Curve(AUC)
#Creating ROC 
knn_pred_test<-prediction(as.numeric(prediction1),as.numeric(x_test1$target))
knn_roc_test<-performance(knn_pred_test,'tpr','fpr')#Generating ROC curve data

#AUC
auc1<-performance(knn_pred_test,'auc')#Creating AUC data
knn_auc_test<-as.numeric(auc1@y.values)#Calculating AUC
knn_auc_test #AUC score 

#Curve
par(mar=c(5,7,3,3))
par(mfrow=c(1,1))
plot(knn_roc_test)
plot(knn_roc_test,add=T,col='red')
legend('right', legend=c('KNN',round(knn_auc_test,2)),col=c('red'),lty=1:2,cex=1)




#Using SVM

trctrl <- trainControl(method = "cv", number = 10,classProbs =T,
                       summaryFunction = twoClassSummary)
grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
set.seed(123)
svm_Linear_best <- train(target ~., data = x_train1, method = "svmLinear",trControl=trctrl,
                         preProcess = c("center", "scale"),metric='ROC',tuneGrid = grid)

svm_Linear_best

#svm_Linear_best model plot
plot(svm_Linear_best)

#Best tune parameter
svm_Linear_best$bestTune

#Influence of variables on Model
varImp(svm_Linear_best)
plot(varImp(svm_Linear_best))

#Compute model prediction accuracy rate
test_pred_grid <- predict(svm_Linear_best,newdata = x_test1,C=0.01)


cm2<-confusionMatrix(as.factor(test_pred_grid),as.factor(x_test1$target))
par(mar=c(4.5,4.5,1.7,1.8))
par(mfrow=c(2,1))
draw_confusion_matrix(cm2)
draw_details(cm2)

#Using Area Under Curve(AUC)
#Creating ROC Curve
svm_pred_test<-prediction(as.numeric(test_pred_grid),as.numeric(x_test1$target))
svm_roc_test<-performance(dtree_pred_test,'tpr','fpr')#Generating ROC curve data

#AUC
auc2<-performance(svm_pred_test,'auc')#Creating AUC data
svm_auc_test<-as.numeric(auc2@y.values)#Calculating AUC
svm_auc_test #AUC score

#Curve
par(mar=c(5,7,3,3))
par(mfrow=c(1,1))
plot(svm_roc_test)
plot(svm_roc_test,add=T,col='red')
legend('right', legend=c('SVM',round(svm_auc_test,2)),col=c('red'),lty=1:2,cex=1)


#Comparison between SVM and KNN using AUC

par(mar=c(5,7,3,3))
par(mfrow=c(1,1))
plot(knn_roc_test,col='red')
plot(svm_roc_test,add=T,col='blue')
legend('right', legend=c('KNN','SVM'),col=c('red','blue'),lty=1:2,cex=1)



