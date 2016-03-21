####################################################################################
# Author: Pooja Shah
# Foundations of Intelligent Systems Homework #2
# Description: Predict the Class of transformer by logistic regression
####################################################################################

#Load data from csv file
dataset<-read.csv("IF1-FEB22-detailed.csv")

dataset_logistic<-dataset[c(-1,-7)]

# Split categories of Class into different columns with binomial values
dataset_logistic[c("Class_B","Class_G","Class_N", "Class_M")]<-NA
dataset_logistic$Class_B<- ifelse(dataset_logistic$Class == "B",1,0)
dataset_logistic$Class_G<- ifelse(dataset_logistic$Class == "G",1,0)
dataset_logistic$Class_N<- ifelse(dataset_logistic$Class == "N",1,0)
dataset_logistic$Class_M<- ifelse(dataset_logistic$Class == "M",1,0)

#Split the data
set.seed(101)
index <- sample(1:nrow(dataset_logistic), size <- 0.70*nrow(dataset_logistic))
logistic_train_data <- dataset_logistic[index,]
logistic_test_data <- dataset_logistic[-index,]
logistic_test_data_multinom<-dataset_logistic[-index,]
logistic_test_data_highlyCor <- dataset_logistic[-index,]

#Store the column names of the data set
column_names<-c()
column_names<-c(column_names,colnames(dataset_logistic))

#Store the dependent variables
dependent_vars<-c()
for(i in 7:10){
  dependent_vars<-c(dependent_vars,column_names[i])
}

#Store the indepedent variables
indep_var<-c()
for(i in 1:5)
{
  indep_var<-c(indep_var,column_names[i])
}
independent_vars<-paste(indep_var,collapse = "+")

# Build a logistic regression model on each Class group for all dataset
logisticModel <- list()

for (y in dependent_vars){
  logisticModel[[y]]<-glm(as.formula(paste(y,"~" , independent_vars)), data=logistic_train_data)
}

lapply(logisticModel, summary)

#Plot each logistic regression model for all dataset
for(y in dependent_vars){
  plot(logisticModel[[y]])
}

#Predict Class for all dataset
prediction<-list()

for(y in dependent_vars){
  prediction[[y]] <- predict(logisticModel[[y]], logistic_test_data)
}

lapply(prediction, summary)

logistic_test_data$predicted_Class_B<-prediction[[1]]
logistic_test_data$predicted_Class_G<-prediction[[2]]
logistic_test_data$predicted_Class_N<-prediction[[3]]
logistic_test_data$predicted_Class_M<-prediction[[4]]

#Find the maximum value of all the predicted classes
logistic_test_data$predicted_Class<-colnames(logistic_test_data[11:14])[apply(logistic_test_data[11:14],1,which.max)]

#Assigning the class name to predicted_Class column
logistic_test_data$predicted_Class<-ifelse(logistic_test_data$predicted_Class=="predicted_Class_G", "G",ifelse(logistic_test_data$predicted_Class=="predicted_Class_B","B",ifelse(logistic_test_data$predicted_Class=="predicted_Class_M","M","N")))

#Writing to csv and exporting to the working directory
write.csv(logistic_test_data,"./testing_LogisticRegAll_PredictedVals.csv")

#Confusion matrix for logistic regression model for all dataset
confusion_Matrix_Logistic<- table(logistic_test_data$predicted_Class, logistic_test_data$Class)
confusionMat_Logistic<-as.data.frame(confusion_Matrix_Logistic)
write.csv(confusionMat_Logistic,"./Confusion_Matrix_Logistic_All.csv")

#plot ROC for each class of logistic regression model for all dataset
library(pROC)
rocB<- roc(Class_B ~ predicted_Class_B,data = logistic_test_data)
plot(rocB)

rocG <- roc(Class_G ~ predicted_Class_G, data=logistic_test_data)
plot(rocG)

rocM <- roc(Class_M ~ predicted_Class_M, data=logistic_test_data)
plot(rocM)

rocN <- roc(Class_N ~ predicted_Class_N, data=logistic_test_data)
plot(rocN)


#Find the highly correlated values of Class column
library(randomForest)
randomForest_logistic<-randomForest(as.formula(paste("Class~" , independent_vars)),data=logistic_train_data)
attributesImp<-importance(randomForest_logistic)

randomForest_Logistic_HighCor <- as.data.frame(attributesImp)
write.csv(randomForest_Logistic_HighCor,"./Random_Forest_Logistic_HighlyCorrelated.csv")

#Store the indepedent variables
highlyCorrelated<- c("Total.Acid.Number..mgKOH..g.","Colour")

# Build a logistic regression model for highly correlated values on each Class group
logisticModel_highlyCorrelated <- list()

for (y in dependent_vars){
  logisticModel_highlyCorrelated[[y]]<-glm(as.formula(paste(y,"~" , paste(highlyCorrelated,collapse = "+"))), data=logistic_train_data)
}

lapply(logisticModel_highlyCorrelated, summary)

#Plot each logistic regression model for highly correlated values
for(y in dependent_vars){
  plot(logisticModel_highlyCorrelated[[y]])
}

#Predict Class for highly correlated values
prediction_highlyCor<-list()

for(y in dependent_vars){
  prediction_highlyCor[[y]] <- predict(logisticModel_highlyCorrelated[[y]], logistic_test_data_highlyCor)
}

lapply(prediction_highlyCor, summary)

logistic_test_data_highlyCor$predicted_Class_B<-prediction_highlyCor[[1]]
logistic_test_data_highlyCor$predicted_Class_G<-prediction_highlyCor[[2]]
logistic_test_data_highlyCor$predicted_Class_N<-prediction_highlyCor[[3]]
logistic_test_data_highlyCor$predicted_Class_M<-prediction_highlyCor[[4]]

#Find the maximum value of all the predicted classes
logistic_test_data_highlyCor$predicted_Class<-colnames(logistic_test_data_highlyCor[11:14])[apply(logistic_test_data_highlyCor[11:14],1,which.max)]

logistic_test_data_highlyCor$predicted_Class<-ifelse(logistic_test_data_highlyCor$predicted_Class=="predicted_Class_G", "G",ifelse(logistic_test_data_highlyCor$predicted_Class=="predicted_Class_B","B",ifelse(logistic_test_data_highlyCor$predicted_Class=="predicted_Class_M","M","N")))

write.csv(logistic_test_data_highlyCor,"./testing_LogisticRegHighCor_PredictedVals.csv")

#Confusion matrix for logistic regression model for highly correlated values
ConfusionMatrix_HighCor<-table(logistic_test_data_highlyCor$predicted_Class, logistic_test_data_highlyCor$Class)
confusion_Matrix_HighlyCor<-as.data.frame(ConfusionMatrix_HighCor)
write.csv(confusion_Matrix_HighlyCor,"./Confusion_Matrix_Logistic_HighlyCor.csv")

#plot ROC for each class for logistic model of highly correlated variables
library(pROC)
rocB_highlyCor<- roc(Class_B ~ predicted_Class_B,data = logistic_test_data_highlyCor)
plot(rocB_highlyCor)

rocG_highlyCor <- roc(Class_G ~ predicted_Class_G, data=logistic_test_data_highlyCor)
plot(rocG_highlyCor)

rocM_highlyCor <- roc(Class_M ~ predicted_Class_M, data=logistic_test_data_highlyCor)
plot(rocM_highlyCor)

rocN_highlyCor <- roc(Class_N ~ predicted_Class_N, data=logistic_test_data_highlyCor)
plot(rocN_highlyCor)

#multinomial
library(nnet)
logistic_model_all <-multinom(Class ~ ., data=logistic_train_data)
summary(logistic_model_all)
pred <- predict(logistic_model_all, logistic_test_data_multinom, "probs")
predict_Class <- predict(logistic_model_all, logistic_test_data_multinom)
summary(pred)
head(predict_Class)

