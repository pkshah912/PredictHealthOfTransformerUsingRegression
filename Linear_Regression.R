####################################################################################
# Author: Pooja Shah
# Foundations of Intelligent Systems Homework #2
# Description: Predict the interfacial tension of transformer by linear regression
####################################################################################

#Load data from csv file
dataset<-read.csv("IF1-FEB22-detailed.csv")

#Exclude the year and class columns
linear_dataset<-dataset[c(-1,-8)]

#Displays the correlation between the variables
correlation_matrix <- cor(linear_dataset)

#Export correlation matrix
corMatrix_df <- as.data.frame(correlation_matrix)
write.csv(corMatrix_df,"./correlation_Matrix.csv")

#Finds highly correlated variables of interfacial tension
index <- 1
highlyCorrelatedCols<-c()
while(index < ncol(linear_dataset)){
  if(abs(correlation_matrix[6,index]) > 0.5){
    column <- index
    highlyCorrelatedCols <- c(highlyCorrelatedCols, column)
  }
  index <- index+1
}

#Store the column names
column_names<-c()
column_names<-c(column_names,colnames(correlation_matrix))

#Split the data into training and testing data
set.seed(101)
indexes <- sample(1:nrow(linear_dataset), size <- 0.70*nrow(linear_dataset))
linear_training_data <- linear_dataset[indexes,]
linear_testing_data <- linear_dataset[-indexes,]
linear_testing_data_highlyCor <- linear_dataset[-indexes,]

#Retrieve dependent variable column name
dependent_column<-column_names[6]

highlyCorrelated<-c()
for(i in 1:length(highlyCorrelatedCols)){
  highlyCorrelated<-c(highlyCorrelated,column_names[highlyCorrelatedCols[i]])
}

#A function for finding root mean square error
rmse<-function(actual_values,predicted_values,test_data){
  sqrt((sum((actual_values-predicted_values)^2))/nrow(test_data))
}

#Random forest
library(randomForest)
randomForest_linear<-randomForest(Interfacial.Tension..mN.m.~.,data=linear_training_data)
importanceRanForest<-importance(randomForest_linear)

randomForestLinear <- as.data.frame(importanceRanForest)
write.csv(randomForestLinear,"./randomForest_LinearRegression.csv")

#Build linear model for all dataset
linear_model_all <- lm(Interfacial.Tension..mN.m.~., data=linear_training_data)
summary(linear_model_all)
anova(linear_model_all)
plot(linear_model_all)

#Predict interfacial tension from linear model for all dataset
predict_tension <- predict.lm(linear_model_all, linear_testing_data)
summary(predict_tension)
linear_testing_data$predicted_tension <- predict_tension

write.csv(linear_testing_data,"./testing_LinearRegAll_PredictedVals.csv")

#Determine the accuracy of linear model for all dataset by finding rmse
actual_values<-linear_testing_data$Interfacial.Tension..mN.m.
predicted_values<-linear_testing_data$predicted_tension

rmse_values<-rmse(actual_values,predicted_values,linear_testing_data)
print(rmse_values)

#Builds the linear model for highly correlated variables
linear_model_highlyCorrelated<-lm(as.formula(paste("Interfacial.Tension..mN.m.~" , paste(highlyCorrelated, collapse="+"))), data=linear_training_data)
summary(linear_model_highlyCorrelated)
anova(linear_model_highlyCorrelated)
plot(linear_model_highlyCorrelated)

#Predict interfacial tension from linear model of highly correlated variables 
prediction_highlyCor <- predict.lm(linear_model_highlyCorrelated, linear_testing_data_highlyCor)
linear_testing_data_highlyCor$predicted_tension_highlyCorrelated <- prediction_highlyCor

write.csv(linear_testing_data_highlyCor,"./testing_LinearRegHighlyCor_PredictedVals.csv")

#Determine the accuracy of linear model for highly correlated variables by finding rmse
actual_highlyCor<-linear_testing_data_highlyCor$Interfacial.Tension..mN.m.
predicted_highlyCor<-linear_testing_data_highlyCor$predicted_tension_highlyCorrelated

rmse_highlyCor<-rmse(actual_highlyCor,predicted_highlyCor,linear_testing_data_highlyCor)
print(rmse_highlyCor)


