#Simple Linear Regression 
getwd()

#Importing the data
dataset = read.csv("Salary_Data.csv")

#Splitting the data into train and test
library(caTools)
set.seed(2)
split=sample.split(dataset$Salary,SplitRatio = 2/3)
training_set = subset(dataset,split==T)
test_set = subset(dataset,split==F)

View(training_set)
View(test_set)

#nrow(training_set)

#Fitting the Simple Linear Regression to the training set
regressor = lm(formula = Salary~YearsExperience,
               data = training_set)

summary(regressor)

#Predicting the test set results
y_predict = predict(object = regressor,newdata = test_set)


#Visualising the training set

library(ggplot2)

p = ggplot() + 
  geom_point(aes(x=training_set$YearsExperience,y=training_set$Salary),
             color='red') +
  geom_line(aes(x=training_set$YearsExperience,y=predict(object = regressor,newdata = training_set)),
                color='blue') +
  ggtitle(label = "YearsExperience Vs Salary(Training Set)") +
  xlab(label = "Years of Expiernce") +
  ylab(label="Salary")


#Visualising the test set

p = ggplot() + 
  geom_point(aes(x=test_set$YearsExperience,y=test_set$Salary),
             color='red') +
  geom_line(aes(x=training_set$YearsExperience,y=predict(object = regressor,newdata = training_set)),
            color='blue') +
  ggtitle(label = "YearsExperience Vs Salary(Test Set)") +
  xlab(label = "Years of Expiernce") +
  ylab(label="Salary")
  
