#importing libraries and data
library(ranger)
library(caret)
library(data.table)
data <- read.csv("C:\\Users\\rishu\\Desktop\\Credit_card_fraud\\Activity-2_CCFD\\creditcard.csv")

#exploration

data.table(data)

#doing statistical analysis

summary(data)
table(data$Class)
names(data)

#summary of amount
summary(data$Amount)
sd(data$Amount)
IQR(data$Amount)
var(data$Amount)

#manipulation
data$Amount<- scale(data$Amount) #normalization

data2<- data[,-c(1)] #removing time from data
head(data2)

set.seed(12)
library(caTools)

sample_data <- sample.split(data2$Class,SplitRatio=0.80)

train_data <- subset(data2,sample_data==TRUE)
test_data <- subset(data2,sample_data==FALSE)

dim(train_data)
dim(test_data)

#fit logit on data

#summary(Logistic_model)

#plot(Logistic_model)

Logistic_model1 <- glm(Class~.,train_data,family=binomial())
summary(Logistic_model1)

plot(Logistic_model1)


#we need ROC curve visit bigquery tutorial to learn about ROC
library(pROC)
lr.predict <- predict(Logistic_model1,test_data,probability = TRUE)
auc.gb <- roc(test_data$Class,lr.predict,plot=TRUE,col="green")

#so we have atmost 90% accuracy but it is not right method of doing project

#fit a decision tree
library(rpart)
library(rpart.plot)

desicion_model <- rpart(Class ~ . , data,method = "class")
predicted_val <- predict(desicion_model,data,type="class")
probability <- predict(desicion_model,data,type='prob')
rpart.plot(desicion_model)
