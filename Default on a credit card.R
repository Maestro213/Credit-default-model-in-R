library(ISLR)
library(Hmisc)
library(corrplot)
library(caTools)
library(ROCR)
library(cvms)
library(tibble)

credit_csv <- read.csv("/Users/fox2/Downloads/UCI_Credit_Card.csv")
str(credit_csv)
credit_csv[sample(1:nrow(credit_csv),10,replace=FALSE),]
head(credit_scv,5)

#is NaN-value present?
apply(is.na(credit_scv),2,sum)
summary_stat <- summary(credit_csv[,2:25])
print(summary_stat)


corrplot(cor(credit_csv[,7:12]))
corrplot(cor(credit_csv[,13:18]))
corrplot(cor(credit_csv[,19:24]))

default<-table(credit_csv$default.payment.next.month)
barplot(default,main="Default Credit Cards",ylab="Number of Cards",names.arg = c("Non-default", "Default"))
barplot(table(credit_csv$LIMIT_BAL),main="Amount of a Credit Limit")

boxplot(credit_csv$LIMIT_BAL~credit_csv$AGE)
boxplot(credit_csv$LIMIT_BAL~credit_csv$SEX)

#Creating a model
set.seed(85)
res <- sample.split(credit_csv$default.payment.next.month,0.7)
Train_data <- subset(credit_csv, res == TRUE)
Test_data <- subset(credit_csv, res ==FALSE)

glm_model <- glm(default.payment.next.month~.,family=binomial(link='logit'),data=Train_data)
summary(glm_model)

glm_model2 <- glm(default.payment.next.month~LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE+PAY_0+PAY_2+PAY_3+BILL_AMT1+PAY_AMT1+PAY_AMT2,data=Train_data,family =binomial(link='logit'))
summary(glm_model2)

glm_model3 <-glm(default.payment.next.month~LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE+PAY_0+BILL_AMT1+PAY_AMT1,data=Train_data,family =binomial(link='logit'))
summary(glm_model3)

#Model accuracy glm2

Test_data$pred.risk2 <- predict(glm_model2, newdata = Test_data,type="response")
cfm2<-tibble(target=Test_data$default.payment.next.month,prediction=as.numeric(Test_data$pred.risk2>=0.5))

cfm2<- as_tibble(table(cfm2))

plot_confusion_matrix(cfm2, target_col = "target", prediction_col = "prediction",counts_col = "n",class_order=c('1','0'))

#ROC curve glm2

pred <- prediction(Test_data$pred.risk2,Test_data$default.payment.next.month)
as.numeric(performance(pred,"auc")@y.values)
predict_Test <- predict(glm_model2,newdata=Test_data,type="response")
ROCpred <- prediction(predict_Test,Test_data$default.payment.next.month)
ROCperf <-performance(ROCpred,"tpr","fpr")
plot(ROCperf)

#Model accuracy glm3

Test_data$pred.risk3 <- predict(glm_model3, newdata = Test_data,type="response")
cfm3<-tibble(target=Test_data$default.payment.next.month,prediction=as.numeric(Test_data$pred.risk3>=0.5))

cfm3<- as_tibble(table(cfm3))

plot_confusion_matrix(cfm3, target_col = "target", prediction_col = "prediction",counts_col = "n",class_order=c('1','0'))

#ROC curve

pred <- prediction(Test_data$pred.risk3,Test_data$default.payment.next.month)
as.numeric(performance(pred,"auc")@y.values)
predict_Test <- predict(glm_model3,newdata=Test_data,type="response")
ROCpred <- prediction(predict_Test,Test_data$default.payment.next.month)
ROCperf <-performance(ROCpred,"tpr","fpr")
plot(ROCperf)


