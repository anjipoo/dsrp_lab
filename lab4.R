#logistic regression

#load and explore
data<-read.csv("C:/Users/anjan/Desktop/pypypypy/dsrp/WA_Fn-UseC_-Telco-Customer-Churn.csv", stringsAsFactors=TRUE)
str(data)
head(data)
colnames(data)

#cleaning
data_clean<-na.omit(data) #remove missing
data_clean$Churn<-ifelse(data_clean$Churn=="Yes", 1,0)  #make categorical to numeric cuz proba
write.csv(data_clean, "cleaned_churn.csv", row.names=FALSE)

#choose imp features
model<-data_clean[, c("Churn", "tenure", "MonthlyCharges", "SeniorCitizen")]  #choose imp features

#split & model
set.seed(123)
train<-sample(1:nrow(model), 0.7*nrow(model))
train_data<-model[train,]
test_data<-model[-train,]

logreg<-glm(Churn~., data = train_data, family = binomial)  #binomial imp otherwise itll work like linear reg
summary(logreg)

#predictions
pred<-predict(logreg, newdata=test_data, type="response")
pred_class<-ifelse(pred>=0.5,1,0)

#eval
conf<-table(Predicted=pred_class, Actual=test_data$Churn)
print(conf)
acc<-mean(pred_class==test_data$Churn)
print(acc)

#roc curve
library(pROC)
roc_obj<-roc(test_data$Churn, pred)
plot(roc_obj, main="ROC Curve")
auc(roc_obj)
#a performance metric for binary classification models, representing the probability that a model ranks a random positive example higher than a negative one

#visualizations
#target dist
barplot(table(data_clean$Churn), col="orange", main="churn dist")

#ft vs target
boxplot(tenure~Churn, data=data_clean, col=c("orange", "green"), main="feature vs churn")

