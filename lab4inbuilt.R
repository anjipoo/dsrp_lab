#logistic regression using inbuilt dataset

data(mtcars)
df<-mtcars
summary(df)
str(df)
head(df)
colnames(df)

#target variable
df$am<-as.factor(df$am)  #Classification problems require categorical output

#feature selection
model_features<-df[, c("am", "mpg", "hp", "wt")]  #only choose imp features to reduce noise

#train test split and model

set.seed(123)
train_idx<-sample(1:nrow(model_features), 0.7*nrow(model_features))
train<-model_features[train_idx,]
test<-model_features[-train_idx,]

model<-glm(am~., data=train, family = binomial)  #predict am using other chosen features
#imp to put family=binomial else it'll work as linear reg
summary(model)  #showa coefficients etc

#pred
pred<-predict(model, test, type="response")  #returns proba
pred_cls<-ifelse(pred>=0.5, 1,0)  #cvt proba to class
pred_cls <- factor(pred_cls, levels = c(0,1))

#eval
acc<-mean(pred_cls==test$am)
acc

conf<-table(Predicted=pred_cls, Actual=test$am)
print(conf)  #tp tn fp fn 

#viz
library(pROC)
roc_obj <- roc(test$am, pred)
plot(roc_obj)
auc(roc_obj)
#a performance metric for binary classification models, representing the probability that a model ranks a random positive example higher than a negative one

hist(df$hp, main="Horsepower Distribution", col="purple")  #Check class balance
barplot(table(df$am), col="skyblue", main="Transmission Type")
cor_matrix <- cor(df[, sapply(df, is.numeric)])
heatmap(cor_matrix)