#logistic regression using inbuilt dataset

data(mtcars)
df<-mtcars
summary(df)
str(df)
head(df)
colnames(df)

#target variable
df$am<-as.factor(df$am)
#feature selection
model_features<-df[, c("am", "mpg", "hp", "wt")]

#train test split and model

set.seed(123)
train_idx<-sample(1:nrow(model_features), 0.7*nrow(model_features))
train<-model_features[train_idx,]
test<-model_features[-train_idx,]

model<-glm(am~., data=train, family = binomial)
summary(model)

#pred
pred<-predict(model, test, type="response")
pred_cls<-ifelse(pred>=0.5, 1,0)
pred_cls <- factor(pred_cls, levels = c(0,1))

#eval
acc<-mean(pred_cls==test$am)
acc

conf<-table(Predicted=pred_cls, Actual=test$am)
print(conf)

#viz
library(pROC)
roc_obj <- roc(test$am, pred)
plot(roc_obj)
auc(roc_obj)

hist(df$hp, main="Horsepower Distribution", col="purple")
barplot(table(df$am), col="skyblue", main="Transmission Type")
cor_matrix <- cor(df[, sapply(df, is.numeric)])
heatmap(cor_matrix)