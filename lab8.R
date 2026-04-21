#classif random forest

data(mtcars)
df<-mtcars
summary(df)  #shows mean, median, min, max 
str(df)  #shows structure: cols, dtypes, some example values
head(df)

#processing
df$am<-as.factor(df$am)  #making am as factor for classification
df_clean<-df

#tts
set.seed(123)  #for reproduceability

library(caret)
train_idx<-createDataPartition(df_clean$am, p=0.7, list = FALSE)  #cDP for class balance
train<-df_clean[train_idx,]
test<-df_clean[-train_idx,]

#model

# install.packages("randomForest")
library(randomForest)
#takes random samples -> builds multiple trees -> uses random features -> then classif (majority) or regression (avg)
model_cls<-randomForest(am~., data=train, ntree=100)  #predict am using all features, ntree=no. of trees
print(model_cls)  #gives error rate, confu matrix, out of bag errors 
#OOB- It provides an unbiased estimate of the model's prediction error on unseen data without the need for a separate validation set or cross-validation. 
# oob uses samples that were not included in any bootstrap sampling

#prediction
pred_cls<-predict(model_cls, test)
confusionMatrix(pred_cls, test$am)  #acc, prec, rec, f1

#vizzzzz
plot(model_cls)  #shows error vs no. of trees
varImpPlot(model_cls)  #shows feature importance
#uses meandecreasegini (Sum of all reductions in Gini Impurity caused by a feature across all splits (and trees), averaged → higher total reduction = higher importance) and meandecreaseaccuracy (Train model → shuffle one feature → measure drop in accuracy → average this drop over all trees → bigger drop = more important feature.)

#feature imp
library(ggplot2)

imp <- as.data.frame(importance(model_cls))  #cvt importance into df
imp$Feature <- rownames(imp)

ggplot(imp, aes(reorder(Feature, MeanDecreaseGini), MeanDecreaseGini)) + geom_col(fill = "darkgreen") + coord_flip() + labs(title = "Feature Importance")

#regression random forest

df<-mtcars
head(df)

df$am<-as.factor(df$am)
df_clean<-df

#tts
set.seed(123)

library(caret)
train_idx<-createDataPartition(df_clean$am, p=0.7, list = FALSE)
train<-df_clean[train_idx,]
test<-df_clean[-train_idx,]

#model

library(randomForest)
model_reg<-randomForest(mpg~., data=train, ntree=100)  #mpg is the target var
print(model_reg)

#pred
pred_reg<-predict(model_reg, test)

residuals<-pred_reg-test$mpg  #error = actual-pred

rmse<-sqrt(mean(residuals^2))  #rmse = sqrt of mean of error^2 -> lower=better
rmse
r2<-cor(pred_reg, test$mpg)^2  #r2 shows how well model explains variance -> 0=bad, 1=perfect
r2

#vizzzzz
plot(test$mpg, pred_reg, main="actual vs pred", xlab="actual", ylab="pred")

varImpPlot(model_reg)
plot(model_reg)

