#classif random forest

data(mtcars)
df<-mtcars
summary(df)
str(df)
head(df)

#processing
df$am<-as.factor(df$am)
df_clean<-df

#tts
set.seed(123)

library(caret)
train_idx<-createDataPartition(df_clean$am, p=0.7, list = FALSE)
train<-df_clean[train_idx,]
test<-df_clean[-train_idx,]

#model

# install.packages("randomForest")
library(randomForest)
model_cls<-randomForest(am~., data=train, ntree=100)
print(model_cls)

#prediction
pred_cls<-predict(model_cls, test)
confusionMatrix(pred_cls, test$am)

#vizzzzz
plot(model_cls)
varImpPlot(model_cls)

#feature imp
library(ggplot2)

imp <- as.data.frame(importance(model_cls))
imp$Feature <- rownames(imp)

ggplot(imp, aes(reorder(Feature, MeanDecreaseGini), MeanDecreaseGini)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Feature Importance")


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
model_reg<-randomForest(mpg~., data=train, ntree=100)
print(model_reg)

#pred
pred_reg<-predict(model_reg, test)

residuals<-pred_reg-test$mpg

rmse<-sqrt(mean(residuals^2))
rmse
r2<-cor(pred_reg, test$mpg)^2
r2

#vizzzzz
plot(test$mpg, pred_reg, main="actual vs pred", xlab="actual", ylab="pred")

varImpPlot(model_reg)
plot(model_reg)

