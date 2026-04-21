#decision tree classifier

data(mtcars)
df<-mtcars
str(df)
summary(df)
head(df)

df$am<-factor(df$am, labels=c("au", "man"))
str(df)

barplot(table(df$am), main="transmission dist")
boxplot(mpg~am, data=df, main="mpg vs transmission")

#tts
# install.packages("caret")
library(caret)

set.seed(123)

train_idx<-createDataPartition(df$am, p=0.7, list=FALSE)
train<-df[train_idx,]
test<-df[-train_idx,]

#model

# install.packages("rpart")
library(rpart)

model<-rpart(am~., data=train, method = "class")

print(model)
summary(model)

#install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(model, type=2, extra=104, fallen.leaves = TRUE, main="decision tree")

#pred
pred<-predict(model, test, type="class")

conf<-confusionMatrix(pred, test$am)
print(conf)
acc<-conf$overall['Accuracy']
print(acc)

plotcp(model)

#pruning
optimal<-model$cptable[which.min(model$cptable[, "xerror"]),"CP"]
model<-prune(model, cp=optimal)

rpart.plot(model, type=2, fallen.leaves = TRUE, main="pruned tree")

corr<-cor(df[, sapply(df, is.numeric)])
heatmap(corr, main="corr heatmap")
