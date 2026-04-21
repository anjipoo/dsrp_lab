#decision tree classifier

data(mtcars)
df<-mtcars
str(df)  #col names, dtypes, examples
summary(df)  #mean median min max etc
head(df)

df$am<-factor(df$am, labels=c("au", "man"))  #cat for decision tree classif, am is the target variable
str(df)

barplot(table(df$am), main="transmission dist")  #class dist
boxplot(mpg~am, data=df, main="mpg vs transmission")  #dist of feature vs target

#tts
# install.packages("caret")
library(caret)

set.seed(123)

train_idx<-createDataPartition(df$am, p=0.7, list=FALSE)  #cDP maintains class distribution
train<-df[train_idx,]
test<-df[-train_idx,]

#model

# install.packages("rpart")
library(rpart)

model<-rpart(am~., data=train, method = "class")  #target = am, method =class because classif problem
#by default uses gini -> measure of how impure a node is in a decision tree, defined as 1-p^2
#gini lower = better

print(model)  #shwos splits, var used, error
summary(model)

#install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(model, type=2, extra=104, fallen.leaves = TRUE, main="decision tree")  #shows tree

#pred
pred<-predict(model, test, type="class")  #returns class cat instead of proba

conf<-confusionMatrix(pred, test$am)
print(conf)
acc<-conf$overall['Accuracy']
print(acc)

plotcp(model)  #complexity parameter shows Error vs tree size

#pruning
optimal<-model$cptable[which.min(model$cptable[, "xerror"]),"CP"]  #removing unnecessary branches
model<-prune(model, cp=optimal)

rpart.plot(model, type=2, fallen.leaves = TRUE, main="pruned tree")  #plotting pruned tree

corr<-cor(df[, sapply(df, is.numeric)])
heatmap(corr, main="corr heatmap")
