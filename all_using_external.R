## using titanic dataset fo r lab 1,2,4,5,6,7a,8

## lab 1 data handling

df<-read.csv("Titanic-Dataset.csv")
str(df)

summary(df)

head(df)

colnames(df)

colSums(is.na(df))

tail(df)

df$Age

df$Survived

filtered<-subset(df, Age>=50)
filtered

df$AgeGroup<-ifelse(df$Age>=30, "adult", "young")
head(df$AgeGroup)

#visualizations
boxplot(df$Age~df$Survived, main="dist of age", col="yellow")
plot(df$Age, df$Fare, main="age vs fare", col=ifelse(df$Survived==1, "blue", "red"), pch=19)
legend("topright", legend = c("not survived", "survived"),col=c("red", "blue"), pch=19)

## lab 2 stats + hypothesis testing

#cleaning
clean_df<-na.omit(df)
colSums(is.na(clean_df))

x<-df$Fare
min(x)
max(x)
mean(x)
median(x)
sd(x)
var(x)

#h0: mean=32, h1: mean!=32
t.test(x, mu=32.20421)

#h0: mean of age of survived is same as not survived, h1: is not same
t.test(Age~Survived, data=clean_df)
#p<0.05 so reject h0

#h0: mean fare for survived and not survived same, h1: diff
t.test(Fare~Survived, data=clean_df)
#p<0.05 so reject h0

## lab 4 logistic regression

feat<-clean_df[,c("Survived", "Pclass", "Age", "Fare")]

set.seed(123)

train_idx<-sample(1:nrow(feat), 0.7*nrow(feat))
train<-feat[train_idx,]
test<-feat[-train_idx,]

model_logreg<-glm(Survived~., data=train, family = binomial)
summary(model_logreg)

pred<-predict(model_logreg, test, type="response")
pred_class<-ifelse(pred>=0.5, 1, 0)

conf<-table(Predicted=pred_class, Actual=test$Survived)
print(conf)
acc<-mean(pred_class==test$Survived)
print(acc)

library(pROC)
roc_obj<-roc(test$Survived, pred)
plot(roc_obj)
auc(roc_obj)

#lab 5 linear regression

set.seed(123)
linreg_feat<-clean_df[, c("Pclass", "Age", "Fare")]
train_idx<-sample(1:nrow(linreg_feat), 0.7*nrow(linreg_feat))
train_linreg<-linreg_feat[train_idx,]
test_linreg<-linreg_feat[-train_idx,]

model_linreg<-lm(Fare~., data=train_linreg)
summary(model_linreg)

pred_linreg<-predict(model_linreg, test_linreg)
residuals=test_linreg$Fare-pred_linreg
rmse=sqrt(mean(residuals^2))
rmse
mae=mean(abs(residuals))
mae

plot(test_linreg$Fare, pred_linreg, main="actual vs predicted", xlab="actual", ylab="pred")
abline(0,1,col="red")

## lab 6 naive bayes

clean_df$Survived<-as.factor(clean_df$Survived)
clean_df$Pclass<-as.factor(clean_df$Pclass)
clean_df$Sex<-as.factor(clean_df$Sex)

nb_feat<-clean_df[, c("Survived", "Pclass", "Sex")]

set.seed(123)

train_idx<-sample(1:nrow(nb_feat), 0.7*nrow(nb_feat))
train_nb<-nb_feat[train_idx,]
test_nb<-nb_feat[-train_idx,]

library(e1071)
model_nb<-naiveBayes(Survived~., data=train_nb)

pred_nb<-predict(model_nb, test_nb)
conf<-table(Predicted=pred_nb, Actual=test_nb$Survived)
print(conf)

acc<-mean(pred_nb==test_nb$Survived)
acc

barplot(conf, col=c("red", "blue"), main="confu matrix")

#lab 7a decision tree

library(rpart)
library(caret)

clean_df$Survived <- as.factor(clean_df$Survived)

feat_dt <- clean_df[, c("Survived", "Pclass", "Sex", "Age")]

set.seed(123)
train <- sample(1:nrow(feat_dt), 0.7*nrow(feat_dt))

train_dt <- feat_dt[train, ]
test_dt <- feat_dt[-train, ]

model_dt <- rpart(Survived ~ ., data=train_dt, method="class")

pred_dt <- predict(model_dt, test_dt, type="class")

confusionMatrix(pred_dt, test_dt$Survived)

library(rpart.plot)
rpart.plot(model_dt)

## lab 8 random forest

library(randomForest)
library(caret)

clean_df$Survived <- as.factor(clean_df$Survived)
clean_df$Pclass   <- as.factor(clean_df$Pclass)
clean_df$Sex      <- as.factor(clean_df$Sex)


rf_feat <- clean_df[, c("Survived", "Pclass", "Sex", "Age")]

set.seed(123)
train <- sample(1:nrow(rf_feat), 0.7*nrow(rf_feat))

train_rf <- rf_feat[train, ]
test_rf<- rf_feat[-train, ]

model_rf <- randomForest(Survived ~ ., data=train_rf, ntree=100)

pred_rf <- predict(model_rf, test_rf)

confusionMatrix(pred_rf, test_rf$Survived)

plot(model_rf)
varImpPlot(model_rf)

## kmeans clustering using mall customers lab 7b

df7 <- read.csv("Mall_Customers.csv")

# select numeric columns
num_df <- df7[, c("Age", "Annual.Income..k..", "Spending.Score..1.100.")]

# scale
df7_scaled <- scale(num_df)

# elbow method
wss <- numeric(10)
for(i in 1:10){
  wss[i] <- kmeans(df7_scaled, centers=i, nstart=10)$tot.withinss
}

plot(1:10, wss, type="b",
     xlab="Clusters", ylab="WSS",
     main="Elbow Method")

# kmeans
set.seed(123)
kmeans_model <- kmeans(df7_scaled, centers=3, nstart=25)

df7$Cluster <- as.factor(kmeans_model$cluster)

# visualization
plot(num_df$Annual.Income..k.., num_df$Spending.Score..1.100.,
     col = kmeans_model$cluster,
     pch = 19)
