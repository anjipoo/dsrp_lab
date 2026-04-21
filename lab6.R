#naive bayes

data(ToothGrowth)
df<-ToothGrowth
str(df)
summary(df)
head(df)

#basic viz
boxplot(len~supp, data=df, main="tooth length vs suppl")

#preprocessing
df$supp<-as.factor(df$supp)
df$dose<-as.factor(df$dose)

clean<-df

#tts
set.seed(123)

train_idx<-sample(1:nrow(clean), 0.7*nrow(clean))
train<-clean[train_idx,]
test<-clean[-train_idx,]

#model
# install.packages("e1071")
library(e1071)
model<-naiveBayes(supp~len+dose, data=train)

#pred
pred<-predict(model, test)

#eval
table(Predicted=pred, Actual=test$supp)
acc<-mean(pred==test$supp)
acc

#vizzzz
plot(test$len, col=pred, main="pred classes")
