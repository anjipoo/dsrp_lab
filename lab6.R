#naive bayes

data(ToothGrowth)
df<-ToothGrowth
str(df)  #col, dtypes, example values
summary(df)  #mean median min max etc
head(df)

#basic viz
boxplot(len~supp, data=df, main="tooth length vs suppl")  #shows dist of len wrt supplement

#preprocessing
df$supp<-as.factor(df$supp)  #Naive Bayes handles classification → target must be factor
df$dose<-as.factor(df$dose)  #Even though numeric, here it represents categories (0.5, 1, 2)

clean<-df  #save as new df

#tts
set.seed(123)

train_idx<-sample(1:nrow(clean), 0.7*nrow(clean))
train<-clean[train_idx,]
test<-clean[-train_idx,]

#model
# install.packages("e1071")
library(e1071)
model<-naiveBayes(supp~len+dose, data=train)  #Predict supp using len and dose, model learns Probability distributions of features per class

#pred
pred<-predict(model, test)  #predicts oj or vc

#eval
conf<-table(Predicted=pred, Actual=test$supp)  #conf matrix
print(conf)
acc<-mean(pred==test$supp)  #corrct/total
acc

#vizzzz
plot(test$len, col=pred, main="pred classes")  #scatterplot to viz separation
