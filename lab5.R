#linear regression

#loading data
data(mtcars)
df<-mtcars
head(df)
str(df)

#exploration
summary(df)
colnames(df)
colSums(is.na(df))

#cleaning
df$mpg[is.na(df$mpg)]<-mean(df$mpg, na.rm=TRUE)  #fill as mean 
df$cyl<-as.factor(df$cyl)  #to cvt to categorical
df$gear<-as.factor(df$gear)

#train test split
set.seed(123)
train_idx<-sample(1:nrow(df), 0.7*nrow(df))
train<-df[train_idx,]
test<-df[-train_idx,]

#model
model=lm(mpg~wt+hp+cyl+gear, data=train)
summary(model)

pred<-predict(model, newdata=test)
head(data.frame(Actual=test$mpg, Predicted=pred))

#evals
residuals<-test$mpg-pred
MAE<-mean(abs(residuals))
RMSE<-sqrt(mean(residuals^2))
print(MAE)
print(RMSE)

#visualizations
plot(test$mpg, pred, main="actual vs predicted", xlab="actual", ylab="predicted", col="blue")
abline(0,1,col="red")
hist(residuals, main="residuals dist", col="orange")
boxplot(df$mpg, main="mpg dist", col="green")
cormat<-cor(df[sapply(df,is.numeric)])
heatmap(cormat, main="corr heatmap")

#anova
anova(model)
