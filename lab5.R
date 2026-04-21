#linear regression

#loading data
data(mtcars)
df<-mtcars
head(df)
str(df)

#exploration
summary(df)
colnames(df)
colSums(is.na(df))  #shows NA counts for each col

#cleaning
df$mpg[is.na(df$mpg)]<-mean(df$mpg, na.rm=TRUE)  #replace na with mean
df$cyl<-as.factor(df$cyl)  #cvt to categorical
df$gear<-as.factor(df$gear)  #Required for regression to treat them as categories

#train test split
set.seed(123)
train_idx<-sample(1:nrow(df), 0.7*nrow(df))
train<-df[train_idx,]
test<-df[-train_idx,]

#model
model=lm(mpg~wt+hp+cyl+gear, data=train)  #mpg is target, wt hp cyl gear are predictors
summary(model)  #shows coeff, r2, p values

pred<-predict(model, newdata=test)
head(data.frame(Actual=test$mpg, Predicted=pred))  #shows confu matrix

#evals
residuals<-test$mpg-pred
MAE<-mean(abs(residuals))
RMSE<-sqrt(mean(residuals^2))  #Penalizes large errors
print(MAE)
print(RMSE)

#visualizations
plot(test$mpg, pred, main="actual vs predicted", xlab="actual", ylab="predicted", col="blue")
abline(0,1,col="red")
# Points near line → good model

hist(residuals, main="residuals dist", col="orange")  #Should be normally distributed
boxplot(df$mpg, main="mpg dist", col="green")  #dist of mpg
cormat<-cor(df[sapply(df,is.numeric)])  #corr heatmap
heatmap(cormat, main="corr heatmap")

#anova
anova(model)  #checks usefulness of each feature that is being added
# Small p-value (< 0.05) → variable is important
# Large p-value (> 0.05) → variable may not matter much
