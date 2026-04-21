#using airquality dataset

data(airquality)
df<-airquality
colnames(df)
str(df)
summary(df)
head(df)
colSums(is.na(df))

#cleaning
clean_data<-na.omit(df)
summary(clean_data)
colSums(is.na(clean_data))

x<-clean_data$Ozone
min(x)
max(x)
mean(x)
median(x)
var(x)
sd(x)

summary(clean_data)
quantile(x, probs = c(0.25,0.5,0.75))  #q1, median, q3
IQR(x)  #q3-q1

#visualizations
hist(x, main="dist of ozone", xlab="ozone level", ylab="freq", col="skyblue")

boxplot(x, main="boxplot", col="orange")

plot(clean_data$Temp, clean_data$Ozone, main="scatter", xlab="temp", ylab="ozone", col="blue")

#hypothesis testing and all

#one sample t test
#h0: mean=40, h1: mean!=40
t.test(x,mu=40)  #test if sample mean is significantly different from 40
#output
#test statistic, p val, confidence interval, sample mean
#if p<0.05 reject h0, else fail to reject

#two sample t test -> needs 2 groups
#check mean of ozone for 2 temp groups high & low
#h0: mean(hgih)=mean(low), h1: mean(high)!=mean(low)
clean_data$Group<-ifelse(clean_data$Temp>=80, "high", "low")
t.test(Ozone~Group, data=clean_data)
#output
#p val shows diff
#confidence interval for diff
#group means

#anova - for 3+ groups -> checks if multiple group means are equal
#h0: all group same mean, h1: 1 mean differs atleast
clean_data$TempGroup<-cut(clean_data$Temp, breaks=c(-Inf,75,85,Inf), labels=c("low","mid","high"))
model<-aov(Ozone~TempGroup, data=clean_data)
summary(model)
#anova checks if atleast one group mean is diff
#output
#f value
#p val <0.05 then reject h0 else fail to reject

#ab testing -> same as t test in this case
#a/b is for old vs new group
t.test(Ozone~Group, data=clean_data)
#output
#test statistic, p val, confidence interval, sample mean
#if p<0.05 reject h0, else fail to reject

#visualizations

cor(clean_data$Temp, clean_data$Ozone)  #corr between temp and ozone
plot(x, type="l", col="blue", xlab="time", ylab="ozone")  #trend of ozone
plot(density(x), main="density plot")  #dist curve like histogram, shows skewness/normal etc
barplot(table(clean_data$Month), col="green", xlab="obs per month", ylab="cnt")  #obs for each month

