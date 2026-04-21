#using toothgrowth dataset

#load data
data(ToothGrowth)
df<-ToothGrowth

#basic exploration
head(df)
tail(df)
str(df)  #structure -> colnames, dtypes, preview of values
summary(df)  #stat summ -> min, max, mean, median, quartiles, freq counts
colnames(df)  

#access col
df$len

#to add new col
df$cat<-ifelse(df$len >=mean(df$len), "high", "low")
head(df)

#filter rows
filt<-subset(df,len>=15)
head(filt)

#sorting
sorted<-df[order(df$len),]
head(sorted)

#rename & delete cols
#colnames(df)[1]<-"toothlength"
#df$supp<-NULL
head(df)

#cleaning and all
colSums(is.na(df))
num_cols<-sapply(df,is.numeric)
for(c in names(df)[num_cols]) {
  df[[c]][is.na(df[[c]])]<-mean(df[[c]], na.rm=TRUE)
}

#to save
write.csv(df,"new_data.csv",row.names=FALSE)
print("data saved")

#visualizations

hist(df$len, main="dist", xlab="tooth length")  #params -> numeric data to plot, title, x axis label

boxplot(len~supp, data=df, main="tooth length vs supplement", xlab="supp type", ylab="tooth length")  #params -> measure x (num) for each group in y (cat), data where x & y are, title 

table_data<-table(df$supp)
barplot(table_data,main="cnt of supp types", xlab="supp types", ylab="freq")

plot(df$dose, df$len, main="dose vs length", xlab="dose", ylab="length")



#---------------------------
#using iris some basic stuff
#---------------------------

data(iris)
df<-iris

colnames(df)
colSums(is.na(df))
str(df)
head(df)
summary(df)

table_data<-table(df$Species)
barplot(table_data, main="freq dist of each specie", xlab="species", ylab="freq")

hist(df$Sepal.Length, main="dist of sepal length", xlab="lenght", ylab="freq")

plot(df$Petal.Length, df$Petal.Width, col=df$Species, main="scatter of petal length for each specie", xlab="length", ylab="width")

