#kmeans

df<-USArrests
head(df)
str(df)  #var, dtypes, some example values
summary(df)  #mean median min max etc

scaled<-scale(df)  #scaling for distance metric for kmeans, mean=0 std=1
scaled_df<-as.data.frame(scaled)  #saving as dataframe for future use
head(scaled_df)

#elbow method

wss<-numeric(10)  #within sum of squares -> measures cluster compactness -> wss= summ of (x-centroid)^2
set.seed(123)
for(i in 1:10) {  #i is no. of clusters, nstart=run model multiple times for better ans
  wss[i]<-kmeans(scaled, centers = i, nstart=10)$tot.withinss
}

plot(1:10, wss, type="b", xlab="k", ylab="wss", main="elbow method")  #Where decrease slows down → optimal K

#apply kmeans
#steps: select k -> assign pts to nearest centroid, update centroid using mean, repeat until convergence
set.seed(123)
model<-kmeans(scaled, centers = 3, nstart=25)

scaled_df$Cluster<-as.factor(model$cluster)  #model$cluster gives each cluster a number
head(scaled_df)
df$Cluster <- as.factor(model$cluster)  # cvt to factor for viz

#vizzzzz
# install.packages("ggplot2")
library(ggplot2)

ggplot(df, aes(x=Murder, y=Assault, color=Cluster)) + geom_point(size=3) + labs(title="kmeans")
#cluster viz in 2d, x=murder, y=assault
#shows separation & patterns

model$centers  #shows all centroid
model$size  #shows no. of points in each cluster

#vixzzzzz
boxplot(Murder ~ Cluster, data = scaled_df, col = "lightblue")
