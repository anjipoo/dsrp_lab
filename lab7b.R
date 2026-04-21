#kmeans

df<-USArrests
head(df)
str(df)
summary(df)

scaled<-scale(df)
scaled_df<-as.data.frame(scaled)
head(scaled_df)

#elbow method

wss<-numeric(10)
set.seed(123)
for(i in 1:10) {
  wss[i]<-kmeans(scaled, centers = i, nstart=10)$tot.withinss
}

plot(1:10, wss, type="b", xlab="k", ylab="wss", main="elbow method")

#apply kmeans
set.seed(123)
model<-kmeans(scaled, centers = 3, nstart=25)
scaled_df$Cluster<-as.factor(model$cluster)
head(scaled_df)
df$Cluster <- as.factor(model$cluster)

#vizzzzz
# install.packages("ggplot2")
library(ggplot2)

ggplot(df, aes(x=Murder, y=Assault, color=Cluster)) + geom_point(size=3) + labs(title="kmeans")

model$centers
model$size

#vixzzzzz
boxplot(Murder ~ Cluster, data = scaled_df, col = "lightblue")

