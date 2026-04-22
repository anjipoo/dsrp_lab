# -----------------------------
# STEP 1: LOAD DATA (COMMON)
# -----------------------------
df <- read.csv("file.csv")

# -----------------------------
# STEP 2: EDA (COMMON)
# -----------------------------
head(df)
str(df)
summary(df)
colSums(is.na(df))

# optional
df <- na.omit(df)

# feature selection (depends on lab)
feat <- df[ required_columns ]

# -----------------------------
# STEP 3: BRANCH BASED ON LAB
# -----------------------------

# no model

# create new column
df$new_col <- ifelse(condition)

# visualize
plot(...)
boxplot(...)

# no split, no model

# basic stats
mean(x)
sd(x)

# hypothesis testing
t.test(x, mu=value)
t.test(feature ~ target, data=df)

# NO train/test

library(arules)

# convert to transactions
trans <- as(split(Item, TransactionID), "transactions")

# apply apriori
rules <- apriori(trans)

inspect(rules)

# -----------------------------
# SPLIT (COMMON)
# -----------------------------
set.seed(123)
train_idx <- sample(1:nrow(feat), 0.7*nrow(feat))

train <- feat[train_idx, ]
test  <- feat[-train_idx, ]

# binary classification

model <- glm(target ~ ., data=train, family="binomial")

pred <- predict(model, test, type="response")
pred_class <- ifelse(pred > 0.5, 1, 0)

# evaluation
table(pred_class, test$target)
accuracy <- mean(pred_class == test$target)

# continuous target

model <- lm(target ~ ., data=train)

pred <- predict(model, test)

# evaluation
rmse <- sqrt(mean((test$target - pred)^2))
mae  <- mean(abs(test$target - pred))

# classification

library(e1071)

# convert target to factor
df$target <- as.factor(df$target)

model <- naiveBayes(target ~ ., data=train)

pred <- predict(model, test)

# evaluation
table(pred, test$target)
accuracy <- mean(pred == test$target)

library(rpart)

model <- rpart(target ~ ., data=train, method="class")

pred <- predict(model, test, type="class")

# evaluation
table(pred, test$target)

# unsupervised

# select numeric columns only
data <- df[numeric_columns]

# scale
data_scaled <- scale(data)

# elbow (optional)
for (k in 1:10) {
  compute WSS
}

# model
model <- kmeans(data_scaled, centers=k)

# assign cluster
df$Cluster <- model$cluster

# visualize
plot(data_scaled[,1:2], col=model$cluster)

library(randomForest)

model <- randomForest(target ~ ., data=train, ntree=100)

pred <- predict(model, test)

# evaluation
# table(pred, test$target)
# accuracy <- mean(pred == test$target)

# #basic pipeline:
# EDA → SELECT FEATURES →

# IF (Apriori):
#     transactions → apriori → rules

# ELSE IF (KMeans):
#     numeric → scale → kmeans

# ELSE:
#     split →
#     model →
#     predict →
#     evaluate