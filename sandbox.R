setwd("~/Desktop/ORIE 4740/Final Project")
library(readr)
library(tibble)
library(dplyr)
library(ggplot2)
source('Helpful_Functions.R')

####Data Cleaning and Manipulation###
music.dat <- read_csv("data.csv") %>%
  tibble()
#missing values
missing <- c()
classes <- c()
for (col.name in colnames(music.dat)) {
  missing[col.name] <- sum(is.na(music.dat[[col.name]]))
  classes[col.name] <- class(music.dat[[col.name]])
}
#Confirmed no missing data
sapply(music.dat, class)
#classes of each column 
df <- select(music.dat, -filename)
df$label <- as.factor(df$label)


####Logistic Regression####
#getting data to be fit for a single genre
df.blues <- filter(df, !(label == 'pop')) %>%
  sample_n(100, replace = FALSE) %>%
  mutate(label = 'other') %>%
  bind_rows(filter(df, label == 'pop'))
df.blues$label <- as.factor(df.blues$label)

train_ind <- sample(1:nrow(df.blues), 0.6*nrow(df.blues))
glm.music <- glm(label~., data = df.blues[train_ind,], family = binomial)
pred <- predict(glm.music, df.blues[-train_ind,], type = "response")
pred.bin <- as.numeric(pred >0.5)
actual.bin <- as.numeric(df.blues[-train_ind,]$label == 'other')

mean(pred.bin == actual.bin)







####K-Nearest-Neighbors####
#running multinomial classification for different values of K
#computing error through 10-fold CV 10 times
k.values <- seq(1,30,1)
errors <- c()
cv.errors <- c()
for (i in 1:length(k.values)) {
  #errors from validation set approach
  out <- run.knn(df, k.values[i], levels(df$label), 0.6)
  errors[i] <- out[[2]]
  #errors from 10-fold, 10 iteration CV
  out2 <- run.knn.cv(df, k.values[i], levels(df$label), 10, 10)
  cv.errors[i] <- out2
}
plot(k.values, errors)
plot(k.values, cv.errors)


#computing error classification error by each class
genres <- levels(df$label)
#dataframe to store error data for each genre across iterations
errors.df <- data.frame(matrix(ncol=10, nrow=0))
colnames(errors.df) <- genres
#number of iterations to average error over
n <- 20

for (itr in 1:n) {
  out <- run.knn(df, 5, levels(df$label), 0.6)
  d <- as.data.frame(out[[1]])
  d1 <- as.data.frame(out[[1]]) %>%
    group_by(test.label) %>%
    summarise(tot = sum(Freq)) %>%
    ungroup()
  
  for (i in 1:length(genres)){
    numer <- filter(d, d$knn.pred == genres[i] & d$test.label == genres[i])$Freq
    denom <- filter(d1, d1$test.label == genres[i])$tot
    errors.df[itr, genres[i]] <- 1 - (numer/denom)
  }
}

errors <- summarise_each(errors.df, list(.=mean))
##ggplot2 barplotting
#getting error data in a valid input to ggplot2
errors.num <- c()
for (i in 1:ncol(errors)) {
  errors.num[genres[i]] <- errors[[i]]
}

errors.df <- cbind(genre = rownames(as.data.frame(errors.num)), as.data.frame(errors.num))
rownames(errors.df) <- 1:nrow(errors.df)
#plotting
ggplot(errors.df, aes(genre, errors.num)) +
  geom_bar(stat = "identity")



#binary classification for each genre
#for each genre
#take data points from genre (100)
#select 100 random data points from non-genre and label as 'other'
#run KNN 
genres <- levels(df$label)
#list for recording errors of KNN
errors <- c()
#lists for finding best k-value for each genre
k.values <- seq(1,20,1)
best.k <- c()

for (i in 1:length(genres)) {
  print(genres[i])
  df.genre <- filter(df, !(label == genres[i])) %>%
    sample_n(100, replace = FALSE) %>%
    mutate(label = 'other') %>%
    bind_rows(filter(df, label == genres[i]))
  df.genre$label <- as.factor(df.genre$label)
  #out <- run.knn(df.genre, 3, levels(df.genre$label), 0.5)
  #error[genres[i]] <- out[[2]]
  #tables[[genres[i]]] <- out[[1]]
  #list for storing error values of each K 
  k.list <- c()
  for (k in 1:length(k.values)) {
    out <- run.knn.cv(df.genre, k.values[k], levels(df.genre$label), 10, 10)
    k.list[k] <- out
  }
  best.k[genres[i]] <- which.min(k.list)
  errors[genres[i]] <- min(k.list)
}

##ggplot2 barplotting
#getting error data in a valid input to ggplot2
errors.df <- cbind(genre = rownames(as.data.frame(errors)), as.data.frame(errors),
                        as.data.frame(best.k))
rownames(errors.df) <- 1:nrow(errors.df)
errors.df$best.k <- as.factor(errors.df$best.k)

ggplot(errors.df, aes(genre, errors, fill=best.k)) +
  geom_bar(stat = "identity")




####Decision Trees####
library(tree)
df.tree <- df
tree.music <- tree(label~., df.tree)
summary(tree.music)
plot(tree.music)
text(tree.music, pretty=0)
cv1 <- cv.tree(tree.music, FUN = prune.misclass)

#validation set
train <- sample(1:1000, 700, replace=FALSE)
tree.music <- tree(label~., df.tree, subset=train)
tree.pred <- predict(tree.music, df.tree[-train,], type="class")
t1 <- table(tree.pred, df.tree[-train,]$label)
#using best cv
pruned.tree.music <- prune.misclass(tree.music, best = cv1$size[which.min(cv1$dev)])
tree.pred2 <- predict(pruned.tree.music, df.tree[-train,], type='class')
t2 <- table(tree.pred2, df.tree[-train,]$label )
#Getting each genre classification error
genres <- levels(df.tree$label)
errors <- c()
pruned.errors <- c()
for (i in 1:length(genres)) {
  print(genres[i])
  num1 <- t1[genres[i], genres[i]]
  num2 <- t2[genres[i], genres[i]]
  denom1 <- 0
  denom2 <- 0
  for (i2 in 1:nrow(t1)) {
    denom1 <- denom1 + t1[genres[i], i2]
    denom2 <- denom2 + t2[genres[i], i2]
  }
  errors[genres[i]] <- 1 - (num1 / denom1)
  pruned.errors[genres[i]] <- 1 - (num2/ denom2)
}
errors
pruned.errors
mean(pruned.errors)

#plotting errors by genre
##ggplot2 barplotting
#getting error data in a valid input to ggplot2
errors.df <- cbind(genre = rownames(as.data.frame(errors)), as.data.frame(errors))
rownames(errors.df) <- 1:nrow(errors.df)

ggplot(errors.df, aes(genre, errors)) +
  geom_bar(stat = "identity")

##Binomial Classification
#single genre
#Comparing standard tree vs pruned tree prediction
df.genre <- filter(df.tree, !(label == 'rock')) %>%
  sample_n(100, replace = FALSE) %>%
  mutate(label = 'other') %>%
  bind_rows(filter(df, label == 'rock'))
df.genre$label <- as.factor(df.genre$label)
tree.1genre <- tree(label~., df.genre)
cv1 <- cv.tree(tree.1genre, FUN = prune.misclass)
#plottin deviation vs size of tree
plot(cv1$size, cv1$dev, type='b')  
#pruned tree prediction
pruned.tree <- prune.misclass(tree.1genre, best=cv1$size[which.min(cv1$dev)])
pruned.tree2 <- prune.misclass(tree.1genre, k = cv1$k[which.min(cv1$dev)])
pred1 <- predict(pruned.tree, newdata=df.genre, type='class')
pred2 <- predict(pruned.tree, newdata=df.genre, type='class')
table(pred1, df.genre$label)
table(pred2, df.genre$label)

#Iterating through all Genres
genres <- levels(df.tree$label)
errors <- c()
for (i in 1:length(genres)) {
  #modifying main dataset to be a balanced 200 rows each time
  df.genre <- filter(df.tree, !(label == genres[i])) %>%
    sample_n(100, replace = FALSE) %>%
    mutate(label = 'other') %>%
    bind_rows(filter(df.tree, label == genres[i]))
  df.genre$label <- as.factor(df.genre$label)
  #Creating a tree and pruned tree using training data
  train <- sample(1:nrow(df.genre), nrow(df.genre)*0.70)
  tree.1genre <- tree(label~., df.genre[train,])
  cv1 <- cv.tree(tree.1genre, FUN = prune.misclass)
  pruned.tree <- prune.misclass(tree.1genre, best=cv1$size[which.min(cv1$dev)])
  #predictions train
  pred <- predict(pruned.tree, newdata=df.genre[-train,], type='class')
  t <-table(pred, df.genre[-train,]$label)
  errors[genres[i]] <-  1 - mean(pred == df.genre[-train,]$label)
}



#####Random Forest####
library(randomForest)
##Use bagging and randomforest with these decision trees p=10
rf.tree1 <- randomForest(label~., data=df.tree, mtry=10, importance=TRUE)

set.seed(18167)
df.genre <- filter(df.tree, (label == 'rock')) %>%
  bind_rows(filter(df, label == 'blues'))
df.genre$label <- as.factor(as.character(df.genre$label))


df.genre <- filter(df.tree, !(label == 'rock')) %>%
  sample_n(100, replace = FALSE) %>%
  mutate(label = 'other') %>%
  bind_rows(filter(df.tree, label == 'rock'))
df.genre$label <- as.factor(df.genre$label)

#mtry parameter tuning
errors <- c()
genre.errors <- c()
mtry <- seq(1,15,1)
for (i in 1:length(mtry)) {
  print(i)
  rf.cv <- run.random.forest.cv(df, mtry = mtry[i], 10, 10, 'rock')
  errors[i] <- rf.cv[[1]]
  #genre.errors[i] <- rf.cv[[2]]
}
plot(mtry, errors)



