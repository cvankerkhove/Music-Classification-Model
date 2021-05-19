setwd("~/Desktop/ORIE 4740/Final Project")
library(readr)
library(tibble)
library(dplyr)
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
errors
barplot(as.matrix(errors), las=2)


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


#binary classification for each genre
#for each genre
#take data points from genre (100)
#select 100 random data points from non-genre and label as 'other'
#run KNN 
genres <- levels(df$label)
error <- c()
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
  for (k in 2:length(k.values)) {
    out <- run.knn.cv(df.genre, k.values[k], levels(df.genre$label), 10, 10)
    k.list[k] <- out
  }
  print(k.list)
  best.k[genres[i]] <- which.min(k.list)
  error[genres[i]] <- min(k.list)
}

error
best.k


