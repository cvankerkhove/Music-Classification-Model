library(class)

run.knn <- function(df, K, labels, train.pct) {
  #This function runs KNN algorithm on a valid
  #music data frame input and outputs a list of an 
  #output table and the KNN classification error
  #using the validation set approach
  #Arg(s):
    #df: dataframe object from music dat
    #k: k paramter for knn algorithm
    #labels: genres of music to use in classification
    #train.pct: percent of data to use for training
  
  #filtering on selected labels
  df2 <- filter(df, (label %in% labels))
  df2$label <- as.factor(as.character(df2$label))
  #normalizing and standardizing data
  df.normal <- scale(df2[,!names(df2) %in% "label"])
  #training and testing index in data
  train_ind <- sample(1:nrow(df2), train.pct*nrow(df2))
  df.train <- df.normal[train_ind,]                    
  df.test <- df.normal[-train_ind,]
  #Store the outcome column separately
  train.label <- df2$label[train_ind]
  test.label <- df2$label[-train_ind]

  #Implement the KNN algorithm
  knn.pred=knn(df.train, df.test, train.label, k=K)
  #outputs
  t <- table(knn.pred, test.label)
  error <- 1-mean(knn.pred==test.label)
  
  return(list(t, error))
}

run.knn.cv <- function(df, K, labels, k.folds, n.iterations) {
  #This function runs KNN algorithm on a valid
  #music data frame input and outputs a list of an 
  #output table and the KNN classification error
  #using the validation set approach
  #Arg(s):
  #k: K paramter for knn algorithm
  #labels: genres of music to use in classification
  #k.folds: number of folds in the cross-validation
  #n.iterations: number of iterations of perfroming cross-validation to get error
  
  #filtering on selected labels
  df2 <- filter(df, (label %in% labels))
  df2$label <- as.factor(as.character(df2$label))
  #normalizing and standardizing data
  df.normal <- scale(df2[,!names(df2) %in% "label"])
  
  #list to store error values from each iteration
  errors <- c()
  for (itr in 1:n.iterations) {
    #classification error from each fold
    cv.error <- c()
    indices <- sample(nrow(df2), nrow(df2))
    for (i in 1:k.folds) {
      #testing indices for this fold
      slice1 <- (i-1)*(nrow(df2)/k.folds)+1
      slice2 <- i*(nrow(df2)/k.folds)
      test_ind <- indices[slice1:slice2]
      df.train <- df.normal[-test_ind,]                    
      df.test <- df.normal[test_ind,]
      #Store the outcome column separately
      train.label <- df2$label[-test_ind]
      test.label <- df2$label[test_ind]
      #Implement the KNN algorithm
      knn.pred=knn(df.train, df.test, train.label, k=K)
      #compute error
      error <- 1-mean(knn.pred==test.label)
      cv.error[i] <- error
    }
    errors[itr] <- mean(cv.error)
  }
  return(mean(errors))
}


run.random.forest.cv <- function(df, mtry, k.folds, n.iterations, single.genre=NULL) {
  #this function runs and creates and performs cross validation on a dataset 
  #and returns the overall cv classification error on this dataset withh specifications
  #for number of folds in the cv and number of iterations to perform
  #Arg(s):
    #df: a valid dataframe adapted from original music.df
    #mtry: the mtry paramater in the random forest
    #k.folds: number of folds in the cross validation test
    #n.iterations: number of times to perform the cross-validation
  
  #overall error for each trial
  errors <- c()
  #errors dataframe for each genre n x n.iterations matrix (n is number of genres)
  genre.errors <- NULL
  for (itr in 1:n.iterations) {
    #sampling new other data for each iteration
    if (!(is.null(single.genre))) {
      df.genre <- filter(df, !(label == single.genre)) %>%
        sample_n(100, replace = FALSE) %>%
        mutate(label = 'other') %>%
        bind_rows(filter(df.tree, label == 'rock'))
      df.genre$label <- as.factor(df.genre$label)
      df <- df.genre
    }
    #classification error from each fold
    cv.errors <- c()
    #classification error for each genre for each fold
    cv.genre.errors <- data.frame()
    #indices to split data into for cross validation
    indices <- sample(nrow(df), nrow(df))
    for (i in 1:k.folds) {
      #testing indices for this fold
      slice1 <- (i-1)*(nrow(df)/k.folds)+1
      slice2 <- i*(nrow(df)/k.folds)
      test_ind <- indices[slice1:slice2]
      df.train <- df[-test_ind,]                    
      df.test <- df[test_ind,]
      #Create a Random Forest
      rf <- randomForest(label~., data=df.train, mtry=mtry, importance=TRUE)
      pred <- predict(rf, newdata=df.test)
      genres <- rf$classes
      #overall classification error
      cv.errors[i] <- 1 - mean(pred == df.test$label)
      #classification error for each genre
      for (g in 1:length(genres)) {
        #true positive accuracy
        ind <- which(df.test$label == genres[g])
        error <- 1 - mean(df.test$label[ind] == pred[ind])
        #updating cv.genre.errors matrix
        cv.genre.errors[i, genres[g]] <- error
      }
    }
    #overall classification error
    errors[itr] <- mean(cv.errors)
    #classification error by genre
    if (is.null(genre.errors)) {
      genre.errors <- sapply(cv.genre.errors, FUN=mean)
    }
    else {
      genre.errors <- bind_rows(genre.errors, sapply(cv.genre.errors, FUN=mean))
    }
  }
  #final return values for n.iterations of n.fold cross validation
  overall.errors <- mean(errors)
  overall.genre.errors <- sapply(genre.errors, FUN = mean)
  return (list(overall.errors, overall.genre.errors))
}

