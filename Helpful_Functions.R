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

run.knn.cv <- function(df, K, labels, n.folds, n.iterations) {
  #This function runs KNN algorithm on a valid
  #music data frame input and outputs a list of an 
  #output table and the KNN classification error
  #using the validation set approach
  #Arg(s):
  #k: k paramter for knn algorithm
  #labels: genres of music to use in classification
  #n.folds: number of folds in the cross-validation
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
    for (i in 1:n.folds) {
      #testing indices for this fold
      slice1 <- (i-1)*(nrow(df2)/n.folds)+1
      slice2 <- i*(nrow(df2)/n.folds)
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
