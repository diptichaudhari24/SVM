# PROGRAMMING ASSIGNMENT 3
# DIPTI CHAUDHARI

#LIBRARIES
library(e1071)

# EXERCISE 1

# FUNCTION ACCEPTS SPLIT PARAMETER AND DATAFRAME
partition<-function(dataset,a)
{
  smp_size <- floor(a * nrow(dataset))
  # SET SEED TO MAKE PARTITION REPRODUCIBLE
  set.seed(123)
  train_ind <- sample(seq_len(nrow(dataset)), size = smp_size,replace = FALSE)
  train <- dataset[train_ind, ]
  test <- dataset[-train_ind, ]
  split <- list(train,test)
  return(split)
}

# CALL FOR EX 1
  # READING DATA SET
dataset <-read.csv("http://www.csulb.edu/~tebert/teaching/spring17/551/assignments/prog_assign2/Exercise-4.csv", header = TRUE)
split_t = partition(dataset,0.2) 

  # READING AS DATA FRAME FROM LIST
df_t1 = as.data.frame(split_t[1])
df_t2 = as.data.frame(split_t[2])

# EXERCISE 2
# BEST SVM ACCEPTING DATASET, SPLIT PARAMETER, DEGREE VECTOR AND COST VECTOR

best_svm<-function(df, alpha, degree, cost)
{
  split = partition(df,alpha)
  df1 = as.data.frame(split[1])
  df2 = as.data.frame(split[2])
  
  colnm <-c()
  cols = paste("col", 1:(ncol(df)-1))
  for (i in 1:(ncol(df)-1))
  {
    colnm[i]=cols[i]
  }
  colnm[ncol(df)]= "classifier"
  
  # RENAME COLS
  colnames(df1)<-colnm
  colnames(df2)<-colnm
  accuracy <- 0
  d_prime = -1
  c_prime = -1
  for (d in degree)
  {
    for (c in cost)
    {
      current_model = svm(classifier~., data = df1, kernel="polynomial", degree = d, type = "C-classification",cost=c,probability = TRUE)
      predictions <-predict(current_model,df2)
      label <- df2[,ncol(df2)]
      prediction_table <- table(predictions,label)
      temp <- 0
      for(i in 1:nrow(prediction_table))
      {
        temp <- temp + prediction_table[i,i]
      }
      current_accuracy <- (temp/nrow(df2))*100  
      if (current_accuracy > accuracy)
      {
        accuracy = current_accuracy
        d_prime = d
        c_prime = c
      }
    }
  }
  cat('\nBest fit: acc:',accuracy,' d:',d,' c:',c)
  best_fit <- as.array(d_prime,c_prime,accuracy)
  return(best_fit)
}

# FUNCTION CALL FOR EX 3
cardataset <-read.csv("/home/sangram/Documents/551/car.csv", header = FALSE)
a = 0.8
degree <- c(1, 2, 3, 4)
cost <- c(0.1, 1.0, 10.0, 100.0, 1000.0, 10000.0)
Number_of_call = 10
for (i in (1:10))
{
  best_fit <- best_svm(cardataset, a, degree, cost)
}

# OUTPUT
# Best fit: acc: 99.71098  d: 4  c: 10000
# Best fit: acc: 99.71098  d: 4  c: 10000
# Best fit: acc: 99.71098  d: 4  c: 10000
# Best fit: acc: 99.71098  d: 4  c: 10000
# Best fit: acc: 99.71098  d: 4  c: 10000
# Best fit: acc: 99.71098  d: 4  c: 10000
# Best fit: acc: 99.71098  d: 4  c: 10000
# Best fit: acc: 99.71098  d: 4  c: 10000
# Best fit: acc: 99.71098  d: 4  c: 10000
# Best fit: acc: 99.71098  d: 4  c: 10000


# EXERCISE 4 BEST_ACCURACY RETURNS DEGREE WIT GIVES 100% ACCURACY 
# FOR COST 100000

best_accuracy<-function(df,c)
{
  colnm <-c()
  cols = paste("col", 1:(ncol(df)-1))
  for (i in 1:(ncol(df)-1))
  {
    colnm[i]=cols[i]
  }
  colnm[ncol(df)]= "classifier"
  colnames(df) <- colnm
  accuracy <- 0
  d = 1 # NONLINEAR MODEL
  
  while(1)
  {
    current_model = svm(classifier~., data = df, kernel="polynomial", degree = d, type = "C-classification",cost=c,probability = TRUE)
    predictions <-predict(current_model,df)
    label <- df[,ncol(df)]
    current_accuracy = sum(predictions==label)/nrow(df)*100
    cat('\n For d =',d,' acc=',current_accuracy)
    if (current_accuracy == 100)
    {
      return(d)
    }
    d = d + 1
  }
}

# FUNCTION CALL FOR EX 4

#best_accuracy(cardataset,100000)

# OUTPUT 
# For d = 1  acc= 94.84954
# For d = 2  acc= 100

# EXERCISE 5
# BEST.SVM.CROSS 

best_svm_cross<- function(df, degree, cost, n)
{
  colnm <-c()
  cols = paste("col", 1:(ncol(df)-1))
  for (i in 1:(ncol(df)-1))
  {
    colnm[i]=cols[i]
  }
  colnm[ncol(df)]= "classifier"
  colnames(df) <- colnm
  accuracy <- 0
  d_prime = -1
  c_prime = -1
  for (d in degree)
  {
    for (c in cost)
    {
      current_model = svm(classifier~., data = df, kernel="polynomial", degree = d,type= "C-classification",cost=c, cross = n)
      current_accuracy = current_model$tot.accuracy
      if (current_accuracy > accuracy)
      {
        accuracy = current_accuracy
        d_prime = d
        c_prime = c
      }
    }
  }
  cat('\nBest fit: acc:',accuracy,' d:',d,' c:',c)
  best_fit <- list(d_prime,c_prime,accuracy)
  return(best_fit)
}

# EXERCISE 6

Breast_cancerdataset <-read.csv("/home/sangram/Documents/551/breast-cancer-wisconsin.csv", header = FALSE)
degree <- c(1, 2, 3, 4)
cost <- c(0.1, 1.0, 10.0, 100.0, 1000.0, 10000.0)
CV_folds <- 10

best_fit = best_svm_cross(Breast_cancerdataset,degree,cost,CV_folds)
d_value = best_fit[[1]]
c_value = best_fit[[1]]
# OUTPUT
# Best fit: acc: 96.42346  d: 4  c: 10000


# EXERCISE 7
bootstrap = function(df, model, p, n)
{
  accuracy_vector <- numeric(length = n)
  for(i in 1:n)
  {
    index <- sample(1:nrow(df), replace = TRUE)
    #print(index)
    training_set <- df[index, ]
    #print(nrow(training_set))
    pred <- predict(model, training_set)
    prediction_table <- table(pred, training_set$classifier)
    temp <- 0
    for(j in 1:nrow(prediction_table))
    {
      temp <- temp + prediction_table[j,j]
    }
    accuracy <- (temp/nrow(df))*100
    accuracy_vector[i] <- accuracy
  }
  sorted_accuracies<- sort(accuracy_vector)
  lowerbound <- sorted_accuracies[((1-p)/2.0)*n]
  upperbound <- sorted_accuracies[(p+((1-p)/2.0))*n]
  cat('\nLower_bound = ',lowerbound,', Upper_bound = ', upperbound)
  return(list(lowerbound,upperbound))
  
}


# EXERCISE 8
p<- 0.90
n<- 100
df <- Breast_cancerdataset
colnm <-c()
cols = paste("col", 1:(ncol(df)-1))
for (i in 1:(ncol(df)-1))
{
  colnm[i]=cols[i]
}
colnm[ncol(df)]= "classifier"
colnames(df) <- colnm
current_model <- svm(classifier~., data = df, kernel="polynomial", degree = d_value ,type = "C-classification",cost=c_value, cross = 10)
Conv_interval <- bootstrap(df,current_model,p,n)

# OUTPUT
# Lower_bound =  96.13734 , Upper_bound =  98.28326