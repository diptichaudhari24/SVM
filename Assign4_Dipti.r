# PROGRAMMING ASSIGNMENT 3
# DIPTI CHAUDHARI

#LIBRARIES
setwd("/home/sangram/Documents/551")
library(e1071)

# EXERCISE 1
df <- read.csv("abalone.data", header = TRUE)
colnames(df)[ncol(df)] <- "Class"
degree <- c(1,2,3)
cost <- c(100.0,10.0,1.0,0.1)
SolutionTable <- rbind(c("Degree", "Cost", "Accuracy"))
current_model <- svm(Class~., data = df)
dvalue<- 0
cvalue <- 0
max_Accuracy<-0
for(d in degree)
{
  for(c in cost)
  {
    current_model <- svm(Class~., data = df, kernel="polynomial", degree = d,type = "C-classification", cost=c,cross=5)
    SolutionTable <- rbind(SolutionTable, as.numeric(c(d,c,current_model$tot.accuracy)))
    if(current_model$tot.accuracy>max_Accuracy)
    {
      dvalue<- d
      cvalue<- c
      best_model<- current_model
      max_Accuracy <- current_model$tot.accuracy
    }
  }
}
# TABLE- MODELS WITH ALL COMBINATIONS 
print(SolutionTable)
# PERFORMING TESTING ON ENTIRE DATA
x_tensor <- subset(df, select=-Class)
y_tensor <- df$Class
predicts <-  as.integer(as.vector(predict(best_model,x_tensor)))
distance_tensor <- as.numeric(vector(length = nrow(df)))
Avg_Loss <- 0
for(data_point in 1:nrow(df))
{
  Avg_Loss <- Avg_Loss + abs(y_tensor[data_point]- predicts[data_point])
  difference_for_instance <- abs(y_tensor[data_point]- predicts[data_point])
  distance_tensor[data_point] = difference_for_instance
}
# PLOTTING GRAPH
hist(distance_tensor, breaks = max(as.integer(unique(df$Class)))+1,labels = TRUE, freq = TRUE, xlim = c(0,29))
cat("\nAverage Loss = ", Avg_Loss/nrow(df))

#################################################################################
# EXERCISE 2
# FINDSVM TAKES DATA FRAME COMPUTES DIFFERENT MODELS AND RETURN BEST MODEL*
findSVM <- function(df)
{
    degree <- c(1,2,3)
    cost <- c(0.1, 1.0, 10.0, 100.0)
    Max_Accuracy<-0
    d_best<- 0
    c_best <- 0
    folds = 5
    for(d in degree)
    {
      for(c in cost)
      {
        current_model <- svm(Class~., data = df, kernel="polynomial", degree = d,type = "C-classification", cost=c,cross=folds)
        if(current_model$tot.accuracy > Max_Accuracy)
        {
          d_best <- d
          c_best <- c
          Max_Accuracy <- current_model$tot.accuracy
        }
      }
    }
    model_best <- svm(Class~., data = df, kernel="polynomial", degree = d_best ,type = "C-classification", cost=c_best ,cross=folds)
    x_tensor <- subset(df, select=-Class)
    y_tensor <- df$Class
    predicts_tensor <-  as.integer(as.vector(predict(best_model,xVector)))
    correctly_classified <- 0
    for(data_point in 1:nrow(df))
    {
      if(yVector[data_point]-predicts_tensor[data_point]==0)
      {
        correctly_classified <- correctly_classified +1
      }
    }
    Training_Accuracy = (correctly_classified/nrow(df))*100
    return (list(dvalue,cvalue,best_model,model_best$tot.accuracy, Training_Accuracy))
}

# READING DATA
df <- read.csv("abalone.data", header = TRUE)
colnames(df)[ncol(df)] <- "Class"

SolutionTable <- rbind(c("Description","Size","Degree", "Cost", "CV Accuracy", "Training Accuracy"))


# CREATING DATA SETS

# CLASSIFIER 1
df_LE9_GE10 <- subset(df, Class<=9 | Class>=10)
df_LE9_GE10$Class[df_LE9_GE10$Class<=9] <- -1
df_LE9_GE10$Class[df_LE9_GE10$Class>=10] <- 1
# BEST SVM FOR df_LE9_GE10
svm_LE9_GE10 <- findSVM(df_LE9_GE10)
SolutionTable <- rbind(SolutionTable, c("LE9_GE10",nrow(df_LE9_GE10),svm_LE9_GE10[[1]],svm_LE9_GE10[[2]],svm_LE9_GE10[[4]],svm_LE9_GE10[[5]]))

# CLASSIFIER 2
df_LE7_E8E9 <- subset(df, Class<=7 | Class==8 | Class==9)
df_LE7_E8E9$Class[df_LE7_E8E9$Class<=7] <- -1
df_LE7_E8E9$Class[df_LE7_E8E9$Class>7] <- 1
# BEST SVM FOR df_LE7_E8E9
svm_LE7_E8E9 <- findSVM(df_LE7_E8E9)
SolutionTable <- rbind(SolutionTable, c("LE7_E8E9",nrow(df_LE7_E8E9),svm_LE7_E8E9[[1]],svm_LE7_E8E9[[2]],svm_LE7_E8E9[[4]],svm_LE7_E8E9[[5]]))

# CLASSIFIER 3
df_LE5_E6E7 <- subset(df, Class<=5 | Class==6 | Class==7)
df_LE5_E6E7$Class[df_LE5_E6E7$Class<=5] <- -1
df_LE5_E6E7$Class[df_LE5_E6E7$Class>5] <- 1
# BEST SVM FOR df_LE5_E6E7
svm_LE5_E6E7 <- findSVM(df_LE5_E6E7)
SolutionTable <- rbind(SolutionTable, c("LE9 vs GE10",nrow(df_LE5_E6E7),svm_LE5_E6E7[[1]],svm_LE5_E6E7[[2]],svm_LE5_E6E7[[4]],svm_LE5_E6E7[[5]]))

# CLASSIFIER 4
df_E8_E9 <- subset(df, Class==8 | Class==9)
# BEST SVM FOR df_E8_E9
svm_E8_E9 <- findSVM(df_E8_E9)
SolutionTable <- rbind(SolutionTable, c("E8_E9",nrow(df_E8_E9),svm_E8_E9[[1]],svm_E8_E9[[2]],svm_E8_E9[[4]],svm_E8_E9[[5]]))

# CLASSIFIER 5
df_E6_E7 <- subset(df, Class==6 | Class==7)
# BEST SVM FOR df_E6_E7
svm_E6_E7 <- findSVM(df_E6_E7)
SolutionTable <- rbind(SolutionTable, c("E8_E9",nrow(df_E6_E7),svm_E6_E7[[1]],svm_E6_E7[[2]],svm_E6_E7[[4]],svm_E6_E7[[5]]))


# CLASSIFIER 6
df_E10E11_GE12 <- subset(df, Class>=12 | Class==10 | Class==11)
df_E10E11_GE12$Class[df_E10E11_GE12$Class<12] <- -1
df_E10E11_GE12$Class[df_E10E11_GE12$Class>=12] <- 1
# BEST SVM FOR df_E10E11_GE12
svm_E10E11_GE12 <- findSVM(df_E10E11_GE12)
SolutionTable <- rbind(SolutionTable, c("E10E11_GE12",nrow(df_E10E11_GE12),svm_E10E11_GE12[[1]],svm_E10E11_GE12[[2]],svm_E10E11_GE12[[4]],svm_E10E11_GE12[[5]]))

# CLASSIFIER 7
df_E12E13_GE14 <- subset(df, Class>=14 | Class==12 | Class==13)
df_E12E13_GE14$Class[df_E12E13_GE14$Class<14] <- -1
df_E12E13_GE14$Class[df_E12E13_GE14$Class>=14] <- 1
# BEST SVM FOR df_E12E13_GE14
svm_E12E13_GE14 <- findSVM(df_E12E13_GE14)
SolutionTable <- rbind(SolutionTable, c("E12E13_GE14",nrow(df_E12E13_GE14),svm_E12E13_GE14[[1]],svm_E12E13_GE14[[2]],svm_E12E13_GE14[[4]],svm_E12E13_GE14[[5]]))

# CLASSIFIER 8
df_E10_E11 <- subset(df, Class==10 | Class==11)
# BEST SVM FOR df_E10_E11
svm_E10_E11 <- findSVM(df_E10_E11)
SolutionTable <- rbind(SolutionTable, c("E10_E11",nrow(df_E10_E11),svm_E10_E11[[1]],svm_E10_E11[[2]],svm_E10_E11[[4]],svm_E10_E11[[5]]))

# CLASSIFIER 9
df_E12_E13 <- subset(df, Class==12 | Class==13)
# BEST SVM FOR df_E12_E13
svm_E12_E13 <- findSVM(df_E12_E13)
SolutionTable <- rbind(SolutionTable, c("E12_E13",nrow(df_E12_E13),svm_E12_E13[[1]],svm_E12_E13[[2]],svm_E12_E13[[4]],svm_E12_E13[[5]]))

cat("\n")
print(SolutionTable)

#########################################################################################
# EXERCISE 3

x_tensor <- subset(df, select=-Class)
y_tensor <- df$Class

distance_tensor <- as.numeric(vector(length = nrow(df)))

for(i in 1:nrow(df))
{
  if(as.integer(as.vector(predict(svm_LE9_GE10[[3]],x_tensor[i, ])))==-1)
  {
    if(as.integer(as.vector(predict(svm_LE7_E8E9[[3]],x_tensor[i, ])))==-1)
    {
      if(as.integer(as.vector(predict(svm_LE5_E6E7[[3]],x_tensor[i, ])))==-1)
      {
        distance_tensor[i] = 5
      }
      else
      {
        if(as.integer(as.vector(predict(svm_E6_E7[[3]],x_tensor[i, ])))==6)
        {
          distance_tensor[i] = 6
        }
        else
        {
          distance_tensor[i] = 7
        }
      }
    }
    else
    {
      if(as.integer(as.vector(predict(svm_E8_E9[[3]],x_tensor[i, ])))==8)
      {
        distance_tensor[i] = 8
      }
      else
      {
        distance_tensor[i] = 9
      }
    }
  }
  else
  {
    if(as.integer(as.vector(predict(svm_E10E11_GE12[[3]],x_tensor[i, ])))==-1)
    {
      if(as.integer(as.vector(predict(svm_E10_E11[[3]],x_tensor[i, ])))==10)
      {
        distance_tensor[i] = 10
      }
      else
      {
        distance_tensor[i] = 11
      }
    }
    else
    {
      if(as.integer(as.vector(predict(svm_E12E13_GE14[[3]],x_tensor[i, ])))==-1)
      {
        if(as.integer(as.vector(predict(svm_E12_E13[[3]],x_tensor[i, ])))==12)
        {
          distance_tensor[i] = 12
        }
        else
        {
          distance_tensor[i] = 13
        }
      }
      else
      {
        distance_tensor[i] = 14
      }
    }
  }
}

Training_Accuracy <- (sum(distance_tensor==avg_diff)/nrow(df))*100
avg_diff <- 0
distance <- as.numeric(vector(length = nrow(df)))
for(i in 1:nrow(df))
{
  avg_diff <- avg_diff +  abs(distance_tensor[i]-y_tensor[i])
  distance[i] <- abs(distance_tensor[i]-y_tensor[i])
}

cat("\nTraining_Accuracy= ", Training_Accuracy)
cat("\nAverage Distance = ", avg_diff/nrow(df),"\n")

hist(distance, breaks = max(as.integer(unique(df$Class)))+1,labels = TRUE, freq = TRUE, xlim = c(0,29))

#################################################################################################33

df <- read.csv("Exercise-4.data", header = TRUE)
colnames(df)[ncol(df)] <- "Class"
plot(df)
folds= 5
eps <- c(1.75,1.5,1.0,0.1)
cost <- c(100.0,10.0,1.0,0.1)
Max_Accuracy <- 0
x_tensor <- subset(df, select=-Class)
y_tensor <- df$Class

c_best <- 0
e_best <- 0
Min_MSE <- 9999

SolutionTable <- rbind(c("Epsilon", "Cost","Mean_Squared_Error","MSE for entire data"))

for(e in eps)
{
  for(c in cost)
  {
    current_svm <- svm(Class~., data = df ,kernel="polynomial", degree = 2,type = "eps-regression", epsilon = e, cost = c,cross = folds)
    predicted <- as.numeric(as.vector(predict(sample_svm,xVector)))
    mse <- 0
    for(i in 1:nrow(df))
    {
      mse <- mse + (predicted[i]-y_tensor[i])^2
    }
    mse_df <- mse/nrow(df)
    MeanSquaredError <- current_svm$tot.MSE
    if(MeanSquaredError < Min_MSE)
    {
      Min_MSE <- meanSquaredError
      c_best <- c
      e_best <- e
    }
    SolutionTable <- rbind(SolutionTable, c(e,c, MeanSquaredError, mse_df))
  }
}

cat("\nBest parameters\nc= ", c_best," e= ",e_best,"\n")
print(SolutionTable)

###########################################################################################################
# EXERSICE 5

Best_svm_model <- svm(Class~., data = df ,kernel="polynomial", degree = 2,type = "eps-regression", epsilon = e_best, cost = c_best,cross = folds)
x_tensor<- seq(from=0, to = 10, by = 0.01)
Seq_df <- as.data.frame(seq(from=0, to = 10, by = 0.01))

#colnames(newdf) <- c("X1")
colnames(Seq_df) <- c("X")

y_tensor = predict(Best_svm_model,Seq_df)

plot(x_tensor,y_tensor,col="yellow")

points(df$X,df$Class,col="green")

##########################################################3
# EXERCISE 6


df <- read.csv("abalone.data", header = TRUE)
colnames(df)[ncol(df)] <- "Class"
degree <- c(1,2,3)
cost <- c(100.0,10.0,1.0,0.1)
eps <- c(0.1,1.0,1.5,1.75)


SolutionTable <- rbind(c("Degree", "Cost","Epsilon","tot.MSE"))
current_model <- svm(Class~., data = df)
d_best<- 0
c_best <- 0
Min_MSE <- 9999
for(d in degree)
{
  for(c in cost)
  {
    for(e in eps)
    {
      current_svm <- svm(Class~., data = df ,kernel="polynomial", degree = d,type = "eps-regression", epsilon = e, cost = c,cross = 5)
      SolutionTable <- rbind(SolutionTable, c(d,c,e,current_svm$tot.MSE))
      if(current_svm$tot.MSE<Min_MSE)
      {
        d_best <- d
        c_best <- c
        best_model<- current_model
        Min_MSE <- current_svm$tot.MSE
      }
    }
  }
}

print(SolutionTable)
x_tensor <- subset(df, select=-Class)
y_tensor <- df$Class

predicted_vector <-  as.integer(as.vector(predict(best_model,x_tensor)))

distance_tensor <- as.numeric(vector(length = nrow(df)))
Avg_Loss <- 0
for(i in 1:nrow(df))
{
  Avg_Loss <- Avg_Loss + abs(y_tensor[i]-predicted_vector[i])
  temp_value <- abs(y_tensor[i]-predicted_vector[i])
  distance_tensor[i] = temp_value
}

hist(distance_tensor, breaks = max(as.integer(unique(df$Class)))+1,labels = TRUE, freq = TRUE, xlim = c(0,29))
cat("\n Average Distance = ", Avg_Loss/nrow(df))

