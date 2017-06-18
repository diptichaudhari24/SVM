# PROGRAMMING ASSIGNMENT 2- CECS 551
# BY DIPTI CHAUDHARI

# LIBRARIES

library(readr)

# EXERCISE 1

simple_learner<-function(df)
{
  colnm <-c()
  cols = paste("col", 1:(ncol(df)-1))
  for (i in 1:(ncol(df)-1))
  {
    colnm[i]=cols[i]
  }
  colnm[length(colnm)+1]= "classifier"
  
  # RENAME COLS
  colnames(df)<-colnm
  #View(df)
  df.lev <- unique(df$classifier)
  
  # SETTING INDEX OF POSITIVE AND NEGATIVE POINTS
  if(df.lev[1]==1 & df.lev[2]==-1)
  {
    plus_ind = 1
    minus_ind = 2
  }
  else
  {
    plus_ind = 2
    minus_ind = 1
  }
  
  # CREATING TWO DIFFERENT DATA FRAMES 
  
  # df_cplus : DATA FRAME WITH POSITIVE POINTS
  df_cplus<-subset(df,classifier==df.lev[plus_ind])
  #View(df_cplus)
  
  # df_cminus : DATA FRAME WITH NEGATIVE POINTS
  df_cminus<-subset(df,classifier==df.lev[minus_ind])
  #View(df_cminus)
  
  # COMPUTING c_plus & c_minus 
  c_plus<-vector(mode = "double",length =ncol(df_cplus)-1 )
  c_minus<-vector(mode = "double",length =ncol(df_cminus)-1 )
  c_plus <- colMeans(df_cplus,1)
  c_minus <- colMeans(df_cminus,1)
  cat('\nC plus - ',c_plus[1: length(c_plus)-1])
  cat('\nC minus - ',c_minus[1: length(c_minus)-1])
  
  # COMPUTING W AND B 
  w <- c_plus - c_minus
  c <- (c_plus + c_minus)/2
  cat('\nW vector-' ,w[1:length(w)-1])
  cat('\nC vector- ',c[1:length(c)-1])
  b= sum(w[1:length(w)-1]*c[1:length(c)-1])
  cat('\nb- ',b)
  
  w_b <- c(w[1:length(w)-1],b)
  return(w_b)
}

# EXERCISE 2

perceptron_learner<-function(df)
{
  
  w <-c(0,0,0,0,0)
  b = 0
  r = 1
  n = 1
  repeat
  {
    sign_match = 0
    for(i in 1:nrow(df))
    {
      x =as.numeric(as.vector(df[i,1:ncol(df)-1]))
      t = sum(w*x) - b
      y = df[i,ncol(df)]
      if (sign(t)==sign(y))
      {
        sign_match = sign_match + 1
      }
      else
      {
        w = w + (n*y)*as.numeric(x)
        b = b - (n*r*r*y)
      }
    }

    if(sign_match==nrow(df))
    {
      cat('\nW-',w)
      cat('\nb-', b)
      break
    }
  }
}

# EXERCISE 3

classify <-function(df,w,b)
{
  v <- vector(mode="character", length=nrow(df))
  for(i in 1:nrow(df))
  {
    # FETCHING X VECTOR
    x = as.numeric(as.vector(df[i,])) 
    # COMPUTING b
    b_1 = sum(w*x)
    if (b_1<=b)
    {
      v[i]= -1
    }
    else
    {
      v[i]= 1
    }
  }
  return(as.numeric(v))
}

# SOLUTIONS

# READING DATA SET
df<-read.csv("http://www.csulb.edu/~tebert/teaching/spring17/551/assignments/prog_assign2/Exercise-4.csv", header = TRUE)

# Call for simple_learner
w_b = simple_learner(df)

# Solution for simple_learner(dataframe)
# C plus -  5.811004 8.378948 -9.355491 12.73719 -10.42068
# C minus -  -6.061495 -11.98923 7.245171 -15.13889 11.87212
# W vector- 11.8725 20.36818 -16.60066 27.87608 -22.29279
# C vector-  -0.1252452 -1.805141 -1.05516 -1.200853 0.7257193
# b-  -70.39144

# Call for function classify
classify(df[,1:ncol(df)-1],w_b[1:length(w_b)-1],w_b[length(w_b)])

# Solution for classify(dataframe,w vector, b)
# [1]  1  1 -1 -1  1  1 -1  1  1 -1  1 -1  1 -1 -1 -1  1  1 -1  1 -1  1  1 -1  1
# [26] -1 -1  1  1  1  1 -1 -1  1 -1  1 -1  1  1  1 -1 -1  1  1 -1  1 -1  1 -1  1
# [51]  1 -1 -1 -1  1  1  1 -1 -1 -1 -1 -1 -1 -1 -1  1 -1  1  1 -1 -1 -1 -1  1  1
# [76]  1  1  1 -1 -1 -1 -1  1 -1 -1 -1  1  1  1  1 -1  1  1  1 -1  1  1  1 -1  1
# [101]  1  1 -1 -1 -1 -1 -1 -1 -1 -1  1 -1  1 -1 -1  1  1  1  1 -1  1  1  1 -1 -1
# [126]  1 -1 -1 -1  1  1  1  1  1 -1 -1 -1 -1  1 -1 -1  1 -1 -1  1  1  1  1  1  1
# [151]  1 -1 -1 -1  1 -1  1  1 -1 -1 -1 -1  1 -1 -1  1 -1  1 -1  1 -1  1  1  1  1
# [176]  1 -1  1 -1  1  1  1 -1  1  1  1  1  1  1 -1 -1  1  1  1 -1  1  1 -1 -1 -1
# [201]  1 -1  1  1  1  1 -1 -1 -1  1 -1  1 -1 -1 -1  1 -1  1  1 -1 -1 -1  1  1 -1
# [226] -1  1 -1  1  1  1 -1  1  1 -1  1  1 -1 -1  1  1  1 -1  1 -1  1 -1 -1  1  1
# [251]  1 -1  1 -1 -1 -1 -1  1  1  1 -1 -1 -1 -1 -1  1 -1 -1 -1  1 -1 -1 -1 -1  1
# [276] -1 -1  1  1 -1  1  1  1 -1 -1 -1 -1  1  1 -1  1 -1  1 -1  1 -1 -1  1  1  1
# [301]  1  1  1 -1 -1  1  1  1 -1 -1  1 -1 -1  1  1 -1  1 -1  1  1 -1  1 -1 -1 -1
# [326] -1 -1 -1  1  1 -1 -1 -1  1 -1  1 -1  1 -1 -1 -1 -1  1  1 -1  1 -1  1  1 -1
# [351] -1  1 -1  1 -1  1  1 -1 -1 -1 -1  1  1 -1 -1 -1 -1  1  1  1  1  1 -1  1 -1
# [376] -1  1  1 -1  1  1  1  1  1  1  1 -1  1 -1  1  1  1 -1 -1 -1  1 -1  1  1  1
# [401]  1  1  1  1 -1 -1 -1 -1  1 -1 -1 -1  1 -1  1  1  1 -1  1 -1  1 -1 -1 -1  1
# [426]  1  1 -1  1 -1  1  1 -1  1  1  1  1  1  1  1 -1 -1 -1 -1  1 -1 -1 -1 -1 -1
# [451] -1  1  1  1 -1  1 -1  1  1 -1  1 -1 -1 -1  1  1  1 -1  1  1 -1  1 -1  1 -1
# [476] -1  1 -1 -1  1 -1 -1 -1 -1 -1 -1 -1 -1  1  1  1  1 -1  1  1 -1 -1 -1  1 -1


# EXERCISE 5
# Call for function perceptron_learner
w_b = perceptron_learner(df)

# Solution for perceptron_learner(dataframe)
# W- 409.2977 757.0583 -722.513 908.0482 -929.8089
# b- -278

