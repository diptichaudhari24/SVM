# NAME: DIPTI CHAUDHARI
# PROGAMMING ASSIGNMENT 1 (CECS 551)

# EXERCISE 1

# READ DATA SET TO R ENVIRONMENT
# BY READING DATA AS INTEGER BY DEFAULT TAKES '?' AS NA
library(readr)
Assign1_Dataset <- read_csv("~/Documents/551/Assign1_Dataset.csv",
col_names = FALSE, col_types = cols(X1 = col_integer(),X2 = col_integer(), 
X3 = col_integer(),X4 = col_integer(), X5 = col_integer(),X6 = col_integer()))
View(Assign1_Dataset)
mammogram.frame <-Assign1_Dataset

# FILTERING ROWS WITH ALL NA VALUES
mammogram2.frame= mammogram.frame[complete.cases(mammogram.frame),]

# SUMMARY FUNCTION- BRIEFS ABOUT MODEL FITTING FUNCTIONS, HERE DATA SET
summary(mammogram2.frame)
#        X1               X2              X3              X4              X5              X6        
# Min.   : 0.000   Min.   :18.00   Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :0.0000  
# 1st Qu.: 4.000   1st Qu.:46.00   1st Qu.:2.000   1st Qu.:1.000   1st Qu.:3.000   1st Qu.:0.0000  
# Median : 4.000   Median :57.00   Median :3.000   Median :3.000   Median :3.000   Median :0.0000  
# Mean   : 4.394   Mean   :55.78   Mean   :2.782   Mean   :2.813   Mean   :2.916   Mean   :0.4855  
# 3rd Qu.: 5.000   3rd Qu.:66.00   3rd Qu.:4.000   3rd Qu.:4.000   3rd Qu.:3.000   3rd Qu.:1.0000  
# Max.   :55.000   Max.   :96.00   Max.   :4.000   Max.   :5.000   Max.   :4.000   Max.   :1.0000  

# CHANGING COL NAMES
colnames(mammogram2.frame)<- c('Birads','Age','Shape','Margin','Density','Severity')
library(e1071)

# GENERATE SVM MODEL OF DEGREE 1 (CLASSIFICATION, TAKES REGRESSION BY DEFAULT)
mammogram2model = svm (Severity~.,data=mammogram2.frame, kernel='linear',type='C-classification')

# PERCENT OF DATA POINTS CLASSIFIED BY MODEL
sum(mammogram2.frame$Severity== fitted(mammogram2model))/nrow(mammogram2.frame)* 100
# 82.77108

# GENERATE SVM MODEL OF DEGREE 2
mammogram2polymodel = svm (Severity~.,data=mammogram2.frame, kernel='polynomial',degree=2,type='C-classification')

# PERCENT OF DATA POINTS CLASSIFIED BY MODEL
sum(mammogram2.frame$Severity== fitted(mammogram2polymodel))/nrow(mammogram2.frame)*100
# 65.66265

# EXERCISE 2
# READ DATA SET TO R ENVIRONMENT
# BY READING DATA AS INTEGER BY DEFAULT TAKES '?' AS NA
library(readr)
Assign1_Dataset <- read_csv("~/Documents/551/Assign1_Dataset.csv",
    col_names = FALSE, col_types = cols(X1 = col_integer(),X2 = col_integer(), 
    X3 = col_integer(),X4 = col_integer(), X5 = col_integer(),X6 = col_integer()))
View(Assign1_Dataset)
mammogram.frame <-Assign1_Dataset

# REPLACE OCURRENCE OF NA BY -1
mammogram.frame[is.na(mammogram.frame)]<- -1

# SUMMARY FUNCTION- BRIEFS ABOUT MODEL FITTING FUNCTIONS, HERE DATA SET
summary(mammogram.frame)
#      X1               X2              X3               X4               X5               X6        
# Min.   :-1.000   Min.   :-1.00   Min.   :-1.000   Min.   :-1.000   Min.   :-1.000   Min.   :0.0000  
# 1st Qu.: 4.000   1st Qu.:45.00   1st Qu.: 1.000   1st Qu.: 1.000   1st Qu.: 3.000   1st Qu.:0.0000  
# Median : 4.000   Median :57.00   Median : 3.000   Median : 3.000   Median : 3.000   Median :0.0000  
# Mean   : 4.337   Mean   :55.19   Mean   : 2.601   Mean   : 2.607   Mean   : 2.601   Mean   :0.4631  
# 3rd Qu.: 5.000   3rd Qu.:66.00   3rd Qu.: 4.000   3rd Qu.: 4.000   3rd Qu.: 3.000   3rd Qu.:1.0000  
# Max.   :55.000   Max.   :96.00   Max.   : 4.000   Max.   : 5.000   Max.   : 4.000   Max.   :1.0000 

# CHANGING COL NAMES
colnames(mammogram.frame)<- c('Birads','Age','Shape','Margin','Density','Severity')

# GENERATE SVM MODEL OF DEGREE 1 (CLASSIFICATION, TAKES REGRESSION BY DEFAULT)
mammogrammodel = svm (Severity~.,data=mammogram.frame, kernel='linear',type='C-classification')

# PERCENT OF DATA POINTS CLASSIFIED BY MODEL
sum(mammogram.frame$Severity== fitted(mammogrammodel))/nrow(mammogram.frame)
# 0.8324662

# GENERATE SVM MODEL OF DEGREE 2
mammogrampolymodel = svm (Severity~.,data=mammogram.frame, kernel='polynomial',degree=2,type='C-classification')

# PERCENT OF DATA POINTS CLASSIFIED BY MODEL
sum(mammogram.frame$Severity== fitted(mammogrampolymodel))/nrow(mammogram.frame)
# 0.7585848