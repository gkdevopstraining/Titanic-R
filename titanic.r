#========================= LOADING packages =============================
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm


#========================= LOADING Data =============================
train <- read.csv("train.csv", stringsAsFactors = F)
test  <- read.csv("test.csv", stringsAsFactors = F)
full  <- bind_rows(train, test) # bind training & test data

#========================= USER DEFINED FUNCTIONS ============================

#Check for blanks in Dataframe columns 

check_cols_for_blanks <- function(df)
{
  sapply(df, function(x)
    length(which(x == "")))
}


#Check for NA's in dataframe columns 

check_for_na <- function(df)
{
  
  sapply(df, function(x)
    sum(is.na(x)))
}


#Check for data unique in column 

check_for_uniqeness<- function(df ,col)
{
  length(unique(df[[col]])) == nrow(df)
  
}

#Replaing NA's rows of AGE with mean or median


check_for_mean <- function(df,col)
{
  age_mean <- mean(df[[col]] , na.rm = T)
  return(age_mean)
}


check_for_mode <- function(df ,col)
  
{
  unique_age <- unique(df[[col]])
  age_mode <- unique_age[which.max(tabulate(match(col ,unique_age)))]
  return(age_mode)
  
}


#========================= PRELIMINARILY DATA EXPLORATION ====================

check_cols_for_blanks(train)
#Blank in Columns of dataframe
#Cabin = 687  Embarked =2 


check_for_na(train)
#Nas in columns of dataframe
#Age =177 


#check all coulmn ids are unique 
check_for_uniqeness(train , 'Passengerid') # All coulmns are unique 

#Calc the mean of the age column 
check_for_mean(train , 'Age') #Mean =  29.6

#Calc the mode of age column 
check_for_mode(train , 'Age') #Mode =22


#Replace NA values of Age with Mean Value = 29.00
#Rounding of all values of age 

train$Age <- round(train$Age , 0)
train$Age[is.na(train$Age)] <- 29



#========================= Data Visualisation ====================


theme_set(theme_classic())
align_x_text <- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#plotting sex and survived
ggplot(train ,aes(x=factor(Sex) , fill=factor(Survived)))+
                  geom_bar(width=0.2 )+geom_text(stat="count" ,
                                                 aes(label=..count..) ,vjust=1.5, color="black"
                                                 , size=3.5)
#Plotting travelled class and survived

ggplot(train , aes(x=factor(Pclass) , fill= factor(Survived)))+geom_bar(width=0.5)+
               geom_text(stat = "count" , aes(label=..count..), vjust =1.0 , size=3.5)


#========================= Data Preparation for Modelling  ====================


train$Sex <- as.factor(train$Sex)
levels(train$Sex) <- c(0,1)
train$Sex <- as.numeric(levels(train$Sex))[train$Sex]


#Replacing Embarked blank values with 'S'


train$Embarked[train$Embarked == "" ] <- "S"
train$Embarked <- as.factor(train$Embarked)
#Converting embarked to dummy variable 

dummy_1 <- data.frame(model.matrix(~Embarked , data = train))
dummy_1 <- dummy_1[ ,-1]
train <- cbind(train[,-12] , dummy_1)

#=========================Model Building  ====================

#select Variables required for model building 

train_1 <- dplyr::select(train , PassengerId ,Survived , Pclass , Sex, Age,SibSp,Parch,
                         Fare,EmbarkedQ,EmbarkedQ)

Model_1 <- glm(Survived ~ ., data = train_1,family="binomial")
summary(Model_1)
