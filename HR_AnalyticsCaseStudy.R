# FOLLOWING SCRIPT CONTAIN SOULTION TO
#

# COURSE :- PREDICTIVE ANALYTICS I

# MODULE :- HR ANALYTICS CASE STUDY
#
# GROUP MEMBERS :-

#   * Sameer Surjikar - DDA1810132

#   * Ayushi Chouksey - DDA1810142

#   * Rahul Dondeti   - DDA1810143

#   * Priyanka Jindal - DDA1810205


#========================= ASSUMPTIONS =======================================



#  1. I assume that working directory is set throught Session-> Set Working Directory to the folder where all necessary files are kept

#  2. Data is unziped and is placed in the folder where session working directory is set
.


#========================= GET CLEAN SESSION  ================================



#Comment this line if you do not want your session to be cleaned out.

#rm(list=ls())



#========================= CHECK  AND INSTALL PACAKGES =======================



#Check if pacakges are installed or not and if not get them installed

#This is done so that If a package is not installed in evaluators environment

#script should not fail cost us marks.


#Required pacakge 
list
pkg <-
  c(
    "data.table",
    "tidyr",
    "plyr",
    "dplyr",
    "stringr",
    "tools",
    "ggplot2",
    "lubridate",
    "devtools",
    "grid",
    "gridExtra",
    "reshape2",
    "ggcorrplot",
    "treemapify",
    "scales",
    "corrplot",
    "MASS",
    "car",
    "e1071",
    "caret",
    "GGally",
    "cowplot",
    "caTools",
    "ROCR",
    ""
  )



#user defined function for checking and if not installed installing all the required pacakges.

check_and_install <- function(pkg) {
  if (!is.element(pkg, installed.packages()[, 1]))
    
install.packages(pkg, dependencies  = TRUE)
}



# installing packages

status <- lapply(pkg, check_and_install)






# loading libraries


status <- lapply(pkg, library, character.only = TRUE)



#========================= LOADING REQUIRED DATA =============================



#load all the data required


general_data <-
  read.csv("general_data.csv", stringsAsFactors = FALSE)

employee_survey_data <-
  read.csv("employee_survey_data.csv", stringsAsFactors = FALSE)

manager_survey_data <-
  read.csv("manager_survey_data.csv", stringsAsFactors = FALSE)

intime_data <- read.csv("in_time.csv", stringsAsFactors = FALSE)

outtime_data <- read.csv("out_time.csv", stringsAsFactors = FALSE)




#========================= USER DEFINED FUNCTIONS ============================



# HELPER FUNCTION


# check for blanks in dataframe columns


check_cols_for_Blanks <- function(df) 
{
  sapply(df, function(x)
    length(which(x == "")))
}



# check for NAs in dataframes


check_for_nas <- function(df) 
{
  sapply(df, function(x)
    sum(is.na(x)))
}



#Check is data is unique in column


check_for_uniqueness <- function(df , col) 
{
  length(unique(df[[col]])) == nrow(df)
}



# Remove ALL NA Columns

remove_all_na_columns <- function(df) 

{
  na_all <- apply(df[-1], 2, function(x)
    all(is.na(x)))
  
nacols <- names(na_all[na_all == TRUE])
 
 df <- df[!colnames(df) %in% nacols]
  return(df)
}



# multiple level dummy variable creation


multi_dummy_creation <- function(df, cols)
 {
  subset <- dplyr::select(df, cols)
  

# convert to factos
  

subset <- data.frame(sapply(subset, function(x)
    factor(x)))
 

 # convert dummeis
  

dummies <- data.frame(sapply(subset, function(x)
    data.frame(model.matrix(~ x - 1, data = subset), check.names = TRUE)[,-1]))
  

# add dummies back to dataframe and remove original columns
  

df <- cbind(dplyr::select(df,-cols), dummies)
  
return(df)
}


convert_columns_to_date_time <- function(df, ignoreColumns) 
{
  subset <- dplyr::select(df, -ignoreColumns)
  

subset <- lapply(subset, function(x)
    as_datetime(x))
  

df <- cbind(dplyr::select(df, ignoreColumns), subset)
  return(df)
}



# Helper function to find out the optimal probalility cutoff


cuttoff_matrix_builder <- function(cutoff) 

{
  predicted <- factor(ifelse(pred >= cutoff, "Yes", "No"))
  

conf <- caret::confusionMatrix(predicted, actual, positive = "Yes")
  

acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  

out <- t(as.matrix(c(sens, spec, acc))) 
  

colnames(out) <- c("sensitivity", "specificity", "accuracy")
  

return(out)
}



# helper function for plotting the lift 

chart

lift <- function(labels , predicted_prob,groups=10) 
{
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) 
predicted_prob <- as.integer(as.character(predicted_prob))
  
helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  
gaintable = helper %>% group_by(bucket)  %>%
 summarise_at(vars(labels ), 
funs(total = n(),
  totalresp=sum(., na.rm = TRUE))) %>%
    
    
mutate(Cumresp = cumsum(totalresp),
          
 Gain=Cumresp/sum(totalresp)*100,
           
Cumlift=Gain/(bucket*(100/groups))) 
 
 return(gaintable)
}





#========================= PRELIMINARILY DATA EXPLORATION ====================




# Check columns in dataframe

# It seems that employee id is the column column between all though intime and outime do not have employee is ,

# we can assum X as employee id. This will need the column to be renamed for join purposes.
names(general_data)
names(employee_survey_data)
names(manager_survey_data)
names(intime_data)
names(outtime_data)
#check for blanks
check_cols_for_Blanks(general_data)   
# No Blanks
check_cols_for_Blanks(employee_survey_data) # No Blanks
check_cols_for_Blanks(manager_survey_data)
# No Blanks
check_cols_for_Blanks(intime_data) # No Blanks
check_cols_for_Blanks(outtime_data) 
# No Blanks


# Check for NA's

# NA's Count

# NumCompaniesWorked = 19

# TotalWorkingYears = 9

check_for_nas(general_data)

# NA's Count

# EnvironmentSatisfaction = 25

# JobSatisfaction = 20

# WorkLifeBalance = 38

check_for_nas(employee_survey_data)

#No NA's

check_for_nas(manager_survey_data)

# Seems to have quite a lot NA's in all columns but there are columns which have 4110 (i.e no of records) , if all emp id's are unique then

# these entries seems to be off days/holidays days these columns should be removed.
check_for_nas(intime_data)

# Same obaservation as above for outtime as well.
check_for_nas(outtime_data)

# NA analysis also points out that most of the values can be imputed to mean .

# WHY ? as the we are planing to scale all variable there will not be any deviation in Z score due to imputation

# for that perticular variable in model but other variable for that row will still be able to contribute to model building.

# Other reason for chosing median is that generally most of the column which are NAs have strong relation to culture of the company

# and hence a general value imputation makes much more sense here.



# Check if Employee id is unique
check_for_uniqueness(general_data, 'EmployeeID')  
# All Employee IDs are unique
check_for_uniqueness(manager_survey_data, 'EmployeeID')  
# All Employee IDs are unique
check_for_uniqueness(employee_survey_data, 'EmployeeID')  
# All Employee IDs are unique
check_for_uniqueness(intime_data, 'X')  
# All Employee IDs are unique
check_for_uniqueness(outtime_data, 'X')  
# All Employee IDs are unique

# Check if there are different employee ids in different data set
all.equal(general_data$EmployeeID , manager_survey_data$EmployeeID)   
# All empid are same
all.equal(general_data$EmployeeID , employee_survey_data$EmployeeID)  # All empid are same
all.equal(general_data$EmployeeID , intime_data$X)  
# All empid are 
same
all.equal(general_data$EmployeeID , outtime_data$X)  

# All empid are same

# From the above analysis it is safe to join all dataset and we mostly will have data for employee ids


# Summary , glimpse of data and general observation.

# This data set have lot of categorical variable and most of them are not 2 levels

# proper dummy variable treatment will be needed to make it convert them and make it usable in the model

# The range of difference between values in variable is too high and scaling will be needed.

# for example monthlyincome vs distance from 
home.
glimpse(general_data)

# This seems to be manager survey data and can be useful in model building

# this data seems to be in good shape but may require NA treatment

glimpse(manager_survey_data)

# This data seems to represent employee point of view

# The data seems to be in good shape in but may require NA 
treatmen.
glimpse(employee_survey_data)

# This seems data seems to represent intime and outtime and In really bad shape.

# There is lot of information here which we will need to condence to make it useful for any modeling putpose.

# we just cannot give this data as it is to model building and get anywhere , derived metrics will be required on this 
data.
glimpse(intime_data)
glimpse(outtime_data)





#========================= DATA CLEANING =====================================



# Rename columns in intime and outime

setnames(intime_data , old = 'X' , new = 'EmployeeID')

setnames(outtime_data , old = 'X' , new = 'EmployeeID')



# Remove columns which has all NA's

intime_data <- remove_all_na_columns(intime_data)

outime_data <- remove_all_na_columns(outtime_data)



# Converting date time columns in intime and outime to datetime

intime_data <-
  convert_columns_to_date_time(intime_data, c("EmployeeID"))

outime_data <-
  convert_columns_to_date_time(outime_data, c("EmployeeID"))



# Impute median values for NAs as per analysis done in preliminary data analysis

# WHY ? as the we are planing to scale all numeric variable , there will not be any deviation in Z score due to imputation by median

# for that perticular variable in model but other variable for that row will still be able to contribute to model building.

# Other reason for chosing median is that generally most of the column which are NAs have strong relation to culture of the company

# and hence a general median value imputation makes much more sense here.


# for general_data Following columns NumCompaniesWorked , TotalWorkingYears

general_data$NumCompaniesWorked[which(is.na(general_data$NumCompaniesWorked))] <-
  mean(general_data$NumCompaniesWorked , na.rm = TRUE)

general_data$TotalWorkingYears[which(is.na(general_data$TotalWorkingYears))] <-
  mean(general_data$TotalWorkingYears , na.rm = TRUE)



# for employee_survey_data following columns EnvironmentSatisfaction,JobSatisfaction,WorkLifeBalance

employee_survey_data$EnvironmentSatisfaction[which(is.na(employee_survey_data$EnvironmentSatisfaction))] <-
  median(employee_survey_data$EnvironmentSatisfaction , na.rm = TRUE)

employee_survey_data$JobSatisfaction[which(is.na(employee_survey_data$JobSatisfaction))] <-
  median(employee_survey_data$JobSatisfaction , na.rm = TRUE)
employee_survey_data$WorkLifeBalance[which(is.na(employee_survey_data$WorkLifeBalance))] <-
  median(employee_survey_data$WorkLifeBalance , na.rm = TRUE)



#========================= DERIVED METRICS ===================================


# 

Average number of hours each employe worked in the give 
duration
work.hour <-
  data.frame(dplyr::select(outime_data, -c("EmployeeID")) - dplyr::select(intime_data, -c("EmployeeID")))

work.hour <-
  data.frame(lapply(work.hour, function(x)
    as.numeric(x)))

avg.work.hr <- round(rowMeans(work.hour, na.rm = TRUE), 2)

time_data <- data.frame(intime_data$EmployeeID, avg.work.hr)

colnames(time_data) <- c("EmployeeID", "avg.work.hr")



#over time

# Based on StandardHours it seems that 8 hour is normal working hours anything above that can be considered 
overtime
time_data <-
  mutate(time_data, over.time = ifelse(time_data$avg.work.hr > 8 , 1, 0))



# under time
#Based on StandardHours it seems that 8 hour is normal working hours anything less that can be considered 
undertime
time_data <-
  mutate(time_data, under.time = ifelse(time_data$avg.work.hr < 7 , 1, 0))




#======================== DATA VISUALIZATION ANALYSIS ========================

theme_set(theme_classic())

align_x_text  <- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))



# Employee survey data with respect to 

Attrition
emp_temp <- 
  merge(employee_survey_data,
        dplyr::select(general_data,c("EmployeeID","Attrition")),
        by = "EmployeeID",
        all = F)



# For work life balance is it is really poor that causes significant impact on Attrition but then 

# for 2 ,3 it seems to have contribute that much so this seems to get crutial if the worklife balance is really bad.

# but for average worklife balance seems to contribute less. But if too much good rating seems to increase in 

# number of people leaving organization may be too much free time also is not good , People like to be busy but 
# not too much or too less.

# For Environment Satisfaction simillar observation can be made. it seems very poor environment contribute to Attrition

# above average it seems to be constant contribution. 

# On other hand job staisfaction is crutial metrics and attrition rate goes down as Job satisfaction increases.

plot_grid(ggplot(emp_temp, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar(position = "fill"), 
         
ggplot(emp_temp, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar(position = "fill"),
 ggplot(emp_temp, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar(position = "fill"),
  align = "h")  



# Manager Survey 
data
mng_temp <- 
  merge(manager_survey_data,
  dplyr::select(general_data,c("EmployeeID","Attrition")),
  by = "EmployeeID",
   all = F)


# Performace Rating , It is curious that people who get good performace rating are leaving more but the increment is marginal 

# Job involment also seems to be mirroring worklife balance observations.

plot_grid(ggplot(mng_temp, aes(x=JobInvolvement,fill=Attrition))+ geom_bar(position = "fill"), 
ggplot(mng_temp, aes(x=as.factor(PerformanceRating),fill=Attrition))+ geom_bar(position = "fill"),
          align = "h") 



# Time 

Data
time_temp <- 
  merge(time_data,
        dplyr::select(general_data,c("EmployeeID","Attrition")),
        by = "EmployeeID",
        all = F)


# As expected people doing overtime are more likely to leave the company

# People who work less than the standard work time seems to less likely leave the company

# Avg.work.hr plot is really intersting , people who work on an average 7 to 8 hours do not leave company

# but people who work on an average works more than 8 hours will mostly leave the company. 

# peculiar case is the outliers of the people who work more on an average 10 to 11 hours and are not leaving the company

# we believe these are mostly highly motivated indiviuals and hence do not mind the exta hours. Now each company have these 

# individuals and hence though an outlier , we do not think these value needs to be treated. 


plot_grid( ggplot(time_temp, aes(x=as.factor(over.time),fill=Attrition))+ geom_bar(position = "fill"),
  ggplot(time_temp, aes(x=as.factor(under.time),fill=Attrition))+ geom_bar(position = "fill"),
    ggplot(time_temp, aes(x=Attrition,y=avg.work.hr, fill=Attrition))+ geom_boxplot(width=0.5)+ 
     coord_flip(), 
   align = "h")




#General data
# Age, We can see that younger peopl are more likely to leave the company

# Business Travel ,  People who travel a lot are more likely to leave the company

# Department , This seems to be funny but HR people are more likely to leave the company compared to other department

# Education level do not seems to have direct corelation with attrition rate.


plot_grid(ggplot(general_data, aes(x=Age,fill=Attrition))+ geom_bar(position = "fill"), 
 ggplot(general_data, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(position = "fill")+align_x_text,
  ggplot(general_data, aes(x=Department,fill=Attrition))+ geom_bar(position = "fill")+align_x_text,
  ggplot(general_data, aes(x=Education,fill=Attrition))+ geom_bar(position = "fill"),
          align = "h") 


# Education Field also model Department as HR seems to be more likely to leave company

# Job role , It seems that more the job is towards technical or research side more likely people may leave company.


plot_grid(ggplot(general_data, aes(x=EducationField,fill=Attrition))+ geom_bar(position = "fill")+align_x_text,
 ggplot(general_data, aes(x=Gender,fill=Attrition))+ geom_bar(position = "fill"),
    ggplot(general_data, aes(x=JobLevel,fill=Attrition))+ geom_bar(position = "fill"),
   ggplot(general_data, aes(x=JobRole,fill=Attrition))+ geom_bar(position = "fill")+align_x_text,
          align = "h") 


# Single people are more liekly to leave the company


plot_grid(ggplot(general_data, aes(x=MaritalStatus,fill=Attrition))+ geom_bar(position = "fill"), 
 ggplot(general_data, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar(position = "fill"),
     align = "h")



# Attrition distribution by various variables

# THERE ARE outliers but in any companies these outliers are natural and genuine and hence will not be treated. 


plot_grid(ggplot(general_data, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
 coord_flip(),
  ggplot(general_data, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
 coord_flip(),
   align = "v",nrow=1)

plot_grid(ggplot(general_data, aes(x=Attrition,y=NumCompaniesWorked, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip(),
          ggplot(general_data, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip(),
          align = "v",nrow=1)
           



plot_grid(ggplot(general_data, aes(x=Attrition,y=TrainingTimesLastYear, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip(),
          ggplot(general_data, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip(),
          align = "v",nrow=1)
plot_grid(ggplot(general_data, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip(),
          ggplot(general_data, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip(),
          align = "v",nrow=1)






#===== CATEGORICAL AND NUMERIC TREATMENT FOR LOGISTIC REGRESSION PURPOSE =====



# Categorical variable treatment


# 2 level dummy variable treatment

# For General data


general_data <-
  mutate(general_data , Attrition = ifelse(general_data$Attrition == 'Yes', 1, 0))

general_data <-
  mutate(general_data , Over18 = ifelse(general_data$Over18 == 'Y', 1, 0))

general_data <-
  mutate(general_data , Gender = ifelse(general_data$Gender == 'Male', 1, 0))




# More than 2 level dummy variable treatment

# For manager_survey_data


manager_survey_data <-
  multi_dummy_creation(manager_survey_data,
    c("JobInvolvement", "PerformanceRating"))
# For general_data

general_data <-
  multi_dummy_creation(
    general_data,
    c(
      "BusinessTravel",
      "Department",
      "Education",
      "EducationField",
      "JobLevel",
      "JobRole",
      "MaritalStatus",
      "StockOptionLevel"
    )
  )


# For employee_survey_data

# This needed to be done seperately using single variable at a time due

# error arising by "data.frame(..., check.names = FALSE) : row names were found from a short variable and have been discarded"

# and hence had to take each categorical variable seperately and create dummy variables for 

employee_survey_data.
employee_survey_data$EnvironmentSatisfaction <-
  as.factor(employee_survey_data$EnvironmentSatisfaction)

employee_survey_data$JobSatisfaction <-
  as.factor(employee_survey_data$JobSatisfaction)

employee_survey_data$WorkLifeBalance <-
  as.factor(employee_survey_data$WorkLifeBalance)

dummy <-
  data.frame(model.matrix( ~ EnvironmentSatisfaction, data = employee_survey_data))

dummy <- dummy[,-1]

employee_survey_data <-
  cbind(dplyr::select(employee_survey_data ,-EnvironmentSatisfaction),
        dummy)

dummy <-
  data.frame(model.matrix( ~ JobSatisfaction, data = employee_survey_data))
dummy <- dummy[,-1]

employee_survey_data <-
  cbind(dplyr::select(employee_survey_data ,-JobSatisfaction), dummy)

dummy <-
  data.frame(model.matrix( ~ WorkLifeBalance, data = employee_survey_data))

dummy <- dummy[,-1]

employee_survey_data <-
  cbind(dplyr::select(employee_survey_data ,-WorkLifeBalance), dummy)




# Scale Numeric variable having high variability in
 numeric range
scale_cols <-
  c(
    "Age",
    "DistanceFromHome",
    "MonthlyIncome",
    "NumCompaniesWorked",
    "PercentSalaryHike",
    "TotalWorkingYears",
    "TrainingTimesLastYear",
    "YearsAtCompany",
    "YearsSinceLastPromotion",
    "YearsWithCurrManager"
  )

general_data[, scale_cols] <-
  lapply(general_data[, scale_cols], scale)


# scale data in time data


time_data$avg.work.hr <- scale(time_data$avg.work.hr)




#======================== FINAL DATA SET =====================================




final_data <-
  merge(employee_survey_data,
        general_data,
        by = "EmployeeID",
        all = F)

final_data <-
  merge(final_data,
        manager_survey_data,
        by = "EmployeeID",
        all = F)

final_data <-
  merge(final_data, time_data, by = "EmployeeID", all = F)




#========================= CREATIING TRAINING AND TEST DATA ==================



# divde training data as 70% training and 30% testing data


# initiallize prime number as seed.

set.seed(100)


trainindices = sample(1:nrow(final_data), 0.7 * nrow(final_data))

train = final_data[trainindices,]
test = final_data[-trainindices,]




#========================= MODEL BUILDING ====================================



#Lets create model with all 

variable
model_1 <- glm(Attrition ~ ., data = train,family = 'binomial')


#Null deviance: 2747.7  on 3086  degrees of freedom

#Residual deviance: 1972.2  on 3028  degrees of freedom

#AIC: 2090.2
summary(model_1)


# while looking at summary there are NAs for some Varibale co-efficient

# This happens when there is not much variation in data. 

# we can ignore and remove these columns all together.

# Removing StandardHours , Over18 , EmployeeCount

train = dplyr::select(train ,-c("StandardHours","Over18","EmployeeCount"))



model_2 <- glm(Attrition ~ ., data = train,family = 'binomial')

#Null deviance: 2747.7  on 3086  degrees of freedom

#Residual deviance: 1972.2  on 3028  degrees of freedom

#AIC: 2090.2

# As you can see removing the element with NAs did not cause any change in result.  


summary(model_2)



# Though AIC is not provide a test of model in the sense of testing null hypothesis

#  It is an estimator of relative quality of statistical model and hence can be used

# for chosing best among others , If the AIC value keeps decreasing then we consider

# it that AIC is improving.

# Lets use stepAIC function to reduce no of variables to use.

# In stepAIC function, we pass our second model i.e model_2 and

# direction is set as both, because in stepwise,  both the forward selection

# of variables and backward elimination of variables happen simultaneously



step <- stepAIC(model_2, direction = "both")



#The last call of stepAIC has removed a lot variables which are insignificant let use those variables and remove the rest.


step


model_3 <- glm(formula = Attrition ~ EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
      EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
      JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
      WorkLifeBalance4 + Age + MonthlyIncome + NumCompaniesWorked + 
      TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + 
      YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
      BusinessTravel.xTravel_Rarely + Education.x5 + EducationField.xLife.Sciences + 
      EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
      EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManufacturing.Director + 
      JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xMarried + 
      MaritalStatus.xSingle + StockOptionLevel.x1 + JobInvolvement.x3 + 
      over.time, family = "binomial", data = train)



#Null deviance: 2747.7  on 3086  degrees of freedom

#Residual deviance: 1986.0  on 3052  degrees of freedom

#AIC: 2056
summary(model_3)


sort(vif(model_3), decreasing = TRUE)



# first lets select only variables which are useful for model building


train <-
  dplyr::select(
    train ,
    EnvironmentSatisfaction2 ,
    EnvironmentSatisfaction3 ,
    EnvironmentSatisfaction4 ,
    JobSatisfaction2 ,
    JobSatisfaction3 ,
    JobSatisfaction4 ,
    WorkLifeBalance2 ,
    WorkLifeBalance3 ,
    WorkLifeBalance4 ,
    Age ,
    MonthlyIncome ,
    NumCompaniesWorked ,
    TotalWorkingYears ,
    TrainingTimesLastYear ,
    YearsAtCompany ,
    YearsSinceLastPromotion ,
    YearsWithCurrManager ,
    BusinessTravel.xTravel_Frequently ,
    BusinessTravel.xTravel_Rarely ,
    Education.x5 ,
    EducationField.xLife.Sciences ,
    EducationField.xMarketing ,
    EducationField.xMedical ,
    EducationField.xOther ,
    EducationField.xTechnical.Degree ,
    JobLevel.x5 ,
    JobRole.xManufacturing.Director ,
    JobRole.xResearch.Director ,
    JobRole.xSales.Executive ,
    MaritalStatus.xMarried ,
    MaritalStatus.xSingle ,
    StockOptionLevel.x1 ,
    JobInvolvement.x3 ,
    over.time,
    Attrition
  )



# MODEL 4
# Here onwards we will keep removing one variable which is insignificant at a time

# To check which variable is insignificant we check its pvalue and vif value.

# from above we see that JobLevel.x5 has high p value.

# METHODOLOGY TO REMOVE ELEMENTS IS AS FOLLOW, 

# following methodology will be used while looking at the result and deciding which one to be removed

# 1. we need to check multicolinearity amongst variables using VIF and remove variables with high vif and low significance.

# 2. The variable with a high vif may be statistically significant(check no of *) in which case we will first have to check 

#    for other insignificant variables before removing the variable with a higher vif and lower p 

values.
train <- dplyr::select(train ,-JobLevel.x5)

model_4 <- glm(Attrition ~ .,  family = "binomial", data = train)


#Null deviance: 2747.7  on 3086  degrees of freedom

#Residual deviance: 1988.1  on 3053  degrees of freedom

#AIC: 2056.1


summary(model_4)

sort(vif(model_4), decreasing = TRUE)




#MODEL 5

# MonthlyIncome will be removed after analyzing previous result

#Null deviance: 2747.7  on 3086  degrees of freedom

#Residual deviance: 1990.4  on 3054  degrees of freedom

#AIC: 2056.4


train <- dplyr::select(train ,-MonthlyIncome)
model_5 <- glm(Attrition ~ .,  family = "binomial", data = train)

summary(model_5)

sort(vif(model_5), decreasing = TRUE)




#MODEL 6

# JobRole.xResearch.Director will be removed after analyzing previous result

#Null deviance: 2747.7  on 3086  degrees of freedom

#Residual deviance: 1993.0  on 3055  degrees of freedom

#AIC: 2057

train <- dplyr::select(train ,-JobRole.xResearch.Director)

model_6 <- glm(Attrition ~ .,  family = "binomial", data = train)

summary(model_6)

sort(vif(model_6), decreasing = TRUE)



#MODEL 7

# Education.x5  will be removed after analyzing previous result

# Null deviance: 2747.7  on 3086  degrees of freedom

# Residual deviance: 1996.6  on 3056  degrees of freedom

# AIC: 2058.6
train <- dplyr::select(train ,-Education.x5 )

model_7 <- glm(Attrition ~ .,  family = "binomial", data = train)

summary(model_7)

sort(vif(model_7), decreasing = TRUE)




#MODEL 8

# StockOptionLevel.x1   will be removed after analyzing previous result

# Null deviance: 2747.7  on 3086  degrees of freedom

# Residual deviance: 2000.0  on 3057  degrees of freedom

# AIC: 2060


train <- dplyr::select(train ,-StockOptionLevel.x1  )

model_8 <- glm(Attrition ~ .,  family = "binomial", data = train)

summary(model_8)
sort(vif(model_8), decreasing = TRUE)



#MODEL 9

# JobInvolvement.x3   will be removed after analyzing previous result

# Null deviance: 2747.7  on 3086  degrees of freedom

# Residual deviance: 2004.5  on 3058  degrees of freedom

# AIC: 2062.5
train <- dplyr::select(train ,-JobInvolvement.x3  )


model_9 <- glm(Attrition ~ .,  family = "binomial", data = train)

summary(model_9)

sort(vif(model_9), decreasing = TRUE)




#MODEL 10

# JobRole.xSales.Executive   will be removed after analyzing previous result
# Null deviance: 2747.7  on 3086  degrees of freedom

# Residual deviance: 2009.3  on 3059  degrees of freedom

# AIC: 2065.3
train <- dplyr::select(train ,-JobRole.xSales.Executive  )


model_10 <- glm(Attrition ~ .,  family = "binomial", data = train)

summary(model_10)

sort(vif(model_10), decreasing = TRUE)




# MODEL 11

# MaritalStatus.xMarried will be removed after analyzing previous result

# Null deviance: 2747.7  on 3086  degrees of freedom

# Residual deviance: 2014.9  on 3060  degrees of freedom

# AIC: 2068.9


train <- dplyr::select(train ,-MaritalStatus.xMarried  )
model_11 <- glm(Attrition ~ .,  family = "binomial", data = train)

summary(model_11)
sort(vif(model_11), decreasing = TRUE)




# MODEL 12

# YearsAtCompany will be removed after analyzing previous result

# Null deviance: 2747.7  on 3086  degrees of freedom

# Residual deviance: 2019.5  on 3061  degrees of freedom

# AIC: 2071.5

train <- dplyr::select(train ,-YearsAtCompany  )

model_12 <- glm(Attrition ~ .,  family = "binomial", data = train)

summary(model_12)

sort(vif(model_12), decreasing = TRUE)




# MODEL 13

# JobSatisfaction2 will be removed after analyzing previous result

#Null deviance: 2747.7  on 3086  degrees of freedom
#Residual deviance: 2026.9  on 3062  degrees of freedom

#AIC: 2076.9
train <- dplyr::select(train ,-JobSatisfaction2  )


model_13 <- glm(Attrition ~ .,  family = "binomial", data = train)

summary(model_13)
sort(vif(model_13), decreasing = TRUE)




# MODEL 14

# JobSatisfaction3 will be removed after analyzing previous result

#Null deviance: 2747.7  on 3086  degrees of freedom

#Residual deviance: 2030.6  on 3063  degrees of freedom

#AIC: 2078.6


train <- dplyr::select(train ,-JobSatisfaction3  )
model_14 <- glm(Attrition ~ .,  family = "binomial", data = train)

summary(model_14)
sort(vif(model_14), decreasing = TRUE)




# MODEL 15

# TrainingTimesLastYear will be removed after analyzing previous result

# Null deviance: 2747.7  on 3086  degrees of freedom

# Residual deviance: 2039.4  on 3064  degrees of freedom

# AIC: 2085.4


train <- dplyr::select(train ,-TrainingTimesLastYear  )

model_15 <- glm(Attrition ~ .,  family = "binomial", data = train)

summary(model_15)

sort(vif(model_15), decreasing = TRUE)




# MODEL 16

# EducationField.xLife.Sciences will be removed after analyzing previous result

# Null deviance: 2747.7  on 3086  degrees of freedom

# Residual deviance: 2062.0  on 3065  degrees of freedom

# AIC: 2106


train <- dplyr::select(train ,-EducationField.xLife.Sciences  )

model_16 <- glm(Attrition ~ .,  family = "binomial", data = train)

summary(model_16)

sort(vif(model_16), decreasing = TRUE)




# MODEL 17

# EducationField.xOther will be removed after analyzing previous result

# Null deviance: 2747.7  on 3086  degrees of freedom

# Residual deviance: 2062.8  on 3066  degrees of freedom

# AIC: 2104.8


train <- dplyr::select(train ,-EducationField.xOther  )

model_17 <- glm(Attrition ~ .,  family = "binomial", data = train)

summary(model_17)

sort(vif(model_17), decreasing = TRUE)



# MODEL 18

# EducationField.xMedical will be removed after analyzing previous result

# Null deviance: 2747.7  on 3086  degrees of freedom
# Residual deviance: 2064.7  on 3067  degrees of freedom

# AIC: 2104.7

train <- dplyr::select(train ,-EducationField.xMedical  )

model_18 <- glm(Attrition ~ .,  family = "binomial", data = train)

summary(model_18)

sort(vif(model_18), decreasing = TRUE)



# MODEL 19

# EducationField.xMarketing will be removed after analyzing previous result

#Null deviance: 2747.7  on 3086  degrees of freedom

#Residual deviance: 2066.3  on 3068  degrees of freedom

#AIC: 2104.3


train <- dplyr::select(train ,-EducationField.xMarketing  )

model_19 <- glm(Attrition ~ .,  family = "binomial", data = train)

summary(model_19)

sort(vif(model_19), decreasing = TRUE)




# MODEL 20

# EducationField.xTechnical.Degree will be removed after analyzing previous result

#Null deviance: 2747.7  on 3086  degrees of freedom

#Residual deviance: 2070.4  on 3069  degrees of freedom

#AIC: 2106.4


train <- dplyr::select(train ,-EducationField.xTechnical.Degree  )

model_20 <- glm(Attrition ~ .,  family = "binomial", data = train)

summary(model_20)
sort(vif(model_20), decreasing = TRUE)







#========================= MODEL EVALUATION ====================================




pred <- predict(model_20, type="response",newdata=dplyr::select(test,-Attrition))


summary(pred)
#View(pred)

test$prod <- pred



# 50 % cut off
# With 50% cut off following are the result
# Accuracy : 0.8647

# Sensitivity : 0.32367        
 
# Specificity : 0.96505

#
#           Reference

#Prediction   No  Yes

#        No  1077  140

#       Yes   39   67



predicted <- factor(ifelse(pred >=0.50,"Yes","No"))

actual <- factor(ifelse(test$Attrition==1,"Yes","No"))


conf_matrix <- caret::confusionMatrix(predicted,actual,positive = "Yes")

conf_matrix



# 60 % cut off

# With 40% cut off following are the result

# Accuracy : 0.8617

# Sensitivity : 0.22705         

# Specificity : 0.97939
#
#           Reference


#Prediction   No  Yes

#        No  1093  160

#       Yes   23   47


predicted <- factor(ifelse(pred >=0.60,"Yes","No"))

actual <- factor(ifelse(test$Attrition==1,"Yes","No"))


conf_matrix <- caret::confusionMatrix(predicted,actual,positive = "Yes")

conf_matrix


# 40 % cut off

# With 40% cut off following are the result

# Accuracy : 0.8496
# Sensitivity : 0.44928         

# Specificity : 0.92384

#
#           Reference
#Prediction   No  Yes

#        No  1031  114

#       Yes   85   93


predicted <- factor(ifelse(pred >=0.40,"Yes","No"))

actual <- factor(ifelse(test$Attrition==1,"Yes","No"))
conf_matrix <- caret::confusionMatrix(predicted,actual,positive = "Yes")

conf_matrix




# Creating cutoff values 


# Summary of test probability

summary(pred)
s = seq(.01,.80,length=100)
OUT = matrix(0,100,3)
for(i in 1:100)
{
  OUT[i,] = cuttoff_matrix_builder(s[i])
} 




plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)

lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))




cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

cutoff


# Lets take 0.1695  as cut off

# With 16.95% cut off following are the result

# Accuracy : 0.7286
# Sensitivity : 0.7246         

# Specificity : 0.7294

#
#           Reference

#Prediction   No  Yes

#        No  814  57

#       Yes  302 150


predicted <- factor(ifelse(pred >=0.1695,"Yes","No"))

actual <- factor(ifelse(test$Attrition==1,"Yes","No"))


conf_matrix <- caret::confusionMatrix(predicted,actual,positive = "Yes")

conf_matrix




# KS-STATISTIC

# KS-Statistic tells how well your model discriminate between two classes.

# It gives you indicator where your model lies between random model and perfect model

# A good model is one for which KS statistic satisfy following CRITERIA

# 1. is equal to 40% or mode

# 2. most of the data lies in the top deciles (eg 80% in first first 5 decile)




test_cutoff <- ifelse(predicted=="Yes",1,0)

test_actual <- ifelse(actual=="Yes",1,0)


pred_object_test<- prediction(test_cutoff, test_actual)


performance_measures_test<- performance(pred_object_test, "tpr", "fpr")


ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])



#0.4540284  This staisfy the FIRST CRITERIA for good model as per KS STATISTIC. 
max(ks_table_test)


# LIFT & GAIN CHART 
# A tibble: 10 x 6

#bucket total totalresp Cumresp  Gain Cumlift

#<int> <int>     <dbl>   <dbl> <dbl>   <dbl>

#  1      1   133        76      76  36.7    3.67

#2      2   132        35     111  53.6    2.68

#3      3   132        31     142  68.6    2.29

#4      4   133        21     163  78.7    1.97

#5      5   132        12     175  84.5    1.69

#6      6   132         5     180  87.0    1.45

#7      7   133        11     191  92.3    1.32

#8      8   132         5     196  94.7    1.18

#9      9   132         5     201  97.1    1.08

#10     10   132         6     207 100      1   



decile = lift(test_actual, pred, groups = 10)

decile



# This statisfy the SECOND CRITERIA for good model as per KS STATISTIC


plot(x=decile$Gain,y=decile$bucket,type = 'o')

plot(x=decile$Cumlift,y=decile$bucket,type = 'o')


# ROC - REciever operating characteristics curve 

# The model is considered goo if curve rise steply

# other way of saing is that model has higher area under curve

# AUC :-  0.7995905 indicating high 
accuracy.
roc = ROCR::prediction(pred, test$Attrition)

roc_perf = performance(roc, "tpr", "fpr")

plot(roc_perf, colorize=TRUE)

auc<-as.numeric(performance(roc, "auc")@y.values)
auc



#========================= RESULT  ===========================================



####### Variable that seems to be influential as per Data visualization are as follow


# Work Life Balance

# Environment Satisfaction

# job staisfaction

# overtime

# Business Travel

# Marital Status

# Job role

# Years at company




####### Following is the final predictor as per selected model i.e model_20


# Deviance Residuals: 

#   Min       1Q   Median       3Q      Max  

# -1.8066  -0.5432  -0.3327  -0.1564   3.8449  

# 
# Coefficients:

#   Estimate Std. Error z value Pr(>|z|)   

# (Intercept)                       -1.97082    0.33522  -5.879 4.12e-09 
***
#   EnvironmentSatisfaction2          -0.68988    0.17009  -4.056 4.99e-05 
***
#   EnvironmentSatisfaction3          -0.85205    0.15641  -5.448 5.11e-08 
***
#   EnvironmentSatisfaction4          -1.26424    0.16342  -7.736 1.03e-14 
***
#   JobSatisfaction4                  -0.91393    0.13597  -6.722 1.80e-11 
***
#   WorkLifeBalance2                  -0.98463    0.22525  -4.371 1.23e-05 
***
#   WorkLifeBalance3                  -1.24633    0.20975  -5.942 2.81e-09 
***
#   WorkLifeBalance4                  -1.05519    0.26364  -4.002 6.27e-05 
***
#   Age                               -0.37251    0.08001  -4.656 3.22e-06 
***#   NumCompaniesWorked                 0.39858    0.05892   6.765 1.34e-11 
***
#   TotalWorkingYears                 -0.48341    0.10571  -4.573 4.81e-06 
***
#   YearsSinceLastPromotion            0.60819    0.07633   7.968 1.61e-15 
***
#   YearsWithCurrManager              -0.56840    0.08707  -6.528 6.67e-11 
***
#   BusinessTravel.xTravel_Frequently  1.82642    0.28497   6.409 1.46e-10
***
#   BusinessTravel.xTravel_Rarely      1.09241    0.26941   4.055 5.02e-05 
***
#   JobRole.xManufacturing.Director   -0.90722    0.22564  -4.021 5.80e-05 
***
#   MaritalStatus.xSingle              0.95262    0.11673   8.161 3.32e-16 
***
#   over.time                          1.74937    0.11864  14.746  < 2e-16 
***
#   
---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# 
# (Dispersion parameter for binomial family taken to be 1)

# 
# Null deviance: 2747.7  on 3086  degrees of freedom

# Residual deviance: 2070.4  on 3069  degrees of freedom

# AIC: 2106.4

# 
# Number of Fisher Scoring iterations: 

6

####### The final VIF scores for selected variable are as follow


# BusinessTravel.xTravel_Frequently     BusinessTravel.xTravel_Rarely                  WorkLifeBalance3                  WorkLifeBalance2 

# 4.813717                          4.794566                          3.449164                          3.052896 

# TotalWorkingYears                  WorkLifeBalance4           YearsSinceLastPromotion                               Age 

# 2.519211                          2.008231                          1.908953                          1.889298 

# YearsWithCurrManager          EnvironmentSatisfaction4          EnvironmentSatisfaction3          EnvironmentSatisfaction2 

# 1.813595                          1.624036                          1.618601                          1.525169 

# NumCompaniesWorked                         over.time             MaritalStatus.xSingle                  JobSatisfaction4 

# 1.260680                          1.112105                          1.054313                          1.041562 

# JobRole.xManufacturing.Director 
# 1.020732 





#######  The cut off selected is 0.1695 after analysis 


# With 16.95% cut off following are the result

# Accuracy : 0.7286

# Sensitivity : 0.7246         

# Specificity : 0.7294
#
#           Reference

#Prediction   No  Yes

#        No  814  57

#       Yes  302 150



#######  KS STATISTICS


#0.4540284  This staisfy the FIRST CRITERIA for good model as per KS STATISTIC. 


# LIFT & GAIN  :- This statisfy the SECOND CRITERIA for good model as per KS STATISTIC

# A tibble: 10 x 6

#bucket total totalresp Cumresp  Gain Cumlift

#<int> <int>     <dbl>   <dbl> <dbl>   <dbl>

#  1      1   133        76      76  36.7    3.67

#2      2   132        35     111  53.6    2.68

#3      3   132        31     142  68.6    2.29

#4      4   133        21     163  78.7    1.97

#5      5   132        12     175  84.5    1.69

#6      6   132         5     180  87.0    1.45

#7      7   133        11     191  92.3    1.32

#8      8   132         5     196  94.7    1.18

#9      9   132         5     201  97.1    1.08

#10     10   132         6     207 100      1  




#######  ROC - Reciever operating characteristics 
# AUC :-  0.7995905 indicating high accuracy.


#######  INTERPRETING RESULT 

# Variable as per logistic regression model which contribute in attrition rate are as follow
 
# EnvironmentSatisfaction2         
  
# EnvironmentSatisfaction3   
  
# EnvironmentSatisfaction4       
  
# JobSatisfaction4                  
  
# WorkLifeBalance2                  
  
# WorkLifeBalance3                  
  
# WorkLifeBalance4                 
  
# Age                               
  
# NumCompaniesWorked                
  
# TotalWorkingYears                 
  
# YearsSinceLastPromotion           
  
# YearsWithCurrManager              
  
# BusinessTravel.xTravel_Frequently 
  
# BusinessTravel.xTravel_Rarely      
  
# JobRole.xManufacturing.Director   
  
# MaritalStatus.xSingle              
  
# over.time                          


# Variable having  positive co-efficent like 

# over.time , BusinessTravel.xTravel_Frequently , BusinessTravel.xTravel_Rarely, NumCompaniesWorked , YearsSinceLastPromotion ,MaritalStatus.xSingle

# Company should work towards reducing impact of these variables


# Variable which have negative co-efficent like 

# EnvironmentSatisfaction2 , EnvironmentSatisfaction3, EnvironmentSatisfaction4, JobSatisfaction4 , WorkLifeBalance2, WorkLifeBalance3 ,WorkLifeBalance4

# Age , TotalWorkingYears , YearsWithCurrManager ,JobRole.xManufacturing.Director  

# Company should invest more in these areas and try to increase the impact of these variables.




#######  RECOMMENDATIONS

# 1. Focus more on culture and focus on improving employee happyness index based on  EnvironmentSatisfaction ,WorkLifeBalance, JobSatisfaction

# 2. Discourage over time rather stop any overtime allownce to discourage people from over time.

# 3. Reduction in Business related travel

# 4. Look towards introduction of promotional cycle programs or provide apportunity for the same on regular intervals




#========================= END OF THE SCRIPT =================================



