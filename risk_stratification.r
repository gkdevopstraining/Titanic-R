# FOLLOWING SCRIPT CONTAIN SOULTION TO
#
# COURSE :- DOMAIN ELECTIVE
# MODULE :- Assignment - Payer Analystics 
# Submitted By :-
#   * Rahul Dondeti   - DDA1810143

#========================= ASSUMPTIONS =======================================

#  1. I assume that working directory is set throught Session-> Set Working Directory to the folder where all necessary files are kept

#========================= CHECK  AND INSTALL PACAKGES =======================

#Check if pacakges are installed or not and if not get them installed
#This is done so that If a package is not installed in evaluators environment
#Required pacakge list

pkg <-
  c(
    "tidyr",
    "plyr",
    "dplyr",
    "stringr",
    "tools",
    "ggplot2",
    "grid",
    "gridExtra",
    "ggcorrplot",
    "scales",
    "corrplot",
    "MASS",
    "car",
    "e1071",
    "caret",
    "cowplot",
    "caTools"
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
status

#========================= LOADING REQUIRED DATA =============================


#load all the data required

diabetic_data <- read.csv("diabetic_data.csv" , stringsAsFactors = T)
str(diabetic_data)


#========================= USER DEFINED FUNCTIONS ============================


# check for blanks in dataframe columns
check_for_blanks <-function(df)
{
  sapply(df, function(x) length(which(x=="")))
}

# check for NA's in dataframe columns
check_for_Nas <- function(df)
{
  sapply(df, function(x) sum(is.na(x)))
}


#check for '?' entires in dataframe column 

check_for_missed <- function(df)
{
  sapply(df, function(x) length(which(x == '?' )))
}

#Check is data is unique in column

check_for_distinct <- function(df , col)
{
  length(unique(df[[col]])) == nrow(df)
}





#========================= PRELIMINARILY DATA EXPLORATION ====================

# check for blanks
check_for_blanks(diabetic_data) # No blanks

#check for NA's

check_for_Nas(diabetic_data)  #No Nas

#check for '?'

check_for_missed(diabetic_data)

# '?' values in 
#race - 2273 weight - 98569 payer_code - 40256 medical_speciality - 49949
#diag_1 - 21 diag_2 - 358 diag_3 - 1423


# '?' values in race can be imputed with other 
# Weight has lot of values with '?' , The possible approcah can be grouping the age and gender and respective 
#values of weight and imputing the missing values with mode of that group interval 
#payer Code and Medical Speciality are the redundant variables which does add any value 
# for identifying the risk factor of  thr patient 
#Assuming the '?' values in daig_2 and diag_3 as patients didnt undergone the
#secondary and tertiary diagnosis as they dont have other diseases and multiple diagnosis is not required 
#these '?' values can be imputed with NA 


summary(diabetic_data$race)


# check for uniqueness 

check_for_distinct(diabetic_data , 'encounter_id')  # All encounter Ids are unique 


glimpse(diabetic_data)



#========================= PRELIMINARILY DATA EXPLORATION ====================  







#Rough exercise 


summary(diabetic_data$weight)

mode_age <- function(df , col)
{
  unique_val <- unique(df[[col]])
  unique_val[which.max(tabulate(match(df[[col]] , unique_val)))]
      
}


age_group_1 <- summarise(group_by(diabetic_data , age))

select1<- filter(dplyr::select(diabetic_data , 'age' , 'weight' ), weight != '?' )

group1 <- summarise(group_by(select1 , age) ,
                     weight = mode_age(select1 , 'weight'))

summarise(select1$weight , weight = mode_age(select1 , 'weight'))

str(select1)
test_1 <- aggregate(select1 , by = list(select1$age) )
mode_age(select1 , 'weight')


unique(select1$weight)





