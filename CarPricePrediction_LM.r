# Following script contains solution to
#
# COURSE :- Preedictive Analytics -I
# MODULE :- Assignment - Linear Regression 

#loading Required libraries 
library(stringr)
library(ggplot2)
library(car)
library(MASS)

# =============== CHECKPOINT 1 : DATA CLEANING 1 ======================

#loading data from CarPrice_Assignment.csv file to a var carprice 
car_price <- read.csv("CarPrice_Assignment.csv" , stringsAsFactors = F)
str(car_price)
#Checking for duplicate and NA values in the data set 

duplicate_val <- sum(duplicated(car_price$car_ID)) # there are no duplicate values present in dataset
na_val <- sum(is.na(car_price)) # No NA values present in data set

# Splitting the Carname to carModel and CarCompany

car_price$carcompany   <- str_split_fixed(car_price$CarName , " " ,2)

car_price$carcompany <-  sapply( car_price$carcompany ,FUN = function(x)  tolower(x[1]))

car_price$carcompany <- as.character(car_price$CarName)

class(car_price$carcompany)
summary(as.factor ( car_price$carcompany) )








