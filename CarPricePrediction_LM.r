# Following script contains solution to
#
# COURSE :- Preedictive Analytics -I
# MODULE :- Assignment - Linear Regression 

#loading Required libraries 

library(tidyr)
library(ggplot2)
library(car)
library(MASS)
library(corrplot)

# =============== CHECKPOINT 1 : DATA CLEANING 1 ======================

#loading data from CarPrice_Assignment.csv file to a var carprice 
car_price <- read.csv("CarPrice_Assignment.csv")
str(car_price)
#Checking for duplicate and NA values in the data set 

duplicate_val <- sum(duplicated(car_price$car_ID)) # there are no duplicate values present in dataset
na_val <- sum(is.na(car_price)) # No NA values present in data set

# Splitting the Carname to carModel and CarCompany

car_price <- separate(car_price , CarName ,into=c("CarCompany" , "CarName"), sep =" " ,
                      extra = "merge",fill = "right")

# Converting the Carcompany col to lower case considering there is Case mismatch for same comapny 

car_price$CarCompany <- sapply(car_price$CarCompany , FUN = function(x)  tolower(x[1]))
summary(as.factor(car_price$CarCompany))

#Multiple spelling mistakes are identified in the Carcompany columns

car_price$CarCompany <- sapply(car_price$CarCompany, function(x)
                                     { gsub ("maxda" , "mazda" ,x) })

car_price$CarCompany <- sapply(car_price$CarCompany, function(x)
{ gsub ("porcshce" , "porsche" ,x) })

car_price$CarCompany <- sapply(car_price$CarCompany, function(x)
{ gsub ("toyouta" , "toyota" ,x) })

car_price$CarCompany <- sapply(car_price$CarCompany, function(x)
{ gsub ("vokswagen" , "volkswagen" ,x) })

str(car_price)


# Treating outliers in the car_price$wheelbase dataset

quantile(car_price$wheelbase , seq(0,1,0.01))

# Outlier is identified at 100% with increase capping the values with max value 115.544

car_price$wheelbase[which(car_price$wheelbase > 115.544)] <- 115.544

# Treating outliers in the car_price$carlength dataset

quantile(car_price$carlength ,seq(0,1,0.01))

# Outlier is identified at 95% with increase capping the carlength with max value 192.700

car_price$carlength[which(car_price$carlength > 192.700)] <- 192.700

# Treating outliers in the car_price$carwidth dataset
quantile(car_price$carwidth ,seq(0,1,0.01)) # No outliers identified in carwidth var

# Treating outliers in the car_price$carheight dataset
quantile(car_price$carheight ,seq(0,1,0.01)) # No outliers identified in carheight var

# Treating outliers in the car_price$curbweight dataset
quantile(car_price$curbweight , seq(0,1,0.01))

# Outliers is observerd on both sides at 0% and 93% 
# At 0% capping at next min value 1819  and max value 3292.48
car_price$curbweight[which(car_price$curbweight < 1819.72)] <- 1819.72
car_price$curbweight[which(car_price$curbweight > 3292.48)] <- 3292.48

# Treating outliers in the car_price$enginesize dataset
quantile(car_price$enginesize , seq(0,1,0.01))
# Outliers is observerd on both sides at 0% and 97% 
# At 0% capping at next min value 90.00  and max value 209.00
car_price$enginesize[which(car_price$enginesize < 90.00)] <- 90.00
car_price$enginesize[which(car_price$enginesize > 209.00)] <- 209.00

# Treating outliers in the car_price$boreratio dataset
quantile(car_price$boreratio , seq(0,1,0.01)) 
# Outliers is observerd on both sides at 0% and 100% 
# At 0% capping at next min value 2.9100  and max value 3.8000
car_price$boreratio[which(car_price$boreratio < 2.9100)] <- 2.9100
car_price$boreratio[which(car_price$boreratio > 3.8000)] <- 3.8000

# Treating outliers in the car_price$stroke dataset
quantile(car_price$stroke , seq(0,1,0.01)) 
# Outliers is observerd on both sides at 1% and 96% 
# At 1% capping at next min value 2.6400  and max value 3.6400
car_price$stroke[which(car_price$stroke < 2.6400)] <- 2.6400
car_price$stroke[which(car_price$stroke > 3.6400)] <- 3.6400

# Treating outliers in the car_price$compressionratio
quantile(car_price$compressionratio , seq(0,1,0.01))

# Outlier is identified at 90% with increase capping the carlength with max value 10.0000
car_price$compressionratio[which(car_price$compressionratio > 10.0000)] <- 10.0000

# Treating outliers in the car_price$horsepower
quantile(car_price$horsepower , seq(0,1,0.01))

# Outlier is identified at 94%  capping with max value 162.00
car_price$horsepower[which(car_price$horsepower > 162.00)] <- 162.00

# Treating outliers in the car_price$horsepower
quantile(car_price$peakrpm , seq(0,1,0.01)) # No outliers in the peakrpm Var

# Treating outliers in the car_price$citympg
quantile(car_price$citympg , seq(0,1,0.01)) 
# Outlier is identified at 98% capping with max value 38.00
car_price$citympg[which(car_price$citympg > 38.00)] <- 38.00

# Treating outliers in the car_price$highwaympg
quantile(car_price$highwaympg , seq(0,1,0.01)) #No outliers in the highwaympg


# =============== CHECKPOINT 2 : DATA Visualisation  ======================


#Plotting corelation matrix between different variables.

# Dropping the categorical variables from dataframe to plot corelation betweeen numeric variables 

car_index <- c(11:15,20:27)
car_numvars<- car_price[ , car_index]
car_cor1  <-  cor(car_numvars)
corrplot(car_cor1  )
corrplot(car_cor1, type="upper", order="hclust")

## Need to fine tune the corelation matrix 

# =============== CHECKPOINT 3 : EDA and Deriving New variables ======================


# Deriving the citympg/curbweight and highwaymph/curbweight

car_price$weightcmpg <- car_price$citympg/car_price$peakrpm























