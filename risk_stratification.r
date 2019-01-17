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

diabetic_data <- read.csv("diabetic_data.csv" , stringsAsFactors = F)
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

#Finding the mode of the Dataframe column

mode_age <- function(df , col)
{
  unique_val <- unique(df[[col]])
  unique_val[which.max(tabulate(match(df[[col]] , unique_val)))]
  
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
# Large number of missing values in the column , even if imputed may lead to biased results ,It is better to removet the column
#payer Code and Medical Speciality are the redundant variables which does add any value 
# for identifying the risk factor of  thr patient 
#Assuming the '?' values in daig_2 and diag_3 as patients didnt undergone the
#secondary and tertiary diagnosis as they dont have other diseases and multiple diagnosis is not required 
#these '?' values can be imputed with NA 


summary(diabetic_data$race)


# check for uniqueness 

check_for_distinct(diabetic_data , 'encounter_id')  # All encounter Ids are unique 


glimpse(diabetic_data)



#========================= Data Cleaning====================  



#Weight Column can be removed as it has lot of missing values 
#Payer code and Medical Speciality can be removed 
#Glucose serum test result as it is mentioned as not important factor
#All the columns of drugs can be removed and we can consider the column change and diabetesMed

#Removing redundatn rows from Data which are not useful in model building 
col_indices <- c( 1,2,6 ,11,12,23,25:41 , 43:47)
diabetic_data <- diabetic_data[ , - col_indices]

#Replacing the '?' values in race and Diag 1 column 
#Replacing '?' values in race column 
diabetic_data$race[diabetic_data$race == '?' ] <- 'Other'

str(diabetic_data)


#Replaicng '?' values in diag_1 , diag_2 , diag_3 with 0 , Assuming patients dosent undergone secondary and tertiary diagnosis 


diabetic_data$diag_1[which(diabetic_data$diag_1 == '?') ]<- 0
diabetic_data$diag_2[which(diabetic_data$diag_2 == '?')] <- 0
diabetic_data$diag_3[which(diabetic_data$diag_3 == '?')] <- 0



#Changing the values ">8" and ">7" of variable "readmitted" to "High" and "Medium"
diabetic_data$A1Cresult[diabetic_data$A1Cresult == '>8'] <- "High"
diabetic_data$A1Cresult[diabetic_data$A1Cresult == '>7'] <- "Medium"



#Changing the values ">30" and "<30" of variable "readmitted" to "YES" 

diabetic_data$readmitted[diabetic_data$readmitted == '>30' ] <- 'Yes'
diabetic_data$readmitted[diabetic_data$readmitted == '<30' ] <- 'Yes'



#========================= DERIVED METRICS ==================================

# Creating derived metric 'comorbidity'
# comorbidity 0 with no diabetes and circulatory disease 
# comorbidity 1 with diabetes and  no circulatory disease
# comorbidity 2 with no diabetes, but with circulatory disease 
# comorbidity 3 with diabetes and circulatory disease 
#Higher the comorbidity , higher the risk of readmittin in the hospital

diabetic_temp <- dplyr::select(diabetic_data ,c(diag_1,diag_2,diag_3) )


diabetic_temp<-mutate(diabetic_temp ,   diabetic= ifelse
                               (diabetic_temp$diag_1 > 250.00 & diabetic_temp$diag_1 <= 250.99| 
                                diabetic_temp$diag_2 > 250.00 & diabetic_temp$diag_2 <= 250.99 |
                                diabetic_temp$diag_3 > 250.00 & diabetic_temp$diag_3 <= 250.99 ,1,0))

diabetic_temp<-mutate(diabetic_temp ,   Ciculatory= ifelse
                          (diabetic_temp$diag_1 >= 390.00 & diabetic_temp$diag_1 <= 459.00 | 
                          diabetic_temp$diag_2 >= 390.00 & diabetic_temp$diag_2 <= 459.00 |
                          diabetic_temp$diag_3 >= 390.00 & diabetic_temp$diag_3 <= 459.00 ,1,0))



diabetic_temp <- mutate(diabetic_temp , comorbidity=case_when
                            (diabetic_temp$diabetic == 0 & diabetic_temp$Ciculatory == 0 ~0,
                             diabetic_temp$diabetic == 1 & diabetic_temp$Ciculatory == 0 ~1,
                             diabetic_temp$diabetic == 0 & diabetic_temp$Ciculatory == 1 ~2,    
                             diabetic_temp$diabetic == 1 & diabetic_temp$Ciculatory == 1 ~3 ))


diabetic_data <- cbind(diabetic_data , diabetic_temp$comorbidity)
colnames(diabetic_data)[23] <- "comorbidity"



#======================== DATA VISUALIZATION ANALYSIS ========================
theme_set(theme_classic())

align_x_text  <- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#Number of days in hospital and the procedures , Lab procedures and medications done on patients

plot_grid(ggplot(diabetic_data, aes(x=factor(time_in_hospital),fill=factor(num_lab_procedures)))+ geom_bar(position = "fill"), 
          ggplot(diabetic_data, aes(x=factor(time_in_hospital),fill=factor(num_procedures)))+ geom_bar(position = "fill"),
          ggplot(diabetic_data, aes(x=factor(time_in_hospital),fill=factor(num_medications)))+ geom_bar(position = "fill"),
          align = "h")



#Number of emergency , out patient and in patient visits 

plot_grid(ggplot(diabetic_data, aes(x=number_outpatient))+ geom_bar(), 
          ggplot(diabetic_data, aes(x=number_emergency))+ geom_bar(),
          ggplot(diabetic_data, aes(x=number_inpatient))+ geom_bar())

# Number of readmissions with respective to insulin intake , diabetes medication and change in Medications

plot_grid(ggplot(diabetic_data, aes(x=factor(insulin),fill=factor(readmitted)))+ geom_bar(position = "fill", width = 0.5), 
          ggplot(diabetic_data, aes(x=factor(change),fill=factor(readmitted)))+ geom_bar(position = "fill" , width = 0.5),
          ggplot(diabetic_data, aes(x=factor(diabetesMed),fill=factor(readmitted)))+ geom_bar(position = "fill", width = 0.5),
          align = "h")


#Number of readmissions with respective to comorbidity value
ggplot(diabetic_data , aes(x=factor(comorbidity) , fill = factor(readmitted))) + geom_bar(position =  "fill" , width =0.5)


#===== CATEGORICAL AND NUMERIC TREATMENT FOR LOGISTIC REGRESSION PURPOSE =====

# Categorical variable treatment

# 2 level dummy variable treatment

diabetic_data <- mutate(diabetic_data , change = ifelse(diabetic_data$change == 'No' , 0 ,1 ))
diabetic_data <- mutate(diabetic_data , diabetesMed = ifelse(diabetic_data$diabetesMed == 'Yes' , 1 , 0))


# More than 2 level dummy variable treatment

#Dummy variable creation for race 
dummy_race <- data.frame(data.frame(model.matrix(~race , data = diabetic_data))[ ,-1])
diabetic_data<- cbind(diabetic_data[ , -1] , dummy_race)

#Dummy variable creation for gender 
dummy_gender <- data.frame(data.frame(model.matrix(~gender , data = diabetic_data))[ ,-1])
diabetic_data<- cbind(diabetic_data[ , -1] , dummy_gender)

#Dummy variable creation for A1Cresult 
dummy_A1cresult <- data.frame(data.frame(model.matrix(~A1Cresult , data = diabetic_data))[ ,-1])
diabetic_data<- cbind(diabetic_data[ , -16] , dummy_A1cresult)

#Dummy variable creation for insulin 
dummy_insulin <- data.frame(data.frame(model.matrix(~insulin , data = diabetic_data))[ ,-1])
diabetic_data<- cbind(diabetic_data[ , -16] , dummy_insulin)

#Dummy variable creation for age
dummy_age <- data.frame(data.frame(model.matrix(~age , data = diabetic_data))[ ,-1])
diabetic_data<- cbind(diabetic_data[ , -1] , dummy_age)

str(diabetic_temp)

diabetic_data$diag_1 <- as.factor(diabetic_data$diag_1)
diabetic_data$diag_1 <- levels(diabetic_data$diag_1)[diabetic_data$diag_1]
diabetic_data$diag_1 <- as.numeric(diabetic_data$diag_1)
diabetic_data$diag_1[is.na(diabetic_data$diag_1)] <- 0

diabetic_data$diag_2 <- as.factor(diabetic_data$diag_2)
diabetic_data$diag_2 <- levels(diabetic_data$diag_2)[diabetic_data$diag_2]
diabetic_data$diag_2 <- as.numeric(diabetic_data$diag_2)
diabetic_data$diag_2[is.na(diabetic_data$diag_2)] <- 0


diabetic_data$diag_3 <- as.factor(diabetic_data$diag_3)
diabetic_data$diag_3 <- levels(diabetic_data$diag_3)[diabetic_data$diag_3]
diabetic_data$diag_3 <- as.numeric(diabetic_data$diag_3)
diabetic_data$diag_3[is.na(diabetic_data$diag_3)] <- 0


check_for_Nas(diabetic_data)
check_for_missed(diabetic_data)
str(diabetic_data)




# Scale Numeric variable having high variability in numeric range

scale_cols <- c('num_medications','num_lab_procedures', 'diag_1' , 'diag_2' , 'diag_3')

diabetic_data[ , scale_cols] <- lapply(diabetic_data[ ,scale_cols], scale)

diabetic_data$readmitted <- as.factor(diabetic_data$readmitted)

str(diabetic_data)

#========================= CREATIING TRAINING AND TEST DATA ==================

# divde training data as 70% training and 30% testing data

final_data <- diabetic_data

# initiallize prime number as seed.
set.seed(100)

trainindices = sample(1:nrow(final_data), 0.7 * nrow(final_data))
train = final_data[trainindices,]
test = final_data[-trainindices,]

#========================= MODEL BUILDING ====================================

#Lets create model with all variable
model_1 <- glm(readmitted ~ ., data = train,family = 'binomial')

#Null deviance: 90110  on 65285  degrees of freedom
#Residual deviance: 84815  on 65247  degrees of freedom
#(5950 observations deleted due to missingness)
#AIC: 84893
summary(model_1)

# Though AIC is not provide a test of model in the sense of testing null hypothesis
#  It is an estimator of relative quality of statistical model and hence can be used
# for chosing best among others , If the AIC value keeps decreasing then we consider
# it that AIC is improving.

# Lets use stepAIC function to reduce no of variables to use.
# In stepAIC function, we pass our first model i.e model_1 and
# direction is set as both, because in stepwise,  both the forward selection
# of variables and backward elimination of variables happen simultaneously

step <- stepAIC(model_1 , direction = "both")
summary(step)


model_3 <- glm(formula = readmitted ~ admission_type_id + discharge_disposition_id + admission_source_id + 
                 time_in_hospital + num_lab_procedures + num_procedures + 
                 number_outpatient + number_emergency + number_inpatient + 
                 diag_3 + number_diagnoses + change + diabetesMed + comorbidity + 
                 raceAsian + raceCaucasian + raceHispanic + raceOther + genderMale + 
                 A1CresultNorm + insulinNo + insulinSteady + insulinUp + age.10.20. + 
                 age.20.30. + age.30.40. + age.40.50. + age.50.60. + age.60.70. + 
                 age.70.80. + age.80.90. + age.90.100. , family = "binomial" , data = train)
#Null deviance: 98321  on 71235  degrees of freedom
#Residual deviance: 92606  on 71203  degrees of freedom
#AIC: 92672
summary(model_3)
sort(vif(model_3) , decreasing = T)
#All the Vif values are very low ,We can check for statistically insignificant variables and remove them one by one 


#Select the variables which are required for model building 

train <- dplyr::select(train ,admission_type_id , discharge_disposition_id,
                              admission_source_id,time_in_hospital, num_lab_procedures,
                              num_procedures, number_outpatient, number_emergency,number_inpatient,
                              diag_3 ,number_diagnoses,change, diabetesMed,comorbidity,
                              raceAsian,raceCaucasian,raceHispanic,raceOther,genderMale,
                              A1CresultNorm,insulinNo , insulinSteady , insulinUp , age.10.20. ,
                              age.20.30. , age.30.40. , age.40.50.,age.50.60. , age.60.70. ,
                              age.70.80. ,age.80.90. ,age.90.100.,readmitted)

# MODEL 4
# Here onwards we will keep removing one variable which is insignificant at a time
# To check which variable is insignificant we check its pvalue and vif value.
# from above we see that JobLevel.x5 has high p value.
# METHODOLOGY TO REMOVE ELEMENTS IS AS FOLLOW, 
# following methodology will be used while looking at the result and deciding which one to be removed
# 1. we need to check multicolinearity amongst variables using VIF and remove variables with high vif and low significance.
# 2. The variable with a high vif may be statistically significant(check no of *) in which case we will first have to check 
#    for other insignificant variables before removing the variable with a higher vif and lower p values.


train <- dplyr::select(train , -age.90.100.)
model_4 <- glm(readmitted~. , data= train , family = "binomial")
#Null deviance: 98321  on 71235  degrees of freedom
#Residual deviance: 92608  on 71204  degrees of freedom
#AIC: 92672
summary(model_4)
sort(vif(model_4) , decreasing = T)

#Model5
# raceCaucasian can be removed after analysing previosu result 

train<- dplyr::select(train , -raceCaucasian)
model_5 <- glm(readmitted~. , data= train , family = "binomial")
#Null deviance: 98321  on 71235  degrees of freedom
#Residual deviance: 92611  on 71205  degrees of freedom
#AIC: 92673
summary(model_5)
sort(vif(model_5) , decreasing = T)

#Model6
# change can be removed after analysing previosu result
train<- dplyr::select(train , -change)
model_6 <- glm(readmitted~. , data= train , family = "binomial")
#Null deviance: 98321  on 71235  degrees of freedom
#Residual deviance: 92613  on 71206  degrees of freedom
#AIC: 92673
summary(model_6)
sort(vif(model_6) , decreasing = T)

#Model7
# insulinup can be removed after analysing previosu result
train<- dplyr::select(train , -insulinUp)
model_7 <- glm(readmitted~. , data= train , family = "binomial")
#Null deviance: 98321  on 71235  degrees of freedom
#Residual deviance: 92616  on 71207  degrees of freedom
#AIC: 92674
#AIC: 92673
summary(model_7)
sort(vif(model_7) , decreasing = T)




#========================= MODEL BUILDING using Random Forest ====================================

train_forest <- diabetic_data 

#========================= CREATIING TRAINING AND TEST DATA ==================

set.seed(100)

trainindices1 <-sample(1:nrow(train_forest), 0.7 * nrow(train_forest)) 
train1 <- train_forest[trainindices1, ]
test1 <- train_forest[-trainindices1 , ]

data.rf <- randomForest(readmitted~., data = train1 , proximity = F ,do.trace = T , mtry=5 ,na.action = na.omit )
data.rf
testpred <- predict(data.rf , newdata = test1 [ ,-24])
test1$pred <- testpred
caret::confusionMatrix(test1$readmitted , test1$pred , positive = "NO") 


summary(testpred)


