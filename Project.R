# ======================== Load Libraries ==========================================================

library(data.table)
library(car)
library(ggplot2)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(earth)
library(caret)
library(mltools)
library(dplyr)
library(knitr)
library(ROSE)
#explain the variables for those ambiguously named columns
##ATTRITION	Employee leaving the company (0=no, 1=yes)
#DAILY RATE	Numerical Value - Salary Level
#EDUCATION	Numerical Value ?? need to find out
#EMPLOYEE NUMBER	Numerical Value - EMPLOYEE ID
#ENVIROMENT SATISFACTION	Numerical Value - SATISFACTION WITH THE ENVIROMENT
#HOURLY RATE	Numerical Value - HOURLY SALARY
#JOB LEVEL	Numerical Value - LEVEL OF JOB
#MONTHY RATE	Numerical Value - MONTHY RATE how is it different from monthly income? need to find out
#NUMCOMPANIES WORKED	Numerical Value - NO. OF COMPANIES WORKED AT
#OVERTIME	(1=NO, 2=YES) is overtime paid? need to find out maybe? could be paid or unpaid overcome and that impacts the analysis
#PERCENT SALARY HIKE	Numerical Value - PERCENTAGE INCREASE IN SALARY annually or during promotion or compared to industry average?
#RELATIONS SATISFACTION	Numerical Value - RELATIONS SATISFACTION what relations?
# STANDARD HOURS	Numerical Value - STANDARD HOURS??? what is this
#STOCK OPTIONS LEVEL	Numerical Value - STOCK OPTIONS
#TOTAL WORKING YEARS	Numerical Value - TOTAL YEARS WORKED i think this includes other companies
#TRAINING TIMES LAST YEAR	Numerical Value - HOURS SPENT TRAINING
#WORK LIFE BALANCE	Numerical Value - TIME SPENT BEWTWEEN WORK AND OUTSIDE
#YEARS AT COMPANY	Numerical Value - TOTAL NUMBER OF YEARS AT THE COMPNAY
# YEARS IN CURRENT ROLE	Numerical Value -YEARS IN CURRENT ROLE does this include other company?
# YEARS SINCE LAST PROMOTION	Numerical Value - LAST PROMOTION
# YEARS WITH CURRENT MANAGER	Numerical Value - YEARS SPENT WITH CURRENT MANAGER

# =========================== Set WD =========================================================
setwd("C:/Users/gyong/OneDrive - Nanyang Technological University/NTU/NTU Studies/Year 2/NTU Y2S2/BC2407/Group Project")
set.seed(2025)

data<-fread("WA_Fn-UseC_-HR-Employee-Attrition.csv", stringsAsFactors = TRUE)
dim(data) #1470 rows, 35 columns

data_encoded<-fread("WA_Fn-UseC_-HR-Employee-Attrition.csv", stringsAsFactors = TRUE)


# =========================== Data Cleaning ===================================================

#identified columns stored as char, will converted them to factor
summary(data) 

# Checking for missing values
na_count <- colSums(is.na(data))
print(na_count) # No missing values found

# Removing unnecessary columns
cols_to_remove <- c("Over18", "EmployeeCount", "StandardHours")
data <- data[, !cols_to_remove, with=FALSE]

# Convert character columns to factor
char_cols <- names(data)[sapply(data, is.character)]
data[, (char_cols) := lapply(.SD, as.factor), .SDcols = char_cols]
summary(data)

# =========================== for Encoded Data ===================================================

cols_to_remove <- c("Over18", "EmployeeCount", "StandardHours")
data_encoded <- data_encoded[, !cols_to_remove, with = FALSE]

# Binary encoding for Yes/No or Male/Female
data_encoded[, Attrition := ifelse(Attrition == "Yes", 1, 0)]
data_encoded[, Gender := ifelse(Gender == "Male", 1, 0)]
data_encoded[, OverTime := ifelse(OverTime == "Yes", 1, 0)]

# One-hot encode selected categorical columns
data_encoded <- one_hot(as.data.table(data_encoded),
                        cols = c("BusinessTravel", "Department", "EducationField", 
                                 "JobRole", "MaritalStatus"),
                        sparsifyNAs = FALSE)

drop_dummies <- c(
  "Department_Research & Development",         # base for Department
  "EducationField_Life Sciences",              # base for EducationField
  "JobRole_Sales Executive",                   # base for JobRole
  "MaritalStatus_Married",                     # base for Marital Status
  "BusinessTravel_Travel_Rarely"               # base for BusinessTravel
)
data_encoded <- data_encoded[, !drop_dummies, with = FALSE]

# Convert any logical columns to numeric
logical_cols <- names(data_encoded)[sapply(data_encoded, is.logical)]
data_encoded[, (logical_cols) := lapply(.SD, as.integer), .SDcols = logical_cols]


# ========================== EDA ===============================================================

#identify all categorical and numeric data
categorical_vars <- names(data)[sapply(data, function(col) is.character(col) || is.factor(col))]
continuous_vars <- names(data)[sapply(data, is.numeric)]
# Split the data into categorical and continuous
cat_data <- data[, ..categorical_vars]
num_data <- data[, ..continuous_vars]

# Print the names of the variables
cat("Categorical Variables:\n", categorical_vars, "\n\n")
cat("Continuous Variables:\n", continuous_vars, "\n\n")

dim(cat_data) #9 columns of categorical variable
dim(num_data) #26 columns of numerical variable

##observation:
#can see that there is a lot more continuous variable involved.many things in work places are quantified?


# Display the first few rows of each dataset

head(cat_data) 
## attrition is a cat data that we want to predict
##over18 is always Y so can remove. but this is from viewing data directly, will prove with code later


head(num_data) 
## numerical data has some columns which are ordinal data rather than continuous.
##employee count is always 1, can be removed since not meaningful. but this is from viewing data directly, will prove with code later

length(unique(num_data[,"EmployeeCount"])) #only 1 unique value



#get dimensions to do plotting
cat.dim<- c(ceiling(sqrt(ncol(cat_data))), ceiling(ncol(cat_data)/ceiling(sqrt(ncol(cat_data)))))
#get dimensions to do plotting
num.dim<- c(ceiling(sqrt(ncol(num_data))),ceiling(ncol(num_data)/ceiling(sqrt(ncol(num_data)))))

#plot all categorical variables
par(mfrow=cat.dim)

for (i in 1:ncol(cat_data)) {
  barplot(table(cat_data[[i]]), main = colnames(cat_data)[i], xlab = colnames(cat_data)[i], ylab = "Frequency")
}
#from the bar plot, can see that over18 only has Y, proven and will be removed.


sum(data[,Over18!="Y"]) #this column only have 1 value since it is 0, proved it only has Y


#plot all numerical variables
par(mfrow=c(3,3)) #used 3x3 such that graphs can be bigger

for(i in 1:ncol(num_data)){
  boxplot(num_data[[i]], main= colnames(num_data)[i])
} 
#press side panel previous(under plot) to view all graphs
##can see that employeecount has only 1 value

##remove columns that has only 1 value
cat_data<-cat_data[,Over18:=NULL] #remove column
data<-data[,Over18:=NULL] #remove column

num_data<-num_data[,EmployeeCount:=NULL] #remove column
data<-data[,EmployeeCount:=NULL] #remove column


#plotting response against numerical var
for (i in 1:ncol(num_data)) {
  p <- ggplot(data, aes(x = Attrition, y = num_data[[i]])) +
    geom_violin(fill = "skyblue", trim = FALSE) +
    geom_boxplot(width = 0.1, fill = "white") +
    ggtitle(colnames(num_data)[i]) +
    ylab(colnames(num_data)[i]) +
    xlab("Attrition")
  print(p)
}
#press side panel previous(under plot) to view all graphs
## yes for attrition has lower age(median)
## will be added later


#plotting response against categorical var
for (i in 1:ncol(cat_data)) {
  var_name <- colnames(cat_data)[i]
  
  if (var_name != "Attrition") {  # Skip response itself
    p <- ggplot(data, aes_string(x = var_name, fill = "Attrition")) +
      geom_bar(position = "fill") +
      ylab("Proportion") +
      ggtitle(paste(var_name, "vs Attrition")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p)
  }
}
#press side panel previous(under plot) to view all graphs
##observations:
## 1. people who has biz travel more has higher attrition rate
## 2. R&D department has lower attrition, HR and sales has higher
## 3. HR education field, technical degree, marketing has high attrition 
## 4. gender no diff on attrition
## 5. for job role, sales rep highest, followed by HR and lab tech
## 6. single higher attrition than married , lowest is divorced. single may have lesser cost than married hence higher attrition, divorced why?
## 7. OT yes has higher attrition


##POSSIBLY IMPORTANT FOR ANALYSIS !!!!!!!!!!!!!!!!!!!!!!
##I find hourly, daily, and monthly rate to be poor variables for this analysis. Here's why:
#If this data relates to US employees, HR law requires hourly employees working over 40 hours in a week to be paid an overtime rate. While salary employees are paid a consistent amount regardless of the number of hours worked.
#Based on the distributions of these columns it seems hourly rates were 'calculated' for salary employees. However, this calculations is useless without the number of hours the employee is actually working.
#The rate columns provide a false representation and could therefore misrepresent it's relationship with variables such as job satisfaction.
#In my analysis I dropped the hourly, daily, and monthly rates, and focused on the monthly income instead.

# ========================== Feature Selection ==========================================

# Logistic Regression
lg_full <- glm(Attrition ~ ., data = data_encoded, family = "binomial")

# Check the summary of the model
summary(lg_full)

# Check for multicollinearity > 10
vif(lg_full)

# Perform backward stepwise selection based on AIC
lg_step <- step(lg_full, direction = "backward")

# Extract and sort absolute values of coefficients
coef_abs <- abs(coef(lg_step))
sorted_vars <- sort(coef_abs, decreasing = TRUE)
top_vars <- names(sorted_vars)

# Print top 15 features (excluding intercept)
top_vars[top_vars != "(Intercept)"][1:20]

# ======================== Selected Features ==============================================

selected_variables <- c("JobRole", "OverTime", "BusinessTravel", "EducationField", "MaritalStatus", 
                      "JobInvolvement", "EnvironmentSatisfaction", "JobSatisfaction", "Gender",
                      "WorkLifeBalance", "StockOptionLevel", "RelationshipSatisfaction", "TrainingTimesLastYear", 
                      "NumCompaniesWorked")

data_selected <- data[, c("Attrition", selected_variables), with = FALSE]

data_selected[, Attrition := ifelse(Attrition == "Yes", 1, 0)]

str(data_selected)

# ======================= Smote Sampling ==========================================================
set.seed(2025)

#Train Test Set
split <- sample.split(data_selected$Attrition, SplitRatio = 0.7)
train <- data_selected[split == TRUE]
test <- data_selected[split == FALSE]

train_rose <- ROSE(Attrition ~ ., data = train, seed = 2025)$data

#Check Class Balance
cat("Before ROSE:\n")
print(table(train$Attrition))

cat("After ROSE:\n")
print(table(train_rose$Attrition))

