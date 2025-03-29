library(data.table) 
library(car)
library(ggplot2)
library(caTools)
library(rpart)
library(rpart.plot)

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


setwd('/Users/xb/Desktop/Uni Notes/Y2S2/BC2407/BC2407 Course Materials/Project')
set.seed(2025)

data<-fread('/Users/xb/Desktop/Uni Notes/Y2S2/BC2407/BC2407 Course Materials/Project/WA_Fn-UseC_-HR-Employee-Attrition.csv')
dim(data) #1470 rows, 35 columns


summary(data) #identified columns stored as char, will converted them to factor
char_cols <- names(data)[sapply(data, is.character)] #identify columns stored as char


data[, (char_cols) := lapply(.SD, as.factor), .SDcols = char_cols]# Convert them to factors

summary(data)


head(data)
str(data)
colSums(is.na(data))  #no Na

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