install.packages("sqldf")
install.packages("RSQLite")
install.packages("xlsx")
install.packages("readr")
install.packages("aod")
library(sqldf)
library(readr)
library(ggplot2)
library(aod)
library(Rcpp)
library(dplyr)
library(tidyr)
library(DT)
library(xlsx)
library(magrittr)

setwd(dir = '/Users/nitishaadhikari/desktop')
download.file(url='https://kaggle2.blob.core.windows.net/datasets/442/905/tobacco.csv?sv=2015-12-11&sr=b&sig=f6aKwLZDbz%2BKtW85zrtcQrcPuR%2BENrxg%2BWLFNeu2P%2B0%3D&se=2016-11-28T22%3A09%3A06Z&sp=r', 
              destfile = 'DMtest.txt',  method = 'libcurl')

HRData <- read.csv(file = '/Users/nitishaadhikari/Desktop/06 Data Management/Project/HR_comma_sep.csv', stringsAsFactors = FALSE)

head(HRData)
str(HRData)
summary(HRData)

CountRows <- sqldf( "Select COUNT(*) as NumberOfRecords 
                    FROM HRData")
CountRows

MeanValuesbyDept <- sqldf("SELECT  Sales as Department
                                ,COUNT(*) as NumberOfEmployees
                                ,AVG(satisfaction_level) as AvgSatisfaction
                                ,AVG(number_project) as AvgNumOfProjects
                                ,AVG(time_spend_company) as AvgNumOfYears
                                ,AVG(average_montly_hours) as HoursPerMonth
                                ,AVG(last_evaluation) as AvgEvaluation
                    FROM HRData
                    GROUP BY Sales
                    ORDER BY AvgSatisfaction DESC")
MeanValuesbyDept

SalaryStructure <- sqldf("SELECT Sales as Department
                         ,Salary 
                         ,COUNT(*) as NumOfEmployees
                         ,SUM(left) as NumOfLeavingEmployees
                         ,AVG(time_spend_company) as NumOfYears
                         ,AVG(average_montly_hours) as AvgHours
                         FROM HRData
                         GROUP BY Sales, Salary")
SalaryStructure

PerformanceVsSalary <- sqldf("SELECT Sales as Department
                             ,Salary
                             ,COUNT(*) as NumOfEmployees
                             ,SUM(left) as NumOfLeavingEmployees
                             ,AVG(satisfaction_level) as AvgSatisfaction
                             ,AVG(last_evaluation) as AvgEvaluation
                             FROM HRData
                         GROUP BY Sales, Salary")
PerformanceVsSalary

#Logistic Regression
#Convert Salary to a factor to indicate it should be treated as a categorical variable
HRData$salary <- factor(HRData$salary)
mylogit <- glm(left ~ satisfaction_level + last_evaluation + salary, data = HRData, family = "binomial")
summary(mylogit)

## CIs using profiled log-likelihood
confint(mylogit)




