# Workbook 6: analyze NHANES data

#SET UP
library(survey)
library(Hmisc)
library(tidyverse)

#load data
demo <- sasxport.get('DEMO_I.XPT')
alq <- sasxport.get('ALQ_I.XPT')

#joining the data sets by the identifying characteristic 
nhanes <- merge(x = demo, y = alq, by = 'seqn', all = TRUE)

# ariable that is the sample weight, sum col and interpret result
#interpretation: sum of the col is the total population of the US population.
swt_sum <- sum(nhanes$wtint2yr, na.rm = TRUE)

#ANALYSIS
#change to binary
#in ALQ151 we want 2 to be 0 and ignore and 7 & 9
nhanes$alq151[nhanes$alq151 == 2] <- 0
nhanes$alq151[nhanes$alq151 == 7] <- NA
nhanes$alq151[nhanes$alq151 == 9] <- NA

#create survey design
nhanes_survey <- svydesign(id = ~sdmvpsu, strata = ~sdmvstra, 
                           weights = ~wtint2yr, data = nhanes,
                           nest = TRUE)

#percentage of people that have ever had more than 4/5 drinks per day
nhanes_mean <- svymean(x = ~alq151, design = nhanes_survey, na.rm = TRUE)

#percentage of people by gender who have ever had more than 4/5 drinks per day
nhanes_mean_by_gender <- svyby(formula = ~alq151, by = ~riagendr,
                               design = nhanes_survey, FUN = svymean)


