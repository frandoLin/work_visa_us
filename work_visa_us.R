#eda on the work visas in the United States

#import the library 
library(tidyverse)
library(readxl)
library(fastDummies)
library(lubridate)

#function to normalize variables
abb_to_state <- function(a)
{
  if(is.na(a)) {
    NA_character_
  } else if(nchar(a) == 2) {
    state.name[match(a,state.abb)]
  } else {
    a
  }
}

wage_normalisation <- function(x, y)
{
  if(is.na(x) | is.na(y)) {
    NA_integer_
  } else if(x == 'Hour') {
    y*52*40
  } else if(x == 'Week') {
    y*52
  } else if(x == 'Bi-Weekly') {
    y*26
  } else if(x == 'Month') {
    y*12
  } else {
    y
  }
}

#read and merge the datasets over the past years
visa <- read_csv("data/us_work_visas.csv", guess_max = min(140000, Inf))

visa_2017 <- read_xlsx("data/PERM_Disclosure_Data_FY17.xlsx") 
  
colnames(visa_2017) <- tolower(colnames(visa_2017))

visa_2018 <- read_xlsx("data/PERM_Disclosure_Data_FY18.xlsx")

colnames(visa_2018) <- tolower(colnames(visa_2018)) 

visa_2019 <- read_xlsx("data/PERM_Disclosure_Data_FY19.xlsx") 

colnames(visa_2019) <- tolower(colnames(visa_2019))

visa_2017_2018 <- merge(visa_2018, visa_2017, all = T)

visa_2017_2019 <- merge(visa_2019, visa_2017_2018, all = T)

visa_to_2019 <- merge(visa_2017_2019, visa, all = T)

visa_to_2019 <- visa_to_2019 %>% drop_na(case_received_date)


#filter the columns with over 80 percent of missing values
visa_to_2019 <- visa_to_2019[lapply( visa_to_2019, function(x) sum(is.na(x)) / length(x) ) < 0.8]

visa_all <- visa_to_2019 %>% 
  select(decision_date,case_received_date,employer_name,employer_city,employer_num_employees,class_of_admission,
         pw_level_9089,pw_amount_9089,pw_unit_of_pay_9089,job_info_job_title,job_info_education,case_status,
         country_of_citizenship,foreign_worker_info_major,job_info_work_state)

visa_all <- visa_all %>% 
  mutate(state = lapply(visa_all$job_info_work_state, abb_to_state)) %>% 
  mutate(company_scale = case_when(
           employer_num_employees <= 10 ~ "startup",
           employer_num_employees > 10 & employer_num_employees <= 100 ~ "small_scale",
           employer_num_employees > 100 & employer_num_employees <= 2000 ~ "medium_scale",
           employer_num_employees > 2000 & employer_num_employees <= 20000 ~ "large_scale",
           employer_num_employees > 20000 ~ "giant_scale",
         )
  )

visa_all$state <- unlist(visa_all$state, use.names=FALSE)

visa_all <- visa_all %>% 
  mutate(processing_time = difftime(visa_all$decision_date,visa_all$case_received_date, units = c("days"))) %>% 
  mutate(case_received_year = year(case_received_date)) %>% 
  mutate(decision_year = year(decision_date))



write.csv(visa_all,"E:\\NSS\\nss_data_science\\work_visa_us\\data\\us_work_visa.csv", row.names = FALSE)

visa1 <- visa_all %>% 
  mutate( wage = lapply(visa_all %>% select(pw_unit_of_pay_9089,visa_all$pw_amount_9089), wage_normalisation))



summary(visa_all)
dim(visa_all)
str(visa_all)









