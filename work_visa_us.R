#eda on the work visas in the United States

#import the library 
library(tidyverse)
library(readxl)
library(lubridate)
library(treemapify)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(magrittr)
library(dplyr)
library(ggplot2)   
library(gganimate)
library(gifski)
library(png)


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

visa_all$pw_amount_9089 <- as.integer(visa_all$pw_amount_9089)

visa_all <- visa_all %>% 
  mutate( wage = mapply(wage_normalisation, pw_unit_of_pay_9089, pw_amount_9089)) %>% 
  select(-job_info_work_state, -pw_amount_9089, -pw_unit_of_pay_9089)

write.csv(visa_all,"E:\\NSS\\nss_data_science\\work_visa_us\\data\\us_perm_visas.csv", row.names = FALSE)

usPermVisas =  read_csv("data/us_perm_visas.csv")

#Create a vector containing only the text
job <- usPermVisas$job_info_job_title %>% 
  iconv(to = "utf-8") %>% 
  paste(collapse = ", ") #convert the column to string 

stop_words <- c('senior', 'manager', 'director', 'and', 'ii', 'sr', 'and', 'business', 'lead', 'developer', 'of',
                'v', 'worker', 'staff', 'services', 'iv','iii','technology','food','i','multiple', 'other', 'or', 
                'commercial','rd','meat','office','group','serving','computer','development','computer','technical',
                'it','poultry') 

docs <- data_frame(text = job) %>% 
  mutate(text = tolower(text)) %>% 
  mutate(text = removeNumbers(text)) %>%
  mutate(text = str_remove_all(text, '[[:punct:]]')) %>% 
  mutate(tokens = str_split(text, "\\s+")) %>%
  unnest() %>% 
  count(tokens) %>% 
  arrange(desc(n)) %>% 
  filter(n>5000) %>% 
  filter(!tokens %in% stop_words)

wordcloud2(docs, color = 'random-dark')

fillColor = "#FFA07A"
fillColor2 = "#F1C40F"

usPermVisas %>% 
  filter(state == None & case_status == 'Certified') %>% 
  group_by(employer_name) %>%
  summarise(CountOfEmployerName = n()) %>%
  arrange(desc(CountOfEmployerName)) %>%
  mutate(employer_name = reorder(employer_name, CountOfEmployerName)) %>%
  head(20) %>% 
  
  ggplot(aes(x = employer_name,y = CountOfEmployerName)) +
  geom_bar(stat='identity',colour="white", fill =fillColor) +
  geom_text(aes(x = employer_name, y = 1, label = paste0("(",CountOfEmployerName,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Employer Name', y = 'Count Of Visa Applications in NYC', title = 'Employers in NYC and Visas') +
  coord_flip() + 
  theme_bw()

#the plot for the changes in the citizenship of the visa applicants over the years
countCountry <- usPermVisas %>% 
  filter(case_status == 'Certified') %>% 
  drop_na(country_of_citizenship) %>% 
  count(case_received_year, country_of_citizenship, name = "counts") %>% 
  rename(year = `case_received_year`, nationality = `country_of_citizenship`) %>% 
  group_by(year)%>%      
  mutate(rank = rank(-counts),
         Value_rel = counts/counts[rank==1],
         Value_lbl = paste0(" ",counts)) %>%
  group_by(nationality) %>%
  filter(rank <= 10)

anim <- ggplot(countCountry, aes(rank, group = nationality))+
  geom_tile(aes(y = counts/2,
                height = counts,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(nationality, " ")), vjust = 0.2, hjust = 1, size = 7) + #determine size of the Nationlity label
  geom_text(aes(y=counts,label = Value_lbl, hjust=0),size = 8 ) +  #determine size of the value label
  coord_flip(clip = "off", expand = TRUE) +
  scale_x_reverse() +
  theme_minimal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold",     colour="red", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="red"),
        plot.caption =element_text(size=12, hjust=0.5, face="italic", color="red"),
        plot.background=element_blank(),
        plot.margin = margin(1,4, 1, 8, "cm")) +
  transition_states(year, transition_length = 4, state_length = 1) +
  ease_aes('sine-in-out') +
  labs(title = 'Number of Foreign Workers in The U.S.A per Year by Nationality: {closest_state}',  
       caption  = "Data Source: https://www.foreignlaborcert.doleta.gov/performancedata.cfm")

animate(anim, nframes = 350,fps = 25,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim.gif"))
