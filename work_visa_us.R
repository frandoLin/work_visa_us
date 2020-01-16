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
library(chorddiag)
library(hrbrthemes)
library(viridis)
library(ggalt)


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


#add approval rate by company and fill by the scale of the companys
usPermVisas %>% 
  filter(case_status == 'Certified') %>% 
  group_by(state) %>%
  summarise(CountOfEmployerName = n()) %>%
  arrange(desc(CountOfEmployerName)) %>%
  mutate(state = reorder(state, CountOfEmployerName)) %>%
  head(20) %>% 
  
  ggplot(aes(x = state,y = CountOfEmployerName)) +
  geom_bar(stat='identity',colour="white", fill =fillColor) +
  geom_text(aes(x = state, y = 1, label = paste0("(",CountOfEmployerName,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Employer Name', y = 'Count Of Visa Applications in NYC', title = 'Employers in NYC and Visas') +
  coord_flip() + 
  theme_bw()


#Create a vector containing only the text
job <- usPermVisas$job_info_job_title %>% 
  iconv(to = "utf-8") %>% 
  paste(collapse = ", ") #convert the column to string 

stop_words <- c('senior', 'manager', 'director', 'and', 'ii', 'sr', 'and', 'business', 'lead', 'developer', 'of',
                'v', 'worker', 'staff', 'services', 'iv','iii','technology','food','i','multiple', 'other', 'or', 
                'commercial','rd','meat','office','group','serving','computer','development','computer','technical',
                'it','poultry') 

#change count to frequency
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

wordcloud2(education, color = 'random-dark')

fillColor = "#FFA07A"
fillColor2 = "#F1C40F"


#degree
usPermVisas %>% 
  filter(job_info_education != 'None') %>% 
  filter(case_status %in% c('Certified','Certified-Expired')) %>% 
  count(case_received_year, job_info_education, name = "degree_count") %>% 
  ggplot( aes(x=case_received_year, y=degree_count, group=job_info_education, color=job_info_education)) +
  geom_line() +
  geom_point() +
  scale_color_hue('job_info_education') +
  ggtitle("The Change in The Education of Applicants over the years") +
  theme_ipsum() +
  xlab("year") +
  ylab("Number of babies born") +
  transition_reveal(year)


#job
#tag1 hist
breaks = seq(0,300000,40000)

usPermVisas %>%
  filter(case_status == 'Certified' ) %>%
  ggplot(aes(wage)) +
  scale_x_continuous(limits = c(0, 300000),breaks=breaks ) +
  geom_histogram(binwidth = 10000,,fill = c("red")) +
  labs(x = 'Amount in 9089', y = 'Count', 
       title = 'Certified US Visa PW Amount 9089 distribution') +  theme_bw()

#tag2 v.s. local salary
glassdoor <- read_xlsx("data/glassdoor.xlsx")

foreign_local_wage <- usPermVisas %>% 
mutate(job_title = case_when(
  str_detect(job_info_job_title, "database engineer") ~ "database engineer",
  str_detect(job_info_job_title, "database administrator") ~ "database administrator",
  str_detect(job_info_job_title, "data analyst") ~ "data analyst",
  str_detect(job_info_job_title, "warehousing") ~ "data warehousing engineer",
  str_detect(job_info_job_title, "programer analyst") ~ "programmer analyst",
  str_detect(job_info_job_title, "data scientist") ~ "data scientist",
  str_detect(job_info_job_title, "quality assurance") ~ "quality assurance analyst",
  str_detect(job_info_job_title, "component design") ~ "component design engineer",
  str_detect(job_info_job_title, "software developer") ~ "software developer",
  str_detect(job_info_job_title, "software engineer") ~ "software developer",
  str_detect(job_info_job_title, "consultant") ~ "consultant",
  str_detect(job_info_job_title, "test") ~ "test engineer",
  str_detect(job_info_job_title, "project manager") ~ "project manager",
  str_detect(job_info_job_title, "assistant professor") ~ "assistant professor",
  str_detect(job_info_job_title, "cook") ~ "cook",
)) %>% 
  drop_na(job_title) %>% 
  group_by(job_title) %>% 
  summarise(wage_mean = mean(wage, na.rm=TRUE)) %>% 
  inner_join(glassdoor, by = 'job_title') %>% 
  mutate(difference = wage_local - wage_mean)


foreign_local_wage %>% 
  ggplot(aes(x = reorder(job_title, difference), y = difference,
             fill = difference > 0))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(x = "Country", y = "GDP per Capita Change %",
       title = "Percentage chage in GDP per Capita",
       subtitles = "Americas (1997 to 2007)")+
  theme_minimal()+
  guides(fill = FALSE)


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

#trump v.s. obama by counts, salary and process time
obama_count <- usPermVisas %>% 
  filter(decision_year %in% c(2014, 2015, 2016) & case_status == 'Certified') %>% 
  count(state, name = "o_count") 

trump_count <- usPermVisas %>% 
  filter(decision_year %in% c(2017, 2018, 2019) & case_status == 'Certified') %>% 
  count(state, name = "t_count")

obama_trump_count <- obama_count %>%
  full_join(trump_count, by = c("state"))  %>% 
  filter(!is.na(state)) %>% 
  replace_na(list(o_count = 0.1, t_count = 0.1)) %>% 
  mutate(count_change = t_count - o_count)

#counts
ggplot(obama_trump_count, aes(x=o_count, xend=t_count, y=state)) + 
  #create a thick line between x and xend instead of using defaut 
  #provided by geom_dubbell
  geom_segment(aes(x=o_count, 
                   xend=t_count, 
                   y=state, 
                   yend=state), 
               color="#b2b2b2", size=1.5)+
  geom_dumbbell(color="light blue", 
                size_x=3.5, 
                size_xend = 3.5,
                #Note: there is no US:'color' for UK:'colour' 
                # in geom_dumbbel unlike standard geoms in ggplot()
                colour_x=fillColor, 
                colour_xend = fillColor2)+
  labs(x=NULL, y=NULL, 
       title="Dumbbell Chart", 
       subtitle="Obama vs Trump")+
  geom_text(color="black", size=2, hjust=-0.5,
            aes(x=o_count, label=o_count))+
  geom_text(aes(x=t_count, label=t_count), 
            color="black", size=2, hjust=1.5)

#weighted salary by state
o_salary_weight <- usPermVisas %>% 
  filter(decision_year %in% c(2014,2015,2016)&case_status=='Certified'& !is.na(state) & !is.na(employer_city)) %>% 
  count(state, employer_city, name = "o_count_by_city") 

o_salary <- usPermVisas %>% 
  filter(case_status=='Certified'& !is.na(state) & !is.na(employer_city) & !is.na(wage)) %>% 
  select(state, employer_city, wage) %>% 
  inner_join(o_salary_weight, by = c("state", 'employer_city')) %>% 
  group_by(state) %>% 
  summarise(o_wage_mean = weighted.mean(wage, o_count_by_city))

t_salary_weight <- usPermVisas %>% 
  filter(decision_year %in% c(2017,2018,2019)&case_status=='Certified'& !is.na(state) & !is.na(employer_city)) %>% 
  count(state, employer_city, name = "t_count_by_city")

t_salary <- usPermVisas %>% 
  filter(case_status=='Certified'& !is.na(state) & !is.na(employer_city) & !is.na(wage)) %>% 
  select(state, employer_city, wage) %>% 
  inner_join(t_salary_weight, by = c("state", 'employer_city')) %>% 
  group_by(state) %>% 
  summarise(t_wage_mean = weighted.mean(wage, t_count_by_city))

o_t_wage <- o_salary %>% 
  inner_join(t_salary, by='state') %>% 
  mutate(change = t_wage_mean - o_wage_mean)

ggplot(o_t_wage, aes(x=o_wage_mean, xend=t_wage_mean, y=state)) + 
  geom_segment(aes(x=o_wage_mean, 
                   xend=t_wage_mean, 
                   y=state, 
                   yend=state), 
               color="#b2b2b2", size=1.5)+
  geom_dumbbell(color="light blue", 
                size_x=3.5, 
                size_xend = 3.5,
                colour_x=fillColor, 
                colour_xend = fillColor2)+
  labs(x=NULL, y=NULL, 
       title="Dumbbell Chart", 
       subtitle="Obama vs Trump")+
  geom_text(color="black", size=2, hjust=-0.5,
            aes(x=o_wage_mean, label=t_wage_mean))+
  geom_text(aes(x=o_wage_mean, label=t_wage_mean), 
            color="black", size=2, hjust=1.5)

#weighted process time by state
o_time <- usPermVisas %>% 
  filter(case_status=='Certified'& !is.na(state) & !is.na(employer_city) & !is.na(processing_time)) %>% 
  select(state, employer_city, processing_time) %>% 
  inner_join(o_salary_weight, by = c("state", 'employer_city')) %>% 
  group_by(state) %>% 
  summarise(o_time_mean = weighted.mean(processing_time, o_count_by_city))

t_time <- usPermVisas %>% 
  filter(case_status=='Certified'& !is.na(state) & !is.na(employer_city) & !is.na(processing_time)) %>% 
  select(state, employer_city, processing_time) %>% 
  inner_join(t_salary_weight, by = c("state", 'employer_city')) %>% 
  group_by(state) %>% 
  summarise(t_time_mean = weighted.mean(processing_time, t_count_by_city))

o_t_time <- o_time %>% 
  inner_join(t_time, by='state') %>% 
  mutate(change = t_time_mean - o_time_mean)

ggplot(o_t_time, aes(x=o_time_mean, xend=t_time_mean, y=state)) + 
  geom_segment(aes(x=o_time_mean, 
                   xend=t_time_mean, 
                   y=state, 
                   yend=state), 
               color="#b2b2b2", size=1.5)+
  geom_dumbbell(color="light blue", 
                size_x=3.5, 
                size_xend = 3.5,
                colour_x=fillColor, 
                colour_xend = fillColor2)+
  labs(x=NULL, y=NULL, 
       title="Dumbbell Chart", 
       subtitle="Obama vs Trump")+
  geom_text(color="black", size=2, hjust=-0.5,
            aes(x=o_time_mean, label=t_time_mean))+
  geom_text(aes(x=o_time_mean, label=t_time_mean), 
            color="black", size=2, hjust=1.5)


  



students = data.frame(Math = c(50, 25, 5, 12),
                      Art = c(10, 55, 5, 20),
                      Science = c(45,12,29, 20),
                      PE = c(24,67,27,15))

students = as.matrix(students)
row.names(students) = c("Section A", "Section B", "Section C", "Section D")

chorddiag(students, type = "bipartite", groupnameFontsize = 14, groupnamePadding = 10, margin = 90)










