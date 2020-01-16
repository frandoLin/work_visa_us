library(shinydashboard)
library(tidyverse)
library(DT)
library(colourpicker)
library(NLP)
library(wordcloud)
library(wordcloud2)
library(ggplot2)   
library(gganimate)
library(tm)
library(hrbrthemes)
library(viridis)

#some colors for plots
fillColor = "#FFA07A"
fillColor2 = "#F1C40F"

usPermVisas <-  read.csv("data/us_perm_visas.csv", encoding = "UTF-8") 

usPermVisas$state <- toupper(usPermVisas$state) 

usPermVisas$employer_city <- usPermVisas$employer_city %>% 
  iconv(to = "utf-8") %>%
  toupper()

#wage comparision between applicants and glassdoor
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

#columns for wordcloud generation
job <- usPermVisas$job_info_job_title %>% 
  iconv(to = "utf-8") %>% 
  paste(collapse = ", ") #convert the column to string 

education <- usPermVisas%>% 
  filter(job_info_education != 'None') %>% 
  count(job_info_education )

stop_words <- c('senior', 'manager', 'director', 'and', 'ii', 'sr', 'and', 'business', 'lead', 'developer', 'of',
                'v', 'worker', 'staff', 'services', 'iv','iii','technology','food','i','multiple', 'other', 'or', 
                'commercial','rd','meat','office','group','serving','computer','development','computer','technical',
                'it','poultry','associate', 'design','engineering','principal') 

#fuction for wordcloud generation
create_wordcloud <- function(data, num_words = 100, background) {
  
  # If text is provided, convert it to a dataframe of word frequencies
  
  if(data == job){
    docs <- data_frame(text = data) %>% 
    mutate(text = tolower(text)) %>% 
    mutate(text = removeNumbers(text)) %>%
    mutate(text = str_remove_all(text, '[[:punct:]]')) %>% 
    mutate(tokens = str_split(text, "\\s+")) %>%
    unnest() %>% 
    count(tokens) %>% 
    arrange(desc(n)) %>% 
    filter(n>5000) %>% 
    filter(!tokens %in% stop_words)
  
  # Grab the top n most common words
    docs <- head(docs, n = num_words)
    if (nrow(docs) == 0) {
      return(NULL)
    }
    
    wordcloud2(docs, color = 'random-dark', backgroundColor = background, widgetsize = 250)
  } else {
    wordcloud2(data, color = 'random-dark', backgroundColor = background, widgetsize = 250)
    }
}

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


# Generate Options for dropdown menu in ui
state <- usPermVisas %>% 
  drop_na(state) %>% 
  pull(state) %>% 
  unique() %>% 
  str_sort() %>% 
  append("ALL", after = 2)

word <- c('education', 'job')

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
  labs(x= 'Year', y= 'State',
       title="Processing Time Comparison by State", 
       subtitle="Obama vs Trump")+
  geom_text(color="black", size=2, hjust=-0.5,
            aes(x=o_time_mean, label=t_time_mean))+
  geom_text(aes(x=o_time_mean, label=t_time_mean), 
            color="black", size=2, hjust=1.5)
