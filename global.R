library(shinydashboard)
library(readxl)
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
library(ggalt)
library(magrittr)
library(dplyr)

#some colors for plots
fillColor = "#FFA07A"
fillColor2 = "#F1C40F"

usPermVisas <-  read.csv("data/us_perm_visas.csv", encoding = "UTF-8") 

usPermVisas$state <- toupper(usPermVisas$state) 

usPermVisas$employer_city <- usPermVisas$employer_city %>% 
  iconv(to = "utf-8") %>%
  toupper()

usPermVisas$job_info_job_title <- usPermVisas$job_info_job_title %>% 
  iconv(to = "utf-8") %>%
  tolower()

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
  mutate(difference = round((wage_local - wage_mean)/wage_mean*100))
  
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
  if (data==job){ 
    
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
  }else{ 
    
    wordcloud2(education, color = 'random-dark', backgroundColor = background, widgetsize = 250)
    
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
  mutate(change = round((t_count - o_count)/o_count*100)) %>% 
  arrange(change, desc(state))%>%
  mutate(state.fact = factor(state, levels = unique(state))) %>% 
  gather(key = count_group, value = people, o_count, t_count) %>% 
  select(state, state.fact, count_group, people, change)

dumbbell_count <- ggplot(data = obama_trump_count,
                         mapping = aes(y=state.fact, x= people, color = ifelse(people == 0, "zero", count_group))) + 
  geom_line(mapping = aes(group = state), color = gray, size = 2.5) +
  geom_point(size=4, pch = 19) + 
  geom_text(size = 4, fontface = "bold", nudge_y = 1,
            mapping = aes(label =ifelse(state == "WASHINGTON",
                                        ifelse(count_group == "o_count", "Obama", "Trump"),""),
                          color = ifelse(people == 0, "zero", count_group))) +
  scale_color_manual(values = c(orange, green, "gray")) +
  scale_x_continuous(limits = c(0,44000)) +
  scale_y_discrete(expand = expand_scale(add=c(0.65,1))) +
  geom_rect(mapping = aes(xmin = 42000, xmax = Inf , ymin = -Inf, ymax = Inf),
            fill = "white",color = "white") +
  geom_rect(mapping = aes(xmin = 42000, xmax = 44000 , ymin = -Inf, ymax = Inf),
            fill = background_diff,color = background_diff) +
  # Add Differences values
  geom_text(fontface = "bold", size = 4, colour = "black",
            mapping = aes(x = 43000, y = state,
                          label = ifelse(count_group == "o_count","",
                                         ifelse(change == 0,paste0(as.character(change)),
                                                ifelse(change > 0, paste0("+",as.character(change)), paste0(as.character(change))))))) +
  # Insert Title of Differences
  geom_text(fontface = "bold", size = 4, colour = "gray", nudge_y = 0.6,
            mapping = aes(x = 43000, y = state,label = ifelse(state == "NORTHERN MARIANA ISLANDS", "DIFF",""))) +
  # Plot Title and Axis Labels
  labs(title = "The Comparison of The Number for The ceritfied applicants",
       subtitle = paste0(
         "Under the Trump administration, the number of the workers \n",
         "being certified was even more than that under the Obama"),
       x = "Count", y = "State") +
  theme(
    text = element_text(color = "#4e4d47", size = 14),
    axis.text.y = element_text(face = "bold"),
    axis.text.x = element_text(vjust = -0.75),
    axis.title = element_text(size=20),
    axis.ticks = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = light_gray, size = 1),
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(face = "italic", size = 16, margin = margin(b = 0.5, unit = "cm"))) 

#weighted salary by state
o_salary_weight <- usPermVisas %>% 
  filter(decision_year %in% c(2014,2015,2016)&case_status=='Certified'& !is.na(state) & !is.na(employer_city)) %>% 
  count(state, employer_city, name = "o_count_by_city") 

o_salary <- usPermVisas %>% 
  filter(case_status=='Certified'& !is.na(state) & !is.na(employer_city) & !is.na(wage)) %>% 
  select(state, employer_city, wage) %>% 
  inner_join(o_salary_weight, by = c("state", 'employer_city')) %>% 
  group_by(state) %>% 
  summarise(o_wage_mean = round(weighted.mean(wage, o_count_by_city)))

t_salary_weight <- usPermVisas %>% 
  filter(decision_year %in% c(2017,2018,2019)&case_status=='Certified'& !is.na(state) & !is.na(employer_city)) %>% 
  count(state, employer_city, name = "t_count_by_city")

t_salary <- usPermVisas %>% 
  filter(case_status=='Certified'& !is.na(state) & !is.na(employer_city) & !is.na(wage)) %>% 
  select(state, employer_city, wage) %>% 
  inner_join(t_salary_weight, by = c("state", 'employer_city')) %>% 
  group_by(state) %>% 
  summarise(t_wage_mean = round(weighted.mean(wage, t_count_by_city)))

o_t_wage <- o_salary %>% 
  inner_join(t_salary, by='state') %>% 
  mutate(change = round((t_wage_mean - o_wage_mean)/o_wage_mean*100)) %>%  
  arrange(change, desc(state))%>%
  mutate(state.fact = factor(state, levels = unique(state))) %>% 
  gather(key = wage_group, value = dollar, o_wage_mean, t_wage_mean) %>% 
  select(state, state.fact, wage_group, dollar, change)

dumbbell_wage <- ggplot(data = o_t_wage,
                        mapping = aes(y=state.fact, x= dollar, color = ifelse(dollar == 0, "zero", wage_group))) + 
  geom_line(mapping = aes(group = state), color = gray, size = 2.5) +
  geom_point(size=4, pch = 19) + 
  geom_text(size = 4, fontface = "bold", nudge_y = 0.6,
            mapping = aes(label =ifelse(state == "SOUTH CAROLINA",
                                        ifelse(wage_group == "o_wage_mean", "Obama", "Trump"),""),
                          color = ifelse(dollar == 0, "zero", wage_group))) +
  scale_color_manual(values = c(orange, green, "gray")) +
  scale_x_continuous(limits = c(19400,140000)) +
  scale_y_discrete(expand = expand_scale(add=c(0.65,1))) +
  geom_rect(mapping = aes(xmin = 135000, xmax = Inf , ymin = -Inf, ymax = Inf),
            fill = "white",color = "white") +
  geom_rect(mapping = aes(xmin = 135000, xmax = 140000 , ymin = -Inf, ymax = Inf),
            fill = background_diff,color = background_diff) +
  # Add Differences values
  geom_text(fontface = "bold", size = 4, colour = "black",
            mapping = aes(x = 137500, y = state,
                          label = ifelse(wage_group == "o_wage_mean","",
                                         ifelse(change == 0,paste0(as.character(change)),
                                                ifelse(change > 0, paste0("+",as.character(change)), paste0(as.character(change))))))) +
  # Insert Title of Differences
  geom_text(fontface = "bold", size = 4, colour = "gray", nudge_y = 0.6,
            mapping = aes(x = 137500, y = state,label = ifelse(state == "SOUTH CAROLINA", "DIFF",""))) +
  # Plot Title and Axis Labels
  labs(title = "The Comparison of The Wage for The ceritfied applicants",
       subtitle = paste0(
         "Overall, under the Trump administration, the minimum wage for the \n",
         "application approval was higher than that under the Obama"),
       x = "State", y = "Dollar") +
  theme(
    text = element_text(color = "#4e4d47", size = 14),
    axis.text.y = element_text(face = "bold"),
    axis.text.x = element_text(vjust = -0.75),
    axis.title = element_text(size=20),
    axis.ticks = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = light_gray, size = 1),
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(face = "italic", size = 16, margin = margin(b = 0.5, unit = "cm"))) 

#weighted process time by state
o_time <- usPermVisas %>% 
  filter(case_status=='Certified'& !is.na(state) & !is.na(employer_city) & !is.na(processing_time)) %>% 
  select(state, employer_city, processing_time) %>% 
  inner_join(o_salary_weight, by = c("state", 'employer_city')) %>% 
  group_by(state) %>% 
  summarise(o_time_mean = round(weighted.mean(processing_time, o_count_by_city)))

t_time <- usPermVisas %>% 
  filter(case_status=='Certified'& !is.na(state) & !is.na(employer_city) & !is.na(processing_time)) %>% 
  select(state, employer_city, processing_time) %>% 
  inner_join(t_salary_weight, by = c("state", 'employer_city')) %>% 
  group_by(state) %>% 
  summarise(t_time_mean = round(weighted.mean(processing_time, t_count_by_city)))

o_t_time <- o_time %>% 
  inner_join(t_time, by='state') %>% 
  mutate(change = round((t_time_mean - o_time_mean)/o_time_mean*100)) %>% 
  arrange(change, desc(state))%>%
  mutate(state.fact = factor(state, levels = unique(state))) %>% 
  gather(key = time_group, value = processing_day, o_time_mean, t_time_mean) %>% 
  select(state, state.fact, time_group, processing_day, change)

dumbbell_time <- ggplot(
  data = o_t_time,
  mapping = aes(y=state.fact, x=processing_day, color = ifelse(processing_day == 0, "zero", time_group))) + 
  geom_line(mapping = aes(group = state), color = gray, size = 2.5) +
  geom_point(size=4, pch = 19) + 
  geom_text(size = 4, fontface = "bold", nudge_y = 0.6,
            mapping = aes(label =ifelse(state == "DELAWARE",
                                        ifelse(time_group == "o_time_mean", "Obama", "Trump"),""),
                          color = ifelse(processing_day == 0, "zero", time_group))) +
  scale_color_manual(values = c(orange, green, "gray")) +
  scale_x_continuous(limits = c(0,520)) +
  scale_y_discrete(expand = expand_scale(add=c(0.65,1))) +
  geom_rect(mapping = aes(xmin = 500, xmax = Inf , ymin = -Inf, ymax = Inf),
            fill = "white",color = "white") +
  geom_rect(mapping = aes(xmin = 500, xmax = 520 , ymin = -Inf, ymax = Inf),
            fill = background_diff,color = background_diff) +
  # Add Differences values
  geom_text(fontface = "bold", size = 4, colour = "black",
            mapping = aes(x = 510, y = state,
                          label = ifelse(time_group == "o_time_mean","",
                                         ifelse(change == 0,paste0(as.character(change)),
                                                ifelse(change > 0, paste0("+",as.character(change)), paste0(as.character(change))))))) +
  # Insert Title of Differences
  geom_text(fontface = "bold", size = 4, colour = "gray", nudge_y = 0.6,
            mapping = aes(x = 510, y = state,label = ifelse(state == "DELAWARE", "DIFF",""))) +
  # Plot Title and Axis Labels
  labs(title = "The Comparison of The Processing Time for The ceritfied applicants",
       subtitle = paste0(
         "Under the Trump administration, the processing time for the \n",
         "application approval was shorter than that under the Obama"),
       x = "State", y = "Day") +
  theme(
    text = element_text(color = "#4e4d47", size = 14),
    axis.text.y = element_text(face = "bold"),
    axis.text.x = element_text(vjust = -0.75),
    axis.title = element_text(size=20),
    axis.ticks = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = light_gray, size = 1),
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(face = "italic", size = 16, margin = margin(b = 0.5, unit = "cm"))) 


# Generate Options for dropdown menu in ui
state <- usPermVisas %>% 
  drop_na(state) %>% 
  pull(state) %>% 
  unique() %>% 
  str_sort() %>% 
  append("ALL", after = 2)

word <- c('education', 'job')



