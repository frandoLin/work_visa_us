library(shinydashboard)
library(tidyverse)
library(DT)
library(colourpicker)
library(NLP)
library(wordcloud)
library(wordcloud2)
library(tm)

usPermVisas <-  read_csv("data/us_perm_visas.csv") 

#some colors for plots
fillColor = "#FFA07A"
fillColor2 = "#F1C40F"

#columns for wordcloud generation
job <- usPermVisas$job_info_job_title %>% 
  iconv(to = "utf-8") %>% 
  paste(collapse = ", ") #convert the column to string 

stop_words <- c('senior', 'manager', 'director', 'and', 'ii', 'sr', 'and', 'business', 'lead', 'developer', 'of',
                'v', 'worker', 'staff', 'services', 'iv','iii','technology','food','i','multiple', 'other', 'or', 
                'commercial','rd','meat','office','group','serving','computer','development','computer','technical',
                'it','poultry','associate') 

#fuction for wordcloud generation
create_wordcloud <- function(data, num_words = 100, background) {
  
  # If text is provided, convert it to a dataframe of word frequencies
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
  
  wordcloud2(docs, color = 'random-dark', backgroundColor = background)
}

# Generate Options for dropdown menu in ui
states <- usPermVisas %>% 
  drop_na(state) %>% 
  pull(state) %>% 
  toupper() %>% 
  unique()

