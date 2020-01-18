#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$plot1 <- renderImage({
        
        # Return a list containing the filename
        list(src = "gganim.gif",
             contentType = 'image/gif',
        width = 1600,
        height = 700,
        alt = "This is alternate text") 
        
    },deleteFile = FALSE)
    
    output$barPlot_1_1 <- renderPlot({
        
        usPermVisas %>% 
            filter(case_status == 'Certified') %>% 
            group_by(state) %>%
            summarise(Count = n()) %>%
            arrange(desc(Count)) %>%
            mutate(state = reorder(state, Count)) %>%
            head(20) %>% 
            
            ggplot(aes(x = state,y = Count)) +
            geom_bar(stat='identity',colour="white", fill =fillColor) +
            geom_text(aes(x = state, y = 1, label = paste0("(",Count,")",sep="")),
                      hjust=0, vjust=.5, size = 6, colour = 'black',
                      fontface = 'bold') +
            labs(x = 'State', y = 'Count Of Visa Applications', 
                 title = 'The Top 20 States Hiring Foreign Workers') +
            coord_flip() + 
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
                plot.subtitle = element_text(face = "italic", size = 16, margin = margin(b = 0.5, unit = "cm"))
            )
        
    })
    
    output$barPlot_1_2 <- renderPlot({
        
        usPermVisas %>% 
            filter(case_status == 'Certified') %>% 
            group_by(employer_city) %>%
            summarise(Count = n()) %>%
            arrange(desc(Count)) %>%
            mutate(city = reorder(employer_city, Count)) %>%
            head(20) %>% 
            
            ggplot(aes(x = city, y = Count)) +
            geom_bar(stat='identity',colour="white", fill =fillColor2) +
            geom_text(aes(x = city, y = 1, label = paste0("(",Count,")",sep="")),
                      hjust=0, vjust=.5, size = 6, colour = 'black',
                      fontface = 'bold') +
            labs(x = 'City', y = 'Count of Visa Applications', 
                 title = 'The Top 20 Citys Hiring Foreign Workers') +
            coord_flip() + 
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
                plot.subtitle = element_text(face = "italic", size = 16, margin = margin(b = 0.5, unit = "cm"))
                )
        
    })
    
    output$barPlot_1_3 <- renderPlot({
        
        usPermVisas %>% 
            filter(if (input$state_1 == 'ALL') case_status == 'Certified' 
                   else state == input$state_1 & case_status == 'Certified') %>% 
            group_by(employer_name) %>%
            summarise(CountOfEmployerName = n()) %>%
            arrange(desc(CountOfEmployerName)) %>%
            mutate(employer_name = reorder(employer_name, CountOfEmployerName)) %>%
            head(20) %>% 
            ggplot(aes(x = employer_name,y = CountOfEmployerName)) +
            geom_bar(stat='identity',colour="white", fill =fillColor) +
            geom_text(aes(x = employer_name, y = 1, label = paste0("(",CountOfEmployerName,")",sep="")),
                      hjust=0, vjust=.5, size = 6, colour = 'black',
                      fontface = 'bold') +
            labs(x = 'Employer Name', y = 'Count Of Visa Applications', 
                 title = 'The Top 20 Employers hiring foreign workers') +
            coord_flip() + 
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
                plot.subtitle = element_text(face = "italic", size = 16, margin = margin(b = 0.5, unit = "cm"))
            )
        
    })
    
    output$barPlot_1_4 <- renderPlot({

        usPermVisas %>% 
            filter(case_status == 'Certified' & class_of_admission != "Not in USA") %>%
            filter(!is.na(country_of_citizenship) & !is.na(class_of_admission)) %>% 
            group_by(country_of_citizenship,class_of_admission) %>%
            summarise(CountOfCountry = n()) %>%
            arrange(desc(CountOfCountry)) %>%
            head(20) %>%
            
            ggplot(aes(x = country_of_citizenship,y = CountOfCountry, fill = class_of_admission)) +
            geom_bar(stat='identity',colour="white") +
            labs(x = 'Country', y = 'Count Of Visa Applications', title = 'Class of Visa Admission by Country',
                 subtitles = paste0(
                     "The E-2 Investor Visa allows an individual to enter and work inside of the United States  \n",
                     "The F-1 visa allows students study in the U.S. \n",
                     "The H1-B is the most common visa for skilled workders to stay temporarily \n",
                     "The L-1 is a short-period work visa\n",
                     "The TN is a kind of special work visa only for the citizens in Mexico and Canada")) +
            coord_flip() + 
            theme(   
                text = element_text(color = "#4e4d47", size = 14),
                axis.text.y = element_text(face = "bold"),
                axis.text.x = element_text(vjust = -0.75),
                axis.title = element_text(size=20),
                axis.ticks = element_blank(),
                legend.title = element_text(size = 20),
                legend.text = element_text(size = 16),
                panel.background = element_blank(),
                panel.grid.major.y = element_line(colour = light_gray, size = 1),
                plot.title = element_text(face = "bold", size = 20),
                plot.subtitle = element_text(face = "italic", size = 12, margin = margin(b = 0.5, unit = "cm"))
            )
        
    })
    
    output$cloud <- renderWordcloud2({
        # Add the draw button as a dependency to
        # cause the word cloud to re-render on click
        input$draw
        isolate({
            if (input$word == 'job'){
                create_wordcloud(job, num_words = input$num, background = input$col)
            } else {
                create_wordcloud(education, num_words = input$num, background = input$col)
                }
        })
    })
    
    output$lineplot <- renderPlot({
        usPermVisas %>% 
            filter(job_info_education != 'None') %>% 
            filter(case_status %in% c('Certified','Certified-Expired')) %>% 
            count(case_received_year, job_info_education, name = "degree_count") %>% 
            ggplot(aes(x=case_received_year, y=degree_count, group=job_info_education, color=job_info_education)) +
            geom_line() +
            geom_point() +
            scale_color_hue('job_info_education') +
            ggtitle("The Change in The Education of Applicants over the years") +
            theme_ipsum() +
            xlab("Year") +
            ylab("Count") +
            theme(   
                legend.text = element_text(size = 14)
            )
    })
    
    output$histplot <- renderPlot({
        
        breaks = seq(0,300000,40000)
        usPermVisas %>%
            filter(case_status == 'Certified' ) %>%
            ggplot(aes(wage)) +
            scale_x_continuous(limits = c(0, 300000),breaks=breaks ) +
            geom_histogram(binwidth = 10000,,fill = c("red")) +
            labs(x = 'Dollor', y = 'Count', 
                 title = 'Distribution of The Wage for The Applicants',
                 subtitle = "The salary for most foreign workers were around from 60K to 12k") +  
            theme(
                text = element_text(color = "#4e4d47", size = 14),
                axis.text.y = element_text(face = "bold"),
                axis.text.x = element_text(vjust = -0.75),
                axis.title = element_text(size=20),
                axis.ticks = element_blank(),
                legend.position = "none",
                panel.grid.major.y = element_line(colour = light_gray, size = 1),
                plot.title = element_text(face = "bold", size = 20),
            ) 
        
    })
    
    output$bi_barplot <- renderPlot({
        
        foreign_local_wage %>% 
            ggplot(aes(x = reorder(job_title, difference), y = difference,
                       fill = difference > 0))+
            geom_bar(stat = "identity")+
            coord_flip()+
            labs(x = "Job Title", y = "Wage_Change_Percentage",
                 title = "Compared to The Mean Salary in Glassdoor by The Job Positions",
                 subtitles = paste0(
                     "For most positions, the foreign workers were slightly paid more than the mean salary in glassdoor\n",
                     "In the sophisticated field, they were well less paid. \n"))+
            theme(   
                text = element_text(color = "#4e4d47", size = 14),
                axis.text.y = element_text(face = "bold"),
                axis.text.x = element_text(vjust = -0.75),
                axis.title = element_text(size=20),
                axis.ticks = element_blank(),
                legend.position = "none",
                legend.title = element_text(size = 20),
                legend.text = element_text(size = 16),
                panel.background = element_blank(),
                panel.grid.major.y = element_line(colour = light_gray, size = 1),
                plot.title = element_text(face = "bold", size = 20),
                plot.subtitle = element_text(face = "italic", size = 14, margin = margin(b = 0.5, unit = "cm"))
            )+
            guides(fill = FALSE)
        
    })
    
    output$dumbbell_1 <- renderPlot({
        
        dumbbell_count
        
    })
    
    output$dumbbell_2 <- renderPlot({
        
        dumbbell_wage
        
    })
    
    output$dumbbell_3 <- renderPlot({
        
        dumbbell_time
        
    })
    
})
