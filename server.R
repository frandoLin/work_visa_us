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
                      hjust=0, vjust=.5, size = 4, colour = 'black',
                      fontface = 'bold') +
            labs(x = 'State', y = 'Count Of Visa Applications', 
                 title = 'The Top 20 States Hiring Foreign Workers') +
            coord_flip() + 
            theme_bw()
        
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
                      hjust=0, vjust=.5, size = 4, colour = 'black',
                      fontface = 'bold') +
            labs(x = 'City', y = 'Count Of Visa Applications', 
                 title = 'The Top 20 Citys Hiring Foreign Workers') +
            coord_flip() + 
            theme_bw()
        
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
                      hjust=0, vjust=.5, size = 4, colour = 'black',
                      fontface = 'bold') +
            labs(x = 'Employer Name', y = 'Count Of Visa Applications', 
                 title = 'The Top 20 Employers hiring foreign workers') +
            coord_flip() + 
            theme_bw()
        
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
            labs(x = 'Country', y = 'Count Of Visa Applications', title = 'Class of Visa Admission by Country') +
            coord_flip() + 
            theme_bw()
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
            ggplot( aes(x=case_received_year, y=degree_count, group=job_info_education, color=job_info_education)) +
            geom_line() +
            geom_point() +
            scale_color_hue('job_info_education') +
            ggtitle("The Change in The Education of Applicants over the years") +
            theme_ipsum() +
            xlab("year") +
            ylab("Number of babies born")
    })
    
    output$histplot <- renderPlot({
        
        breaks = seq(0,300000,40000)
        
        usPermVisas %>%
            filter(case_status == 'Certified' ) %>%
            ggplot(aes(wage)) +
            scale_x_continuous(limits = c(0, 300000),breaks=breaks ) +
            geom_histogram(binwidth = 10000,,fill = c("red")) +
            labs(x = 'Dollor', y = 'Count', 
                 title = 'Distribution of The Wage for The Applicants') +  theme_bw()
        
    })
    
    output$bi_barplot <- renderPlot({
        
        foreign_local_wage %>% 
            ggplot(aes(x = reorder(job_title, difference), y = difference,
                       fill = difference > 0))+
            geom_bar(stat = "identity")+
            coord_flip()+
            labs(x = "Job Title", y = "Wage Change",
                 title = "The chage in Salaries",
                 subtitles = "mean of the applicants - mean of the Glassdoor")+
            theme_minimal()+
            guides(fill = FALSE)
        
    })
    
    output$dumbbell_1 <- renderPlot({
        
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
                          colour_x="black", 
                          colour_xend = "red")+
            labs(x= 'Year', y= 'State',
                 title="Count Comparison by State", 
                 subtitle="Obama vs Trump")+
            geom_text(color="black", size=2, hjust=-0.5,
                      aes(x=o_count, label=o_count))+
            geom_text(aes(x=t_count, label=t_count), 
                      color="black", size=2, hjust=1.5)
        
    })
    
    output$dumbbell_2 <- renderPlot({
        
        ggplot(o_t_wage, aes(x=o_wage_mean, xend=t_wage_mean, y=state)) + 
            geom_segment(aes(x=o_wage_mean, 
                             xend=t_wage_mean, 
                             y=state, 
                             yend=state), 
                         color="#b2b2b2", size=1.5)+
            geom_dumbbell(color="light blue", 
                          size_x=3.5, 
                          size_xend = 3.5,
                          colour_x="black", 
                          colour_xend = "red")+
            labs(x= 'Year', y= 'State',
                 title="Wage Comparison by State", 
                 subtitle="Obama vs Trump")+
            geom_text(color="black", size=2, hjust=-0.5,
                      aes(x=o_wage_mean, label=t_wage_mean))+
            geom_text(aes(x=o_wage_mean, label=t_wage_mean), 
                      color="black", size=2, hjust=1.5)
        
    })
    
    output$dumbbell_3 <- renderPlot({
        
        ggplot(o_t_time, aes(x=o_time_mean, xend=t_time_mean, y=state)) + 
            geom_segment(aes(x=o_time_mean, 
                             xend=t_time_mean, 
                             y=state, 
                             yend=state), 
                         color="#b2b2b2", size=1.5)+
            geom_dumbbell(color="light blue", 
                          size_x=3.5, 
                          size_xend = 3.5,
                          colour_x="black", 
                          colour_xend = "red")+
            labs(x= 'Year', y= 'State',
                 title="Processing Time Comparison by State", 
                 subtitle="Obama vs Trump")+
            geom_text(color="black", size=2, hjust=-0.5,
                      aes(x=o_time_mean, label=t_time_mean))+
            geom_text(aes(x=o_time_mean, label=t_time_mean), 
                      color="black", size=2, hjust=1.5)
        
    })
    
})
