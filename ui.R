#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(
    dashboardPage(
        skin = 'red',
        dashboardHeader(title = 'Foreign Workers'),
        dashboardSidebar(
            div(a(href = "https://www.foreignlaborcert.doleta.gov/performancedata.cfm", img(src="WWW/tn.png", width = 140, height=70))),
            sidebarMenu(
                menuItem("About", tabName = "about", icon = icon("info-circle")),
                menuItem("Trends", tabName = "trends", icon = icon("globe")),
                menuItem("Widgets", icon = icon("th"), tabName = "widgets"),
                menuItem("Obama V.S. Trump", icon = icon("bar-chart-o"),
                         menuSubItem("Counts of Applicants", tabName = "counts"),
                         menuSubItem("Wage", tabName = "wage"),
                         menuSubItem("Processing Time", tabName = "processing_time")
                )
            )
        ),
        dashboardBody(
            tabItems(
                tabItem("about",
                    mainPanel(
                        fluidRow(
                            align = "center",
                            "How to center this?"
                        )
                    ),
                    mainPanel(
                        fluidRow(
                            div(a(img(src="WWW/p.jpg")))
                        )
                    )
                ),
                tabItem("trends",
                    fluidRow(
                        tabBox(width = 1650, height = 900,
                            tabPanel("By State", 
                                 fluidRow(
                                     plotOutput("barPlot_1_1", width = 1600, height = 750)
                                 )      
                            ),
                            tabPanel("By City", 
                                 fluidRow(
                                     plotOutput("barPlot_1_2", width = 1600, height = 750)
                                 )      
                            ),
                            tabPanel("By company", 
                                 fluidRow(
                                     selectInput("state_1", label = "States:", choices = state,
                                                 selected = 'ALL')
                                 ),
                                 fluidRow(
                                     plotOutput("barPlot_1_3", width = 1600, height = 750)
                                 )  
                            ),
                            tabPanel("By Type", 
                                 fluidRow(
                                     plotOutput("barPlot_1_4", width = 1600, height = 750)
                                 )      
                            )
                        )
                    )    
                ),
                tabItem("widgets",   
                    fluidRow(
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("word", label = "Word:", choices = word,
                                            selected = 'education'),
                                numericInput("num", "Maximum number of words",
                                             value = 100, min = 5),
                                colourInput("col", "Background color", value = "white"),
                
                                actionButton(inputId = "draw", label = "Draw!")
                            ),
                            mainPanel(
                                wordcloud2Output("cloud")
                            )
                        )
                    ),
                    
                    fluidRow(
                        conditionalPanel(
                            condition = "input.word == 'education'",
                            plotOutput("lineplot", width = 1650, height = 500)
                        ),
                        conditionalPanel(
                            condition = "input.word == 'job'",
                            tabBox(width = 1650, height = 900,
                                   tabPanel("Distribution", 
                                            fluidRow(
                                                plotOutput("histplot", width = 1600, height = 500)
                                            )      
                                   ),
                                   tabPanel("coming soon...", 
                                            fluidRow(
                                                plotOutput("bi_barplot", width = 1600, height = 500)
                                            )      
                                   )
                            )
                        )
                    )
                ),
                tabItem("counts", 
                    fluidRow(
                        plotOutput("dumbbell_1", width = 1650, height = 850)
                    )
                ), 
                tabItem("wage",  
                    fluidRow(
                        plotOutput("dumbbell_2", width = 1650, height = 850)
                    )
                ),
                tabItem("processing_time", 
                    fluidRow(
                        plotOutput("dumbbell_3", width = 1650, height = 850)
                    ) 
                )
            )
        )
    )
)
