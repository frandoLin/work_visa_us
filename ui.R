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
        dashboardHeader(title = 'Foreign Workers'),
        dashboardSidebar(
            sidebarMenu(
                actionLink(inputId='ab1', label="Xin Lin", 
                           icon = icon("github"), 
                           onclick ="window.open('https://github.com/frandoLin/work_visa_us', '_blank')"),
                br(),
                br(),
                menuItem("About", tabName = "about", icon = icon("info-circle")),
                menuItem("Trends", tabName = "trends", icon = icon("globe")),
                menuItem("Widgets", icon = icon("th"), tabName = "widgets"),
                menuItem("Obama V.S. Trump", icon = icon("bar-chart-o"),
                         menuSubItem("Counts of Applicants", tabName = "counts"),
                         menuSubItem("Wage", tabName = "wage"),
                         menuSubItem("Processing Time", tabName = "processing_time")
                        ),
                menuItem(" Certification", icon = icon("cc-visa"),
                         menuSubItem("Type of degree", tabName = "degree"),
                         menuSubItem("Type of visa", tabName = "visa")
                         ),
                menuItem("Data Source", tabName = "data", icon = icon("database"))
            )
        ),
        dashboardBody(
            tabItems(
                tabItem('about',
                        title = 'Data', status = 'primary', solidHeader = TRUE, width=3,
                        h2('Context'),
                        h4('A Program Electronic Review Management (PERM) Labor Certification issued by 
                        the Department of Labor (DOL) allows an employer to hire a foreign worker to 
                        work permanently in the United States. In most instances, the employer must 
                        obtain a certified labor from the DOL before submiting an immigration petition 
                        for the worker.'),
                        h2('Overview'),
                        h4('The U.S. has been the largest coutry for immigrants, with the steady growth 
                           of the foreign workers over the years. The purpose of this project is to
                           explore the worker demographics and their salary levels, and find out the 
                           key factors determining their approval of the visas'),
                       imageOutput('plot1')
                ),
                tabItem("trends",
                    fluidRow(
                        tabBox(width = 1200, height = 900,
                            tabPanel("By State", 
                                 fluidRow(
                                     plotOutput("barPlot_1_1", width = 1200, height = 650)
                                 )      
                            ),
                            tabPanel("By City", 
                                 fluidRow(
                                     plotOutput("barPlot_1_2", width = 1200, height = 650)
                                 )      
                            ),
                            tabPanel("By company", 
                                 fluidRow(
                                     selectInput("state_1", label = "States:", choices = state,
                                                 selected = 'ALL')
                                 ),
                                 fluidRow(
                                     plotOutput("barPlot_1_3", width = 1200, height = 650)
                                 )  
                            ),
                            tabPanel("By Type", 
                                 fluidRow(
                                     plotOutput("barPlot_1_4", width = 1200, height = 650)
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
                            plotOutput("lineplot", width = 1300, height = 500)
                        ),
                        conditionalPanel(
                            condition = "input.word == 'job'",
                            tabBox(width = 1200, height = 900,
                                   tabPanel("Distribution", 
                                            fluidRow(
                                                plotOutput("histplot", width = 1200, height = 500)
                                            )      
                                   ),
                                   tabPanel("Wage Comparison", 
                                            "The figure might not accurate because of sample selection. Also, the job titles vary
                                             from company to company and it is hard to standardize them.",
                                            fluidRow(
                                                # A static valueBox
                                                valueBox("-7.57%", "Overall decrease", icon = icon("credit-card"))
                                            ),
                                            fluidRow(
                                                plotOutput("bi_barplot", width = 1200, height = 500)
                                            )      
                                   )
                            )
                        )
                    )
                ),
                tabItem("counts",
                    fluidRow(
                        # A static valueBox
                        valueBox("67.78%", "Overall increase", icon = icon("credit-card"))
                    ),
                    fluidRow(
                        plotOutput("dumbbell_1", width = 1250, height = 650)
                    )
                ), 
                tabItem("wage",
                    fluidRow(
                        # A static valueBox
                        valueBox("5.11%", "Overall increase", icon = icon("list"), color = "purple")
                    ),
                    fluidRow(
                        plotOutput("dumbbell_2", width = 1200, height = 650)
                    )
                ),
                tabItem("processing_time", 
                    fluidRow(
                        # A static valueBox
                        valueBox("-3.8%", "Overall decrease", icon = icon("thumbs-up", lib = "glyphicon"),
                                 color = "yellow")
                    ),
                    fluidRow(
                        plotOutput("dumbbell_3", width = 1200, height = 650)
                    ) 
                ),
                tabItem("degree",
                        fluidRow(
                            sankeyNetworkOutput("sankey_1", width = 1200, height = 650)
                        )
                ), 
                tabItem("visa",
                        fluidRow(
                            sankeyNetworkOutput("sankey_2", width = 1200, height = 650)
                        )
                ),
                tabItem('data',
                        h2('Data Source'),
                        tags$a(href="https://www.foreignlaborcert.doleta.gov/performancedata.cfm", 
                               h3("UNITED STATES DEPARTMENT OF LABOR Employment & Training Administration"))
                )
            )
        )
    )
)
