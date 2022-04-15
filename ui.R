library(shiny)
library(shinydashboard)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(dashboardPage(skin="blue",
                      #Application title
                      dashboardHeader(title = "Mental Health In Tech industry",titleWidth = 800),
                      # dashboard sidebar functions will be inserted here
                      dashboardSidebar(
                        
                        sidebarMenu(
                          menuItem('Map and Overview',tabName='map'),
                          menuItem("Information for Employeer",tabName = "employeer"),
                          menuItem("Information for Employee",tabName = "employee")
                        ),
                        radioButtons("Gender",
                                     label = "Select Gender:",
                                     choices = c('Female' = 'Female', 'Male'='Male','Others'= 'Others'),
                                     selected = "Male"),
                        sliderInput("Age",
                                    label = "Age range",
                                    min =  20,
                                    max = 85,
                                    step = 1,
                                    value = c(20,85),
                                    sep = ""),
                        radioButtons("CompanySize",
                                     label = "Select Company Size:",
                                     choices = c("1-5"="1-5",
                                       "6-25" = "6-25",
                                                 "26-100" = "26-100",
                                                 "100-500" = "100-500",
                                                 "500-1000" = "500-1000",
                                                 "More than 1000" = "More than 1000"),
                                     selected = "500-1000"),
                        uiOutput("typeSelectOutput"),
                        radioButtons("Interfere",
                                     label = "Mental health Interfere Level",
                                     choices =  c("Never" = "Never",
                                                  "Often" = "Ofter",
                                                  "Rarely" = "Rarely",
                                                  "Sometimes" = "Sometimes"
                                     ),
                                     selected = "Never")
                        
                      ),
                      # functions that must go in the body of the dashboard.
                      dashboardBody(
                        tabItems(
                          tabItem(tabName = "map",
                                  h3("Participants Country wise"),
                                  plotOutput('count'),
                                  h3("Distribution of participants"),
                                  plotlyOutput("world")
                                  
                          ),
                  
                          
                          tabItem(tabName = "employeer",
                                  h3("Mental Health"),
                                  plotOutput("mentalhealth"),
                                  h3("Physical Health"),
                                  plotOutput('physicalhealth')
                          ),
                          
                          tabItem(tabName = "employee",
                                  h3("Affect of company size on mental health"),
                                  plotOutput("companysizeaffect"),
                                  br(),
                                  h3("Do employee seek for help"),
                                  plotOutput("seekhelp")
                                  
                          )
                          
                        )
                      )
))
