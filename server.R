###########################################################
### Importing Library ###

library('shiny')
library('tidyverse')
library('stringr')
library('shinydashboard')
library('plotly')
library('shiny')
library('shinyjs')
library('ggplot2')
library("DT")
library('dplyr')
library('markdown')
library('zoo')
library('tidyr')
library('rworldmap')
library('rnaturalearth')
library('rnaturalearthdata')
###########################################################3
####  Data Cleaning  ####
#Reading the dataset
mental_health <- read.csv("survey.csv")

str(mental_health)

#Statistical summary of data
summary(mental_health)

#Missing Value check
sapply(mental_health, function(x) sum(is.na(x)))

#Comments column 
mental_health$comments <- NULL

#Replacing missing values
mental_health$state <- mental_health$state %>% replace_na("NA")
mental_health$work_interfere <- mental_health$work_interfere %>% replace_na("NA")
mental_health$self_employed <- mental_health$self_employed %>% replace_na("NA")

# check for missing values
sapply(mental_health, function(x) sum(is.na(x)))


#accuracy check
str(mental_health)

#categorical variables

table(mental_health['no_employees'])
table(mental_health['Gender'])

#grouping the gender column
mental_health$Gender <- as.character(mental_health$Gender)
mental_health$Gender <- tolower(mental_health$Gender)

Male <- c('m', 'male', 'male-ish', 'maile', 'cis male', 'mal', 'male (cis)', 
          'make', 'male ', 'man', 'msle', 'mail', 'malr', 'cis man','Mle')


Female <- c('female', 'cis female', 'f', 'woman', 'femake', 'female ',
            'cis-female/femme', 'female (cis)', 'femail')

Others <- c('ostensibly male, unsure what that really means','others','a little about you','Agender','androgyne',  
            'guy (-ish) ^_^', 'male leaning androgynous','queer','p','all','trans-female', 'trans woman',
            'female (trans)','something kinda male?','neuter','queer/she/they', 'non-binary', 'nah', 'enby', 
            'fluid', 'genderqueer')


Gender_new <- as.vector(mental_health$Gender)  
Gender_new <- sapply(as.vector(Gender_new), function(x) if(x %in% Male) "Male" else x)
Gender_new <- sapply(as.vector(Gender_new), function(x) if(x %in% Female) "Female" else x)
Gender_new <- sapply(as.vector(Gender_new), function(x) if(x %in% Others) "Others" else x)

mental_health$Gender<- Gender_new


#numerical variables
summary(mental_health)

#histogram
hist(mental_health$Age)


#removing outliers
mental_health<-mental_health[!(mental_health$Age<0 | mental_health$Age >100),]


#cleaned data
head(mental_health)
mental_health$no_employees <- as.factor(mental_health$no_employees)
mental_health$seek_help <- as.factor(mental_health$seek_help)
##########################################################

### Server ###

shinyServer(function(input, output) {
  
  #Country list
  output$typeSelectOutput <- renderUI({
    selectInput("typeInput","Select country",
                sort(unique(mental_health$Country)),
                multiple = TRUE,
                selected = c("Canada","United States","United Kingdom"))
    
  })
  
  output$count <- renderPlot({
    plot <- mental_health %>% 
      filter(Age>input$Age[1], Age<input$Age[2]) %>%
      filter(Country %in% input$typeInput) %>%
      ggplot(aes(x = Country, fill = Country)) + geom_bar()
    plot
  })
  
  output$world <- renderPlotly({
    world <- ne_countries(scale = "medium", returnclass = "sf")
    world_modified <- world %>% 
      mutate(my_selection = ifelse(admin %in% mental_health$Country,"country", NA))
    plot <- ggplot(data = world_modified) +
      geom_sf(aes(fill=my_selection)) + theme_classic()
    plot
  })
  
  # physical health
  output$physicalhealth <- renderPlot({
    if(input$Interfere == "Never"){
      plot <- mental_health %>% 
        filter(work_interfere == "Never") %>%
        filter(Age>input$Age[1], Age<input$Age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == input$Gender) %>%
        filter(no_employees == input$CompanySize) %>%
        ggplot(aes(x=phys_health_consequence)) + geom_bar(fill = "steelblue")+  coord_flip()+
        xlab("Physical Health Conerns") + 
        ylab("Number of Employees") 
    }
    else if(input$Interfere == "Often"){
      plot <- mental_health %>% 
        filter(work_interfere == "Often") %>%
        filter(Age>input$Age[1], Age<input$Age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == input$Gender) %>%
        filter(no_employees == input$CompanySize) %>%
        ggplot(aes(x=phys_health_consequence)) + geom_bar(fill = "steelblue")+  coord_flip()+
        xlab("Physical Health Conerns") + 
        ylab("Number of Employees") 
    }
    else if(input$Interfere == "Rarely"){
      plot <- mental_health %>% 
        filter(work_interfere == "Rarely") %>%
        filter(Age>input$Age[1], Age<input$Age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == input$Gender) %>%
        filter(no_employees == input$CompanySize) %>%
        ggplot(aes(x=phys_health_consequence)) + geom_bar(fill = "steelblue")+  coord_flip()+
        xlab("Physical Health Conerns") + 
        ylab("Number of Employees") 
    }
    else {
      plot <- mental_health %>% 
        filter(work_interfere == "Sometimes") %>%
        filter(Age>input$Age[1], Age<input$Age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == input$Gender) %>%
        filter(no_employees == input$CompanySize) %>%
        ggplot(aes(x=phys_health_consequence)) + geom_bar(fill = "steelblue")+  coord_flip()+ 
        xlab("Physical Health Conerns") + 
        ylab("Number of Employees") 
    }
    plot
    
  })    
  
  #Mental Health
  output$mentalhealth <- renderPlot({
    
    if(input$Interfere == "Never"){
      plot <- mental_health %>% 
        filter(work_interfere == "Never") %>%
        filter(Age>input$Age[1], Age<input$Age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == input$Gender) %>%
        filter(no_employees == input$CompanySize) %>%
        ggplot(aes(x=mental_health_consequence)) + geom_bar(fill = "steelblue")+  coord_flip()+
        xlab("Mental Health Conerns") + 
        ylab("Number of Employees") 
    }
    else if(input$Interfere == "Often"){
      plot <- mental_health %>% 
        filter(work_interfere == "Often") %>%
        filter(Age>input$Age[1], Age<input$Age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == input$Gender) %>%
        filter(no_employees == input$CompanySize) %>%
        ggplot(aes(x=mental_health_consequence)) + geom_bar(fill = "steelblue")+  coord_flip()+
        xlab("Mental Health Conerns") + 
        ylab("Number of Employees")  
    }
    else if(input$Interfere == "Rarely"){
      plot <- mental_health %>% 
        filter(work_interfere == "Rarely") %>%
        filter(Age>input$Age[1], Age<input$Age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == input$Gender) %>%
        filter(no_employees == input$CompanySize) %>%
        ggplot(aes(x=mental_health_consequence)) + geom_bar(fill = "steelblue")+  coord_flip()+
        xlab("Mental Health Conerns") + 
        ylab("Number of Employees")  
    }
    else {
      plot <- mental_health %>% 
        filter(work_interfere == "Sometimes") %>%
        filter(Age>input$Age[1], Age<input$Age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == input$Gender) %>%
        filter(no_employees == input$CompanySize) %>%
        ggplot(aes(x=mental_health_consequence)) + geom_bar(fill = "steelblue")+  coord_flip()+
        xlab("Mental Health Conerns") + 
        ylab("Number of Employees") 
    }
    plot
    
  })    
  
  #company size affect on mental health
  output$companysizeaffect <- renderPlot({
    
    plot <- mental_health %>% 
      filter(Age>input$Age[1], Age<input$Age[2]) %>%
      filter(Country %in% input$typeInput) %>%
      filter(Gender == input$Gender) %>%
      ggplot(aes(x = no_employees, fill = treatment)) + geom_bar(position = "dodge")+
      xlab("Company Size") + 
      ylab("Number of Employees") 
    plot
    
  })
  
  
  #seek help
  output$seekhelp <- renderPlot({
    plot <- mental_health %>% 
      filter(Age>input$Age[1], Age<input$Age[2]) %>%
      filter(Country %in% input$typeInput) %>%
      filter(Gender == input$Gender) %>%
      filter(no_employees == input$CompanySize) %>%
      ggplot(aes(x = no_employees, fill = seek_help)) + geom_bar(position = "dodge") +
      xlab("Company Size") + 
      ylab("Number of Employees") 
    plot
    
  })
  
  
})



