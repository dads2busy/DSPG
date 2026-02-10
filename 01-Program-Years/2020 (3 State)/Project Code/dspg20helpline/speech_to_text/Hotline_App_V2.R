#Load required Packages

packages <- c("dplyr", "purrr", "readr", "stringr",
              "magrittr", "stringr", "ggplot2", "shiny", "sentimentr", "shinythemes", "shinydashboard", "plotly")

for (pkgs in packages){
  if(!require(pkgs, character.only = TRUE)){ # Condition 
    install.packages(pkgs) # Install if not
    library(pkgs) # Load if installed
  }
}

#-------------------------------------------------------------------------------------------#

completex <- completex_text #from data manipulation .rmd

transcripts <- rough_data_trans # original rough_data_trans

transcripts$Call_Number <- as.factor(transcripts$Call_Number) #change to factor 

#--------------------------------------------------------------------------------------------#
#Drop down menu stuff

menu_1 <- dropdownMenu(type = "messages",
             messageItem(
               from = "Sales Dept",
               message = "Sales are steady this month."
             ),
             messageItem(
               from = "New User",
               message = "How do I register?",
               icon = icon("question"),
               time = "13:45"
             ),
             messageItem(
               from = "Support",
               message = "The new server is ready.",
               icon = icon("life-ring"),
               time = "2014-12-01"
             )
)



menu_2 <- dropdownMenu(type = "tasks", badgeStatus = "success",
             taskItem(value = 90, color = "green",
                      "Documentation"
             ),
             taskItem(value = 17, color = "aqua",
                      "Project X"
             ),
             taskItem(value = 75, color = "yellow",
                      "Server deployment"
             ),
             taskItem(value = 80, color = "red",
                      "Overall project"
             )
)



menu_3 <- dropdownMenu(type = "notifications",
             notificationItem(
               text = "5 new users today",
               icon("users")
             ),
             notificationItem(
               text = "12 items delivered",
               icon("truck"),
               status = "success"
             ),
             notificationItem(
               text = "Server load at 86%",
               icon = icon("exclamation-triangle"),
               status = "warning"
             )
)


#--------------------------------------------------------------------------------------------#

#Sidebar Stuff

sidebar <- dashboardSidebar(
  
  selectInput(inputId = "call_number", label = strong("Select Call"),
              choices = unique(completex$Call_Number),
              selected = "6183"),
  
  selectInput(inputId = "call_number2", label = strong("Select Log"),
              choices = unique(transcripts$Call_Number),
              selected = "6183")
)


#Panel's body stuff

body <- dashboardBody(
  fluidRow(
    
    box(title = "Sentiment Analysis", status = "primary", height = 600, solidHeader = TRUE,
        plotOutput(outputId = "lineplot", width = "100%"))),
  
  fluidRow(
    
    box(title = "Call Transcript", status = "warning", height = 350, solidHeader = TRUE,
        textOutput("calllog"), style = "height:300px; overflow-y: scroll;"))
  )


#---------------------------------------------------------------------------------------------#


ui <- dashboardPage(
  dashboardHeader(title = "Hotline Shiny App", menu_1, menu_2, menu_3),
  sidebar = sidebar,
  body = body
)


#---------------------------------------------------------------------------------------------#

server <- function(input, output){
  
  
  filtered_data <- reactive({
    filter(completex, Call_Number == input$call_number )
  })
  
  
  filtered_data2 <- reactive({
    filter(transcripts, Call_Number == input$call_number2)
  })
  
  
  output$lineplot <- renderPlot({
    
     ggplot(filtered_data(), 
           aes(x = call_record, y = ave_sentiment, color = currently_speaking ))+ 
      geom_line()+ theme_bw()
    
  
  })
  
  output$calllog <- renderText({filtered_data2() %>%
      select(Call_Transcript) %>% 
      as.character()
  })
}


#----------------------------------------------------------------------------------------------#

shinyApp(ui = ui, server = server)