#Load required Packages

packages <- c("dplyr", "purrr", "readr", "stringr", "readr", "readxl",
              "magrittr", "stringr", "ggplot2", "shiny", "sentimentr",
              "shinythemes", "shinydashboard", "shinydashboardPlus", "plotly")

for (pkgs in packages){
  if(!require(pkgs, character.only = TRUE)){ # Condition 
    install.packages(pkgs) # Install if not
    library(pkgs) # Load if installed
  }
}


#-----------------------------------------------------------------------------------------#

completex <- read_csv("completex_text.csv")

transcripts <- read_csv("rough_data_trans.csv")

Tammy_Data <- read_excel("Tammy_Data.xlsx", sheet = "Call Topic")

Outcome_Data <- read_excel("Tammy_Data.xlsx", sheet = "Outcome")

transcripts$Call_Number <- as.factor(transcripts$Call_Number) #change to factor 


#-----------------------------------------------------------------------------------------#


library(tidyr)

Tammy_Data_Long <- gather(Tammy_Data, Month_and_Annual, Number_of_Cases, October:Annual, factor_key = TRUE )

Tammy_Data_Long$Topic <- as.factor(Tammy_Data_Long$Topic)



Outcome_Data_Long <- gather(Outcome_Data, Month_and_Annual, Number_of_Cases, October:Annual, factor_key = TRUE)


#-----------------------------------------------------------------------------------------#



shinyApp(
  ui = dashboardPagePlus(
    header = dashboardHeaderPlus(
      enable_rightsidebar = TRUE,
      rightSidebarIcon = "gears"
    ),
    
    
    sidebar = dashboardSidebar(
      
      sidebarMenu(
        
        menuItem("Sentiment Analysis",icon = icon("dashboard"), tabName = 'sentiment'),
        
        menuItem("Call Topic", icon = icon("dashboard"), tabName = "calltopic"),
        
        menuItem("Outcome", icon = icon("dashboard"), tabName = "outcome"))
      ),
    
    #---------------------------------------------------------------------------------------------------------------------------------#
    
    body = dashboardBody(
      
      tabItems(
        
        tabItem(tabName = "sentiment",
                fluidRow(
                  
                  box(title = "Sentiment Analysis", status = "primary", height = 600, solidHeader = TRUE,
                      plotOutput(outputId = "lineplot", width = "100%"))),
                
                fluidRow(
                  
                  box(title = "Call Transcript", status = "warning", height = 350, solidHeader = TRUE,
                      textOutput("calllog"), style = "height:300px; overflow-y: scroll;"))
                ),
        
        tabItem(tabName = "calltopic",
                fluidRow(
                  
                  box(title = "Histogram_1", status = "primary", height = 600, solidHeader = TRUE,
                      plotOutput(outputId = "output", width = "100%")))
                ),
       
        
        tabItem(tabName = "outcome",
                h2("Outcome"),
                dashboardBody(inputPanel(selectInput('z', 'Y', choices = c( "January", "February", "March","April", "May", "June","July", "August", "September","October", "November", "December","Annual"), 
                                                     selected = "January")),
                              mainPanel(plotOutput("outcome_plot"))))
        
      )
      
    ),
    rightsidebar = rightSidebar(
      background = "dark",
      rightSidebarTabContent(
        id = 1,
        title = "Sentiment Analysis Controls",
        icon = "paint-brush",
        active = TRUE,
        
        selectInput(inputId = "call_number", label = strong("Select Call"),
                    choices = unique(completex$Call_Number),
                    selected = "6183"),
        
        selectInput(inputId = "call_number2", label = strong("Select Log"),
                    choices = unique(transcripts$Call_Number),
                    selected = "6183")
      ),
      rightSidebarTabContent(
        id = 2,
        title = "Tammy's Data",
        
        selectInput(inputId = "month", label = strong("Select a Month"),
                    choices = unique(Tammy_Data_Long$Month_and_Annual),
                    selected = "January")#,
        
        #selectInput(inputId = "topic", label = strong("Select a Topic"),
                    #choices = unique(Tammy_Data_Long$Topic),
                    #selected = "Abuse")
        
        #selectInput() # Need to Figure Out
      ),
      rightSidebarTabContent(
        id = 3,
        icon = "paint-brush",
        title = "Tab 3"
        
        #selectInput() # Need to Figure Out
        
      )
    ),
    title = "Right Sidebar"
  ),
  
  
  
  server = function(input, output){
    
    
    filtered_data <- reactive({
      filter(completex, Call_Number == input$call_number )
    })
    
  
    
    filtered_data2 <- reactive({
      filter(transcripts, Call_Number == input$call_number2)
    })
    
    
    filtered_data3 <- reactive({
      filter(Tammy_Data_Long, Month_and_Annual == input$month)
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
    
    
    output$output <- renderPlot({
      ggplot(filtered_data3(), aes(x = Topic, y = Number_of_Cases )) +
        geom_bar(stat = "identity") +
        coord_flip() + 
        theme(legend.position = "top")
    })
    
    output$outcome_plot <- renderPlot({
      ggplot(Outcome_Data, aes(x = Outcome)) +
        geom_bar(aes_string(weight = input$z)) +
        coord_flip() + 
        theme(legend.position = "top")
    })
    
  }
)


#options(shiny.reactlog=TRUE)
