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

#Load speech to text data and Tammy's data

completex <- read_csv("completex_text.csv")

transcripts <- read_csv("rough_data_trans.csv")

Tammy_Data <- read_excel("Tammy_Data.xlsx", sheet = "Call Topic")

Outcome_Data <- read_excel("Tammy_Data.xlsx", sheet = "Outcome")

transcripts$Call_Number <- as.factor(transcripts$Call_Number) #change to factor 


#-----------------------------------------------------------------------------------------#


library(tidyr)

Tammy_Data_Long <- gather(Tammy_Data, Month_and_Annual, Number_of_Cases, October:Annual, factor_key = TRUE ) #Convert from wide to long format

Tammy_Data_Long$Topic <- as.factor(Tammy_Data_Long$Topic)#change to factor

Outcome_Data_Long <- gather(Outcome_Data, Month_and_Annual, Number_of_Cases, October:Annual, factor_key = TRUE)#Convert from wide to long format

Outcome_Data_Long$Outcome <- as.factor(Outcome_Data_Long$Outcome) #change to factor

#--------------------------------------------------------------------------------------------------------------#

completex$ave_sentiment_dup <- completex$ave_sentiment


#

#Sidebar menu and icons

sidebar <- dashboardSidebar(
  
  sidebarMenu(
    menuItem("Sentiment Analysis", icon = icon("diagnoses"), tabName = 'sentiment'),
    menuItem("Call Topic", icon = icon("bar-chart-o"), tabName = "calltopic"),
    menuItem("Outcome", icon = icon("bar-chart-o"), tabName = "outcome")
  )
  
)

#--------------------------------------------------------------------------------------------------------------#

#Many body with multiple panels

body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'sentiment',
            
            fluidRow(
              
              boxPlus(
                width = 7,
                title = "Sentiment Analysis", 
                closable = TRUE, 
                status = "primary", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                enable_sidebar = TRUE,
                sidebar_width = 8,
                sidebar_start_open = FALSE,
                sidebar_content = tagList(
                  selectInput(inputId = "call_number", label = strong("Select Call"),
                              choices = unique(completex$Call_Number),
                              selected = "6183")
                ),
                plotlyOutput(outputId = "lineplot"), style = "height:400px"
              )
            ),
            
            fluidRow(
              
              boxPlus(
                width = 7,
                title = "Sentiment Analysis Score Hex", 
                closable = TRUE, 
                status = "primary", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                enable_sidebar = TRUE,
                sidebar_width = 8,
                sidebar_start_open = FALSE,
                sidebar_content = tagList(
                  selectInput(inputId = "call_number3", label = strong("Select Call"),
                              choices = unique(completex$Call_Number),
                              selected = "6183")
                ),
                plotOutput(outputId = "hexplot"), style = "height:400px"
              )
            ),
            
            fluidRow( #Two Graph Developement
              
              boxPlus(
                width = 7,
                title = "Sentiment Analysis Score Histograms", 
                closable = TRUE, 
                status = "primary", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                enable_sidebar = TRUE,
                sidebar_width = 10,
                sidebar_start_open = FALSE,
                sidebar_content = tagList(
                  selectInput(inputId = "call_number4", label = strong("Select Call"),
                              choices = unique(completex$Call_Number),
                              selected = "6183"),
                  
                  selectInput(inputId = "value", label = strong("Select Graph Type"),
                              choices = c("Facet", "Overlayed"),
                              selected = NULL, multiple = FALSE, selectize = TRUE)
                  
                ),
                plotlyOutput(outputId = "twoplot"), style = "height:400px"
              )
            ),
            
            fluidRow(
              
              boxPlus(
                width = 7,
                title = "Call Transcript", 
                closable = TRUE, 
                status = "warning", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                enable_sidebar = TRUE,
                sidebar_width = 8,
                sidebar_start_open = FALSE,
                sidebar_content = tagList(
                  selectInput(inputId = "call_number2", label = strong("Select Log"),
                              choices = unique(transcripts$Call_Number),
                              selected = "6183")
                ),
                textOutput("calllog"), style = "height:300px; overflow-y: scroll;")
              )
            ),
    
    tabItem(tabName = 'calltopic',
            
            fluidRow(
              
              boxPlus(
                width = 7,
                title = "Distribution of Call Topics by Date", 
                closable = TRUE, 
                status = "primary", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                enable_sidebar = TRUE,
                sidebar_width = 10,
                sidebar_start_open = FALSE,
                sidebar_content = tagList(
                  selectInput(inputId = "month", label = strong("Select a Month or Annual Report"),
                              choices = unique(Tammy_Data_Long$Month_and_Annual),
                              selected = "January")
                ),
                plotlyOutput(outputId = "output")
              )
            )
          ),
    
    tabItem(tabName = 'outcome',
            
            fluidRow(
              
              boxPlus(
                width = 7,
                title = "Distribution of Call Outcomes by Date", 
                closable = TRUE, 
                status = "primary", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                enable_sidebar = TRUE,
                sidebar_width = 10,
                sidebar_start_open = FALSE,
                sidebar_content = tagList(
                  selectInput(inputId = "month2", label = strong("Select a Month or Annual Report"),
                              choices = unique(Outcome_Data_Long$Month_and_Annual),
                              selected = "January")
                ),
                plotlyOutput(outputId = "outcome_plot")
              )
            )
            
            
    )
    
    )
)


#---------------------------------------------------------------------------#

ui <- dashboardPage(
  dashboardHeader(title = "Hotline Shiny App"),
  sidebar = sidebar,
  body = body
)


#---------------------------------------------------------------------------#

server <- function(input, output){
  
  
  filtered_data <- reactive({
    filter(completex, Call_Number == input$call_number ) #Reactive
  })
  
  
  filtered_data2 <- reactive({
    filter(transcripts, Call_Number == input$call_number2) #Reactive
  })
  
  
  filtered_data3 <- reactive({
    filter(Tammy_Data_Long, Month_and_Annual == input$month) #Reactive
  })
  
  
  filtered_data4 <- reactive({
    filter(Outcome_Data_Long, Month_and_Annual == input$month2) #Reactive
  })
  
  
  filtered_data5 <- reactive({
    filter(completex, Call_Number == input$call_number3) #Reactive Will need to move up for organization
  })
  
  
  filtered_data6 <- reactive({
    filter(completex, Call_Number == input$call_number4) #Reactive Will need to move up for organization
  })
  
  
  output$lineplot <- renderPlotly({
    
    ggplotly(ggplot(filtered_data(), 
           aes(x = call_record, y = ave_sentiment, color = currently_speaking ))+ 
      geom_line()+ theme_bw()+ scale_color_manual(values=c("darkorange2", "dodgerblue3"))) #Interactive line Plot
    
    
  })
  
  output$hexplot <- renderPlot({
    
    ggplot(filtered_data5()) +
              geom_hex(aes(y= ave_sentiment, x=call_record,
                           fill=currently_speaking), bins = 50)+
              facet_wrap(~currently_speaking,ncol=2)+ theme_bw()+ scale_fill_manual(values=c("darkorange2", "dodgerblue3"))
  })
  
  
  #---------------------------------------------------------------------------------------#
  
  #Two Plot 
  
  output$twoplot <- renderPlotly({
    if (input$value == "Facet"){
      filtered_data6() %>%
        ggplot() +
          geom_histogram(aes(x=ave_sentiment, fill=currently_speaking))+
            facet_wrap(~currently_speaking,ncol=2)+ theme_bw()+ scale_fill_manual(values=c("darkorange2", "dodgerblue3"))
    }
    else {
      filtered_data6() %>%
        ggplot() +
          geom_histogram(aes(x= ave_sentiment_dup,fill=currently_speaking), 
                       colour="grey50", alpha=0.5, position="identity")+ theme_bw()+ scale_fill_manual(values=c("darkorange2", "dodgerblue3"))
    }
    
  })
  
  

  #---------------------------------------------------------------------------------------#
  
  
  output$calllog <- renderText({filtered_data2() %>%
      select(Call_Transcript) %>% 
      as.character() #Interactive Transricpts
  })
  
  
  output$output <- renderPlotly({
    ggplotly(ggplot(filtered_data3(), aes(x = Topic, y = Number_of_Cases)) +
      geom_bar(stat = "identity", fill = "darkorange2") +
      coord_flip() + 
      theme(legend.position = "top")+ theme_bw()) #Interactive Bar Chart 
  })
  
  output$outcome_plot <- renderPlotly({
    ggplotly(ggplot(filtered_data4(), aes(x = Outcome, y = Number_of_Cases )) +
      geom_bar(stat = "identity", fill = "darkorange2") +
      coord_flip() + 
      theme(legend.position = "top")+ theme_bw()) #Interactive Bar Chart
  })
}
  

shinyApp(ui = ui, server = server) #Run Shiny App

