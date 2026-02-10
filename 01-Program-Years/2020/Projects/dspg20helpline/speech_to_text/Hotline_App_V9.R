#Load required Packages

setwd("C:/Users/dgthomas/Downloads/hotline-AM_shinydev (4)/hotline-AM_shinydev/speech_to_text")

#packages <- c("dplyr", "purrr", "stringr", "readxl",
 #             "magrittr", "stringr")

#for (pkgs in packages){
 # if(!require(pkgs, character.only = TRUE)){ # Condition 
  #  install.packages(pkgs) # Install if not in set 1
   # library(pkgs) # Load if installed
  #}
#}





#packages_2 <- c("ggplot2", "shiny", "sentimentr",
#                "shinythemes", "shinydashboard", "shinydashboardPlus", "plotly")

#for (pkgs2 in packages_2){
#  if(!require(pkgs2, character.only = TRUE)){ # Condition 
#    install.packages(pkgs2) # Install if not in set 2
#    library(pkgs2) # Load if installed
#  }
#}


#packages_3 <- c("tidyverse", "lubridate", "hrbrthemes", "viridis", "viridisLite",
 #               "rayshader", "magick")

#for (pkgs3 in packages_3){
#  if(!require(pkgs3, character.only = TRUE)){ # Condition 
#    install.packages(pkgs3) # Install if not in set 3
#    library(pkgs3) # Load if installed
#  }
#}




library(dplyr)
library(purrr)
library(stringr)
library(readxl)
library(magrittr)
library(stringr)
library(ggplot2)
library(shiny)
library(sentimentr)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(tidyverse)
library(lubridate)
library(hrbrthemes)
library(viridis)
library(viridisLite)
library(rayshader)
library(magick)
library(sf)


options(rgl.useNULL = TRUE) #MUST RUN before RGL for Rayshader to work.
library(rgl)


load("Hotline_Data_V2.RData")
calldetails$num_of_calls <- as.numeric(ave(calldetails$start_date, calldetails$start_date, FUN = length))


#Sidebar menu and icons

sidebar <- dashboardSidebar(
  
  sidebarMenu(
    
    menuItem("Project Description", icon = icon("diagnoses"), tabName = "description"),
    menuItem("Sentiment Analysis", icon = icon("diagnoses"), tabName = 'sentiment'),
    menuItem("Call information Panel", icon = icon("bar-chart-o"), tabName = "bar"),
    menuItem("Nice Data", icon = icon("bar-chart-o"),
             menuSubItem("Call Information",icon = icon("angle-right"), tabName = "bubble1"),
             menuSubItem("3D Plots", icon = icon("angle-right"), tabName = "bubble2")),
    menuItem("ICarel Data", icon = icon("bar-chart-o"), tabName = "bubble3")
    
  )
  
)

#--------------------------------------------------------------------------------------------------------------#

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "description",
            fluidRow(
              boxPlus(
                title = "Project Overview",
                closable = FALSE,
                width = NULL,
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                h1("2020 DSPG Hotline"),
                
                h2("Project Description"),
                p("The Hotline system enables the citizens of Iowa to make direct contact with scientific and educational support. We have created an analytic strategy that maximizes the actionable insights yielded from the hotline data."),
                
                h2("Project Goals"),
                p("Project goals include developing a system for extension that provides:"),
					p("a) Implement sentiment analysis using recorded voice chats"),
                    p("b) Timely analysis of call logs associated with the helplines"),
                    p("c) Insights about the needs of Iowan's by time, place, and topic"),
                    p("d) Additional infrastructure to support Extension Specialists that work with the helplines."),
                
                h2("Our Approach"),
                p("Review current hotline data architecture"),
                     p("a) Collect current hotline data as well as other data sources"),
                     p("b) Develope Shiny Dashboard application"),
					 p("c) Get information and feedback from Hotline Personnel"),
                
                h2("Ethical Considerations"),
                p("We took the utmost caution when it came to the privacy of our clients data.")
              )
            )
            
    ),
    
    tabItem(tabName = 'sentiment',
            fluidRow(
              
              boxPlus(
                width = 9,
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

              ),                 p("On the x-axis, the call record shows the timeline of a call. Y-axis shows the average sentiment score of the sentence spoken at that timeframe. 
                                   The orange line represents speaker one while the blue line represents speaker two. 
                                   Together, we can observe the fluctuation in the emotion of the parties that are involved in the call"),
                                 br()
            
 
              
            ),
            
            fluidRow(
              
              boxPlus(
                width = 9,
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
              ),               p("Hex plots are used to plot sentiment score points on a timeline of a record(x-axis) 
                                 and a sentiment score (vertical axis) in the attempt to show how much the average sentiment score is affected by the timeline of a call"),
                               br()
            ),
            
            fluidRow( #Two Graph Developement
              
              boxPlus(
                width = 9,
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
              ),               p("The X axis shows the average sentiment scores and the Y axis demonstrates the number of sentences that have those scores. 
                                 The color orange represents speaker 1 and speaker 2 is represented by the color blue."),
                               br()
            ),
            
            fluidRow(column(11, 
              
              boxPlus(
                width = 10,
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
                textOutput("calllog"), style = "height:300px; overflow-y: scroll;")),
              
              column(1, actionButton( 'play_audio', 'Play Audio' ))
            )
            
            
    ),
    
    tabItem(tabName = "bar",
            
            fluidRow(
              
              boxPlus(
                width = 8,
                title = "Distribution of Call Topics", 
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
              ),
                             p("The y axis denotes the call topics while the x axis provides the number of such  calls received. 
                             The plot data is monthly in nature and call topic counts based on months can be viewed using the drop down located on the sidebar."),
                             br()
            ),
            
            fluidRow(
              
              boxPlus(
                width = 8,
                title = "Distribution of Call Outcomes", 
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
              ),               p("The y axis denotes the call outcomes while the x axis provides the number of such  calls received. 
                                  The plot data is monthly in nature and call outcome counts based on months can be viewed using the drop down located on the sidebar"),
                               br()
            ),
            
            fluidRow(
              
              boxPlus(
                width = 8,
                title = "Distribution of Call Information", 
                closable = TRUE, 
                status = "primary", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                enable_sidebar = TRUE,
                sidebar_width = 10,
                sidebar_start_open = FALSE,
                sidebar_content = tagList(
                  selectInput(inputId = "month3", label = strong("Select a Month or Annual Report"),
                              choices = unique(call_information_Long$Month_and_Annual),
                              selected = "January")
                ),
                plotlyOutput(outputId = "call_information_plot")
              ),               p("The y axis denotes the call information obtained while the x axis provides the number of such calls received. 
                                  The plot data is monthly in nature and call information counts based on months can be viewed using the drop down located on the sidebar."),
                               br()
            ),
            
            fluidRow(
              
              boxPlus(
                width = 8,
                title = "Distribution of Referral", 
                closable = TRUE, 
                status = "primary", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                enable_sidebar = TRUE,
                sidebar_width = 10,
                sidebar_start_open = FALSE,
                sidebar_content = tagList(
                  selectInput(inputId = "month4", label = strong("Select a Month or Annual Report"),
                              choices = unique(referral_Long$Month_and_Annual),
                              selected = "January")
                ),
                plotlyOutput(outputId = "referral_plot")
              ),               p("The y axis denotes the referrals provided while the x axis provides the number of such referrals. 
                                  The plot data is monthly in nature and referral counts based on months can be viewed using the drop down located on the sidebar."),
                               br()
            ),
            
         
            
            fluidRow(
              
              boxPlus(
                width = 8,
                title = "Distribution of Brochures by Date", 
                closable = TRUE, 
                status = "primary", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                enable_sidebar = TRUE,
                sidebar_width = 10,
                sidebar_start_open = FALSE,
                sidebar_content = tagList(
                  selectInput(inputId = "month6", label = strong("Select a Month or Annual Report"),
                              choices = unique(brochure_Long$Month_and_Annual),
                              selected = "January")
                ),
                plotlyOutput(outputId = "brochure_plot")
              ),               p("The y axis denotes brochure topics while the x axis provides the count of the brochures handed out. 
                                 The plot data is monthly in nature and counts based on brochure given away can be viewed using the drop down located on the sidebar."),
                               br()
            ),
            
            fluidRow(
              
              boxPlus(
                width = 8,
                title = "Statistics", 
                closable = TRUE, 
                status = "primary", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                enable_sidebar = TRUE,
                sidebar_width = 10,
                sidebar_start_open = FALSE,
                sidebar_content = tagList(
                  selectInput(inputId = "year1", label = strong("Year"),
                              choices = unique(stats_Long$Years),
                              selected = "FY2015")
                ),
                plotlyOutput(outputId = "stats_plot")
              ),               p("The x axis denotes the calls received while the x axis provides the month it was received. 
                                 The plot data is yearly in nature and call counts based on year can be viewed using the drop down located on the sidebar.
"),
                               br()
            )
            
    ),
    
    tabItem(tabName = "bubble1",
            
            fluidRow(
              
              boxPlus(
                width = 7,
                title = "Statistics", 
                closable = TRUE, 
                status = "primary", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                enable_sidebar = TRUE,
                sidebar_width = 10,
                sidebar_start_open = FALSE,
                sidebar_content = tagList(
                  selectInput(inputId = "yearx", label = strong("Year"),
                              choices = unique(c$year),
                              selected = "2020")
                ),
                plotOutput(outputId = "linex_plot")
              ),                 p("The Y-axis is the sum of total time for each call per day. The x-axis is the Date. 
                                  Each small table represents a month. 
                                  For example, when you zoom into the first small table,  it shows an increase of total time per day from Jan 15 to Jan 16."),
                                 br()
            ),
            
            fluidRow(
              
              boxPlus(
                width = 7,
                title = "Statistics", 
                closable = TRUE, 
                status = "primary", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                enable_sidebar = TRUE,
                sidebar_width = 10,
                sidebar_start_open = FALSE,
                sidebar_content = tagList(
                  selectInput(inputId = "yearx2", label = strong("Year"),
                              choices = unique(d$year),
                              selected = "2020")
                ),
                plotOutput(outputId = "bubblex_plot")
              ),                   p("GRAPHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH 12"),
                                   br()
            )
            
    ),
    
    tabItem(tabName = "bubble2",
            
            fluidRow(
              
              column(5,
                     rglwidgetOutput("MAPPlot")),
              
              column(5,
                     plotlyOutput("dscat"))
            )
            
    ),
  
  tabItem(tabName = "bubble3",
          
          fluidRow(
            
            boxPlus(
              
              width = 12,
              title = "I-Carol Datasets", 
              closable = TRUE, 
             status = "primary", 
              solidHeader = TRUE, 
              collapsible = TRUE,
              enable_sidebar = TRUE,
              sidebar_width = 10,
              sidebar_start_open = FALSE,
              sidebar_content = tagList(
                checkboxGroupInput(inputId = "calldata_var", label = strong("Variables to show:"),
                                   choices = unique(Final$Classification), selected = NULL),

              ),
              plotlyOutput(outputId = "calldata_plot")
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
  
  filtered_data7 <- reactive({
    filter(call_information_Long, Month_and_Annual == input$month3) #Reactive
  })
  
  filtered_data8 <- reactive({
    filter(referral_Long, Month_and_Annual == input$month4) #Reactive
  })
  
  filtered_data9 <- reactive({
    filter(web_stats_Long, Month_and_Annual == input$month5) #Reactive
  })
  
  filtered_data10 <- reactive({
    filter(brochure_Long, Month_and_Annual == input$month6) #Reactive
  })
  filtered_data11 <- reactive({
    filter(stats_Long, Years == input$year1) #Reactive
  })
  
  
  filtered_data12 <- reactive({
    filter(c, year == input$yearx)
  })
  
  filtered_data13 <- reactive({
    filter(d, year == input$yearx2)
  })
  
  filtered_data14 <-reactive({
    filter(Final, Classification %in% input$calldata_var)
  })
  
  
  output$lineplot <- renderPlotly({
    
    ggplotly(ggplot(filtered_data(), 
                    aes(x = call_record, y = ave_sentiment, color = currently_speaking ))+ 
               geom_line()+ theme_bw()+ scale_color_manual(values=c("darkorange2", "dodgerblue3"))+
               labs(x = 'Call Records', y = 'Average Sentiment')) #Interactive line Plot
    
    
  })
  
  output$hexplot <- renderPlot({
    
    ggplot(filtered_data5()) +
      geom_hex(aes(y= ave_sentiment, x=call_record,
                   fill=currently_speaking), bins = 50)+
      facet_wrap(~currently_speaking,ncol=2)+ theme_bw()+ scale_fill_manual(values=c("darkorange2", "dodgerblue3"))+
      labs(x = 'Call Records', y = 'Average Sentiment')
  })
  
  
  #---------------------------------------------------------------------------------------#
  
  #Two Plot 
  
  output$twoplot <- renderPlotly({
    if (input$value == "Facet"){
      filtered_data6() %>%
        ggplot() +
        geom_histogram(aes(x=ave_sentiment, fill=currently_speaking))+
        facet_wrap(~currently_speaking,ncol=2)+ theme_bw()+ scale_fill_manual(values=c("darkorange2", "dodgerblue3"))+
        labs(x = 'Average Sentiment', y = 'Count')
    }
    else {
      filtered_data6() %>%
        ggplot() +
        geom_histogram(aes(x= ave_sentiment_dup,fill=currently_speaking), 
                       colour="grey50", alpha=0.5, position="identity")+ theme_bw()+ scale_fill_manual(values=c("darkorange2", "dodgerblue3"))+
        labs(y = 'Count', x = "Average Sentiment")
    }
    
  })
  
  
  
  #---------------------------------------------------------------------------------------#
  
  
  output$calllog <- renderText({filtered_data2() %>%
      select(Call_Transcript) %>% as.character() #Interactive Transricpts
  })
  
  
  output$output <- renderPlotly({
    ggplotly(ggplot(filtered_data3(), aes(x = Topic, y = Number_of_Cases)) +
               geom_bar(stat = "identity", fill = "darkorange2") +
               coord_flip() + 
               theme(legend.position = "top")+ theme_bw() +
      labs(y = 'Call Count', x = "Topic"))#Interactive Bar Chart 
  })
  
  output$outcome_plot <- renderPlotly({
    ggplotly(ggplot(filtered_data4(), aes(x = Outcome, y = Number_of_Cases )) +
               geom_bar(stat = "identity", fill = "darkorange2") +
               coord_flip() + 
               theme(legend.position = "top")+ theme_bw() + #Interactive Bar Chart
               labs(y = 'Call Count', x = "Call Outcome"))
  })
  
  output$call_information_plot <- renderPlotly({
    ggplotly(ggplot(filtered_data7(), aes(x = information, y = Number_of_Cases )) +
               geom_bar(stat = "identity", fill = "darkorange2") +
               coord_flip() + 
               theme(legend.position = "top")+ theme_bw() +#Interactive Bar Chart
               labs(y = 'Call Count', x = "Call Information"))
  })
  
  output$referral_plot <- renderPlotly({
    ggplotly(ggplot(filtered_data8(), aes(x = Referral, y = Number_of_Cases )) +
               geom_bar(stat = "identity", fill = "darkorange2") +
               coord_flip() + 
               theme(legend.position = "top")+ theme_bw() + #Interactive Bar Chart
               labs(y = 'Call Count', x = "Referral")
             )
  })
  
  # output$web_stats_plot <- renderPlotly({
  #   ggplotly(ggplot(filtered_data9(), aes(x = information, y = Number_of_Cases )) +
  #              geom_bar(stat = "identity", fill = "darkorange2") +
  #              coord_flip() + 
  #              theme(legend.position = "top")+ theme_bw() + #Interactive Bar Chart
  #              labs(y = 'Call Count', x = "Referral")
  #   )
  # })
  
  output$brochure_plot <- renderPlotly({
    ggplotly(ggplot(filtered_data10(), aes(x = information, y = Number_of_Cases )) +
               geom_bar(stat = "identity", fill = "darkorange2") +
               coord_flip() + 
               theme(legend.position = "top")+ theme_bw() + #Interactive Bar Chart
               labs(y = 'Call Count', x = "Brochures")
    )
  })
  output$stats_plot <- renderPlotly({
    ggplotly(ggplot(filtered_data11(), aes(x = Months, y = Number_of_Cases )) +
               geom_bar(stat = "identity", fill = "darkorange2") +
               coord_flip() + 
               theme(legend.position = "top")+ theme_bw() + #Interactive Bar Chart
               labs(y = 'Call Count', x = "Months")
    )
  })
  
  output$linex_plot <- renderPlot({
    ggplot(filtered_data12(), aes(x=day, y=sumTotalTime, group=1))+ 
      geom_line(color="lightblue")+
      geom_point()+
      facet_wrap(~month)
  })
  
  output$bubblex_plot <- renderPlot({
    ggplot(filtered_data13(), aes(x=day, y=Total_Time, size=sumTotalTime, color=Total_Time)) +
      geom_point(alpha=0.5) +facet_wrap(~month) +
      scale_size(range = c(.1, 24), name="Total Time")+
      scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
      theme_ipsum() +
      theme(legend.position="middle") +
      ylab("Total Time Spend for Each Call") +
      xlab("Date") +
      theme(legend.position = "none")
  })
  
  output$MAPPlot <-  renderRglwidget({
    
    gg2 <- ggplot() +
      geom_sf(data = iowa, aes(fill = n))+
      geom_sf(data = iowa2, aes(fill = n))+
      scale_fill_viridis_c(option = "plasma")
    
    plot_gg(gg2, width = 5, height = 3, scale = 300, multicore = TRUE, windowsize = c(500, 500))
    rglwidget()
    
  })
  
  output$dscat <- renderPlotly({
    
    graph <- calldetails %>%
      separate('start_date', into = c("month","day","year")) %>%
      group_by(day,month, year, Total_Time,num_of_calls )%>%
      summarise(sumTotalTime = sum(Total_Time)) %>%
      filter(year == 2020) 
      
    plot_ly(graph, x=~day, y=~num_of_calls, 
              z=~Total_Time,
              text=~Total_Time,
              color = ~month,
              mode = 'markers',
              type='scatter3d')
  
    
  })
  
  output$calldata_plot = renderPlotly({plot_ly(filtered_data14(), x = ~X1, y= ~X2, color= ~Classification, mode="lines+markers",type='scatter') %>%
      layout(title = "Call Data",
             xaxis = list(
               rangeslider = list(type = "Date"),
               title="Date"),
             yaxis = list(title = "Number of Calls"))
  })
  
  
  #------------------------------------------------------------------------#
  
  
  #observeEvent(input$beep, {
    #js$beep()
  #})
  
  
  
  observeEvent(input$play_audio, {
    insertUI(selector = "#play_audio",
             where = "afterEnd",
             ui = tags$audio(src = "6183.wav", type = "audio/wav", autoplay = NA, controls = NA, style="display:none;")  
    )
  })
}


shinyApp(ui = ui, server = server) #Run Shiny App
