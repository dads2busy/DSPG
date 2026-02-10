#Load required Packages



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



library(shinyjs)
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
jscode <- "var referer = document.referrer;
           var n = referer.includes('economic');
           var x = document.getElementsByClassName('logo');
           if (n != true) {
             x[0].innerHTML = '<a href=\"https://datascienceforthepublicgood.org/events/symposium2020/poster-sessions\">' +
                              '<img src=\"DSPG_white-01.png\", alt=\"DSPG 2020 Symposium Proceedings\", style=\"height:42px;\">' +
                              '</a>';
           } else {
             x[0].innerHTML = '<a href=\"https://datascienceforthepublicgood.org/economic-mobility/community-insights\">' +
                              '<img src=\"AEMLogoGatesColors-11.png\", alt=\"Gates Economic Mobility Case Studies\", style=\"height:42px;\">' +
                              '</a>';
           }
           "



options(rgl.useNULL = TRUE) #MUST RUN before RGL for Rayshader to work.
library(rgl)


load("Hotline_Data_V2.RData")
# call_information_Long  = drop_na(call_information_Long)
# brochure_Long = drop_na(brochure_Long)
# stats_Long = drop_na(stats_Long)
# call_information_Long = dplyr::rename(call_information_Long, "Call Counts" = Number_of_Cases)
# brochure_Long = dplyr::rename(brochure_Long, "Call Counts" = Number_of_Cases)
# stats_Long = dplyr::rename(stats_Long, "Call Counts" = Number_of_Cases)
# Tammy_Data_Long = dplyr::rename(Tammy_Data_Long, "Call Counts" = Number_of_Cases)
# Outcome_Data_Long = dplyr::rename(Outcome_Data_Long, "Call Counts" = Number_of_Cases)
# referral_Long = dplyr::rename(referral_Long, "Call Counts" = Number_of_Cases)
# Final = drop_na(Final)
# levels(Final$Classification)[levels(Final$Classification)=="CCT"] <- "211 Call Transfer"

# Final <- plyr::revalue(Final$CCT, c("old_name" = "New_Name")

# calldetails$num_of_calls <- as.numeric(ave(calldetails$start_date, calldetails$start_date, FUN = length))
# 
# 
# 
# #--------------------------------------------------------------------------------------------------------------------------------#
# 
# 
# calldetails_180 <- read_csv("calldetail0713_TotalTime-180max.csv")
# 
# 
# 
# 
# 
# cols <- c(1,4,8,17,24)
# cd_180_1<- calldetails_180[, cols]
# 
# # Create a new column named number of calls
# cd_180_1$num_of_calls <- as.numeric(ave(cd_180_1$start_date, cd_180_1$start_date, FUN = length))
# 
# 
# bx <- cd_180_1 %>% 
#   separate('start_date', into = c("month","day","year"))
# 
# 
# bx$day <- as.factor(bx$day)
# bx$day <- as.numeric(bx$day)
# bx$month <- as.numeric(bx$month)
# bx$year <- as.numeric(bx$year)
# 
# 
# bbx <- bx %>%
#   filter(year == 2020) %>%
#   group_by(day, month,num_of_calls) %>%
#   summarise(sumTT = sum(Total_Time_180max))
# 
# 
# 
# 
# 
#------------------------------------------------------------------------------------------------------------------------------#


#Sidebar menu and icons

sidebar <- dashboardSidebar(
  
  sidebarMenu(
    
    menuItem("Project Description", icon = icon("diagnoses"), tabName = "description"),
    menuItem("Sentiment Analysis", icon = icon("diagnoses"), tabName = 'sentiment'),
    menuItem("Call information Report", icon = icon("bar-chart-o"), tabName = "bar"),
    menuItem("Nice Data", icon = icon("bar-chart-o"),
             menuSubItem("Call Information",icon = icon("angle-right"), tabName = "bubble1")),
    menuItem("ICarol Data", icon = icon("bar-chart-o"), tabName = "bubble3"),
    menuItem(
      tabName = "team",
      text = "Team",
      icon = icon("user-friends")
    )
    
  )
  
)

#--------------------------------------------------------------------------------------------------------------#

body <- dashboardBody(
  useShinyjs(),
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
                # h1("2020 DSPG Hotline"),
                
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
                title = "Distribution of Brochures", 
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
                width = 9,
                title = "Number of Call per each call center", 
                closable = TRUE, 
                status = "primary", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                enable_sidebar = TRUE,
                sidebar_width = 10,
                sidebar_start_open = FALSE,
                sidebar_content = tagList(
                  selectInput(inputId = "yearx", label = strong("Year"),
                              choices = unique(bx$year),
                              selected = "2020"),
                  
                  selectInput(inputId = "monthx", label = strong("Month"),
                              choices = unique(bx$month),
                              selected = "1")
                ),
                plotlyOutput(outputId = "linex_plot")
              ),                 p("X- axis represents date of each hotlines. Y-axis represents number of calls received by each hotline. 
                                   For example, the light green denotes 211 hotlines, at January 10th, 211 hotline centers received 74 calls on that day."),
              br()
            ),
            
            fluidRow(
              
              boxPlus(
                width = 9,
                title = "Number of Calls and Total Call Time by Date", 
                closable = TRUE, 
                status = "primary", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                enable_sidebar = TRUE,
                sidebar_width = 10,
                sidebar_start_open = FALSE,
                sidebar_content = tagList(
                  selectInput(inputId = "monthx2", label = strong("Month"),
                              choices = unique(bbx$month),
                              selected = "1")
                ),
                plotlyOutput(outputId = "bubblex_plot")
              ),                   p("Y-axis is the total number of calls per each day. x - axis represents the date. The bubble size refers to the sum of total call time by day.  
                                     For example, on the January 2nd, hotline center received 157 calls and total time spent for the day is longer than the January 1st."),
              br()
            ),
            fluidRow(
              
              boxPlus(
                width = 9,
                title = "Number of Calls by Area Code", 
                closable = TRUE, 
                status = "primary", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                enable_sidebar = TRUE,
                sidebar_width = 10,
                sidebar_start_open = FALSE,
                sidebar_content = tagList(
                  
                  selectInput(inputId = "valuex", label = strong("Select Graph Scale Type"),
                              choices = c("Numeric", "Log Scale", "Square Root", "Reverse"),
                              selected = NULL, multiple = FALSE, selectize = TRUE)
                  
                ),
                plotOutput(outputId = "MAPPlot")
              ),                   p("This map shows the number of calls by area code throughout the state of Iowa."),
              br()
            )
            
    ),
    
    
    
    tabItem(tabName = "bubble3",
            
            fluidRow(
              
              boxPlus(
                
                width = 12,
                title = "Call Type (I - Carol)", 
                closable = TRUE, 
                status = "primary", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                enable_sidebar = TRUE,
                sidebar_width = 10,
                sidebar_start_open = FALSE,
                sidebar_content = tagList(
                  checkboxGroupInput(inputId = "calldata_var", label = strong("Variables to show:"),
                                     choices = unique(Final$Classification), selected = NULL)
                  
                ),
                plotlyOutput(outputId = "calldata_plot")
              ),p("This plot allows you to focus on you a specific interval of time (on the x axis), while viewing call counts (on the y axis). The user can also toggle through different call topics.")
            )
            
    ),
    tabItem(tabName = "team",
            fluidRow(
              boxPlus(
                title = "Findings",
                closable = FALSE,
                width = NULL,
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                h4("Fellow - Deepak George Thomas"),h4("Interns - Andrew Maloney, Kok Kent Chong, Xinyi Zhu"),
                h4("Team leaders - Adisak Sukul, Shawn Dorius"),
                #h3("UVA SDAD Team Members"),
                h4("Project Sponsors - Cooperative Extension Helplines")
              )
            ))
    
  )
)



#---------------------------------------------------------------------------#

ui <- dashboardPage(
  dashboardHeaderPlus(
    title = "Hotline Shiny App",
    left_menu = tagList(div(toupper("Expand the Iowa State University Extension Community Helpline Services Across the State"), style="height:35px; display:flex; align-items: center;"))),
  sidebar = sidebar,
  body = body 
) 


#---------------------------------------------------------------------------#

server <- function(input, output){
  runjs(jscode)
  
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
    
    bx$num_of_line <- as.numeric(ave(bx$skill_name, bx$skill_name, FUN = length))
    bx$day <-as.numeric(bx$day)
    bx<-subset(bx, skill_name!="Cheryl Clarke" & skill_name!="211 VM")
    
    filter(bx, year == input$yearx,
               month == input$monthx)
  })
  
  filtered_data13 <- reactive({
    filter(bbx, month == input$monthx2)
  })
  
  filtered_data14 <-reactive({
    filter(Final, Classification %in% input$calldata_var)
  })
  
  
  output$lineplot <- renderPlotly({
    
    ggplotly(ggplot(filtered_data(), 
                    aes(x = call_record, y = ave_sentiment, color = currently_speaking ))+ 
               geom_line()+ theme_bw()+ scale_color_manual(values=c("darkorange2", "dodgerblue3"))+
               labs(x = 'Call Duration (Sentences)', y = 'Average Sentiment'))
    
    
    
    
  })
  
  output$hexplot <- renderPlot({
    
    ggplot(filtered_data5()) +
      geom_hex(aes(y= ave_sentiment, x=call_record,
                   fill=currently_speaking), bins = 50)+
      facet_wrap(~currently_speaking,ncol=2)+ theme_bw()+ scale_fill_manual(values=c("darkorange2", "dodgerblue3"))+
      labs(x = 'Call Duration (Sentences)', y = 'Average Sentiment')
  })
  
  
  #---------------------------------------------------------------------------------------#
  
  #Two Plot 
  
  output$twoplot <- renderPlotly({
    if (input$value == "Facet"){
      filtered_data6() %>%
        ggplot() +
        geom_histogram(aes(x=ave_sentiment, fill=currently_speaking))+
        facet_wrap(~currently_speaking,ncol=2)+ theme_bw()+ scale_fill_manual(values=c("darkorange2", "dodgerblue3"))+
        labs(x = 'Sentiment Score', y = 'Frequency of Sentences')
    }
    else {
      filtered_data6() %>%
        ggplot() +
        geom_histogram(aes(x= ave_sentiment_dup,fill=currently_speaking), 
                       colour="grey50", alpha=0.5, position="identity")+ theme_bw()+ scale_fill_manual(values=c("darkorange2", "dodgerblue3"))+
        labs(y = 'Frequency of Sentences', x = "Sentiment Score")
    }
    
  })
  
  
  
  #---------------------------------------------------------------------------------------#
  
  
  output$calllog <- renderText({filtered_data2() %>%
      select(Call_Transcript) %>% as.character() #Interactive Transricpts
  })
  
  
  output$output <- renderPlotly({
    ggplotly(ggplot(filtered_data3(), aes(x = Topic, y = `Call Counts`)) +
               geom_bar(stat = "identity", fill = "darkorange2") +
               coord_flip() + 
               theme(legend.position = "top")+ theme_bw() +
               labs(y = 'Call Count', x = "Topic"))#Interactive Bar Chart 
  })
  
  output$outcome_plot <- renderPlotly({
    ggplotly(ggplot(filtered_data4(), aes(x = Outcome, y = `Call Counts` )) +
               geom_bar(stat = "identity", fill = "darkorange2") +
               coord_flip() + 
               theme(legend.position = "top")+ theme_bw() + #Interactive Bar Chart
               labs(y = 'Call Count', x = "Call Outcome"))
  })
  
  output$call_information_plot <- renderPlotly({
    ggplotly(ggplot(filtered_data7(), aes(x = information, y = `Call Counts` )) +
               geom_bar(stat = "identity", fill = "darkorange2") +
               coord_flip() + 
               theme(legend.position = "top")+ theme_bw() +#Interactive Bar Chart
               labs(y = 'Call Count', x = "Call Information"))
  })
  
  output$referral_plot <- renderPlotly({
    ggplotly(ggplot(filtered_data8(), aes(x = Referral, y = `Call Counts` )) +
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
    ggplotly(ggplot(filtered_data10(), aes(x = information, y = `Call Counts`)) +
               geom_bar(stat = "identity", fill = "darkorange2") +
               coord_flip() + 
               theme(legend.position = "top")+ theme_bw() + #Interactive Bar Chart
               labs(y = 'Call Count', x = "Brochures")
    )
  })
  output$stats_plot <- renderPlotly({
    ggplotly(ggplot(filtered_data11(), aes(x = Months, y = `Call Counts` )) +
               geom_bar(stat = "identity", fill = "darkorange2") +
               coord_flip() + 
               theme(legend.position = "top")+ theme_bw() + #Interactive Bar Chart
               labs(y = 'Call Count', x = "Months")
    )
  })
  
  output$linex_plot <- renderPlotly({
    
    #bx$num_of_line <- as.numeric(ave(bx$skill_name, bx$skill_name, FUN = length))
    #bx$day <-as.numeric(bx$day)
    #bx<-subset(bx, skill_name!="Cheryl Clarke" & skill_name!="211 VM")
    filtered_data12()%>%
      group_by(day, month, year,skill_name) %>%
      count(skill_name)%>%
      ungroup() %>%
      plot_ly(x = ~day, y = ~n, 
              type = 'scatter', 
              mode = 'lines', 
              color = ~skill_name,
              marker = list( opacity = 0.5, sizemode ='diameter'))%>% 
      layout(
        title = " ",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Total number of call")
      )
    
  })
  
  output$bubblex_plot <- renderPlotly({
    filtered_data13() %>%
      plot_ly(x = ~day, y = ~num_of_calls, 
              type = 'scatter', 
              mode = 'makers', 
              size = ~sumTT,
              marker = list( opacity = 0.5, sizemode ='diameter'))%>% 
      layout(
        title = " ",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Total number of call")
      )
    
  })
  

  output$MAPPlot <-  renderPlot({
    
    
    if (input$valuex == "Numeric"){
      ggplot() +
        geom_sf(data = iowa, aes(fill = n))+
        geom_sf(data = iowa2, aes(fill = n))+
        
        ggtitle(" ")+
        labs(fill = "Number of Calls")+
        theme_bw()
    }
    else if(input$valuex == "Log Scale") {
      
      
      ggplot() +
        geom_sf(data = iowa, aes(fill = log(n)))+
        geom_sf(data = iowa2, aes(fill = log(n)))+
        scale_x_continuous(expand=c(0,0)) +
        scale_y_continuous(expand=c(0,0)) +
        scale_fill_gradient(low="gold", high="deepskyblue3")+ 
        ggtitle(" ")+
        labs(fill = "Number of Calls")+
        theme_bw()
    }
    
    else if(input$valuex == "Square Root"){
      ggplot() +
        geom_sf(data = iowa, aes(fill = sqrt(n)))+
        geom_sf(data = iowa2, aes(fill = sqrt(n)))+
        scale_x_continuous(expand=c(0,0)) +
        scale_y_continuous(expand=c(0,0)) +
        scale_fill_gradient(low="gold", high="deepskyblue3")+ 
        ggtitle(" ")+
        labs(fill = "Number of Calls")+
        theme_bw()
      
    }
    
    else{
      
      ggplot() +
        geom_sf(data = iowa, aes(fill = (-n)))+
        geom_sf(data = iowa2, aes(fill = (-n)))+
        scale_x_continuous(expand=c(0,0)) +
        scale_y_continuous(expand=c(0,0)) +
        scale_fill_gradient(low="gold", high="deepskyblue3")+ 
        ggtitle(" ")+
        labs(fill = "Number of Calls")+
        theme_bw()
      
    }
    
    
    
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
