#Load required Packages

options(rgl.useNULL = TRUE)

packages <- c("dplyr", "purrr", "stringr", "readxl",
              "magrittr", "stringr", "ggplot2", "shiny", "sentimentr",
              "shinythemes", "shinydashboard", "shinydashboardPlus", "plotly")

for (pkgs in packages){
  if(!require(pkgs, character.only = TRUE)){ # Condition 
    install.packages(pkgs) # Install if not
    library(pkgs) # Load if installed
  }
}

library(rgl)
library(tidyverse)
library(lubridate)
library(hrbrthemes)
library(viridisLite)
library(viridis)
library(rayshader)
library(magick)

#-----------------------------------------------------------------------------------------#

#Load speech to text data and Tammy's data

completex <- read.csv("completex_text.csv")

transcripts <- read.csv("rough_data_trans.csv")

Tammy_Data <- read_excel("Tammy_Data.xlsx", sheet = "Call Topic")

Outcome_Data <- read_excel("Tammy_Data.xlsx", sheet = "Outcome")

transcripts$Call_Number <- as.factor(transcripts$Call_Number) #change to factor 


call_information = read_excel("Tammy_Data.xlsx", sheet = "Call Information")

referral = read_excel("Tammy_Data.xlsx", sheet = "Referral")

stats = read_excel("Tammy_Data.xlsx", sheet = "STATS")

web_stats = read_excel("Tammy_Data.xlsx", sheet = "Website stats")

brochure = read_excel("Tammy_Data.xlsx", sheet = "Brochure")


#-----------------------------------------------------------------------------------------#

library(tidyr)

Tammy_Data_Long <- gather(Tammy_Data, Month_and_Annual, Number_of_Cases, October:Annual, factor_key = TRUE ) #Convert from wide to long format

Tammy_Data_Long$Topic <- as.factor(Tammy_Data_Long$Topic)#change to factor

Outcome_Data_Long <- gather(Outcome_Data, Month_and_Annual, Number_of_Cases, October:Annual, factor_key = TRUE)#Convert from wide to long format

Outcome_Data_Long$Outcome <- as.factor(Outcome_Data_Long$Outcome) #change to factor



call_information_Long <- gather(call_information, Month_and_Annual, Number_of_Cases, October:Annual, factor_key = TRUE ) #Convert from wide to long format

#Renaming first column
call_information_Long = call_information_Long %>% rename(information = names(.)[1])

call_information_Long$information  <- as.factor(call_information_Long$information) #change to factor

referral_Long = gather(referral, Month_and_Annual, Number_of_Cases, October:Annual, factor_key = TRUE ) #Convert from wide to long format

referral_Long$Referral = as.factor(referral_Long$Referral) #change to factor

web_stats_Long = gather(web_stats, Month_and_Annual, Number_of_Cases, October:Annual, factor_key = TRUE ) #Convert from wide to long format

#Renaming first column
web_stats_Long = web_stats_Long %>% rename(information = names(.)[1])

web_stats_Long$information  <- as.factor(web_stats_Long$information) #change to factor

brochure_Long = gather(brochure, Month_and_Annual, Number_of_Cases, October:Annual, factor_key = TRUE ) #Convert from wide to long format

brochure_Long = brochure_Long %>% rename(information = names(.)[1])


brochure_Long$information  <- as.factor(brochure_Long$information) #change to factor

stats_Long = gather(stats, Years, Number_of_Cases, FY2015:FY2020, factor_key = TRUE ) #Convert from wide to long format

#Renaming first column
stats_Long = stats_Long %>% rename(Months = names(.)[1])

stats_Long$Months  <- as.factor(stats_Long$Months) #change to factor
#--------------------------------------------------------------------------------------------------------------#

completex$ave_sentiment_dup <- completex$ave_sentiment


#--------------------------------------------------------------------------------------------------------------#


calldetails<- read.csv("call-detail20200713.csv")
head(calldetails)

data <- calldetails %>% 
  group_by(start_date) %>%
  mutate(date_count= n()) %>%
  mutate(sum_total_time= sum(Total_Time))


b <- calldetails %>% separate('start_date', into = c("month","day","year"))


#Making line plot

c <-b %>%
  group_by(day,month, year)%>% 
  summarise(sumTotalTime = sum(Total_Time))




d <-b %>%
  group_by(day,month, year, Total_Time)%>% 
  summarise(sumTotalTime = sum(Total_Time))


#--------------------------------------------------------------------------------------------------------------#
library(sf)

df <- read.csv("call-detail20200713.csv")

df$ANI_DIALNUM <- as.character(df$ANI_DIALNUM)
df$contact_name <- as.character(df$contact_name)


df$ANI_DIALNUM <- gsub("(\\d{3})(\\d{3})(\\d{4})$","\\1-\\2-\\3",df$ANI_DIALNUM)
df$contact_name <- gsub("(\\d{3})(\\d{3})(\\d{4})$","\\1-\\2-\\3",df$contact_name)

area_code_IA <- df 


area_code_IA <- separate(area_code_IA, col = ANI_DIALNUM, into = c("NPA","second", "third"), sep = "-", remove = FALSE)

area_code_IA1 <- area_code_IA[area_code_IA$NPA == "712", ]
area_code_IA2 <- area_code_IA[area_code_IA$NPA == "641", ]
area_code_IA3 <- area_code_IA[area_code_IA$NPA == "563", ]
area_code_IA4 <- area_code_IA[area_code_IA$NPA == "515", ]
area_code_IA5 <- area_code_IA[area_code_IA$NPA == "319", ]


area_code_IA <- rbind(area_code_IA1, area_code_IA2, area_code_IA3, area_code_IA4, area_code_IA5)

area_code_IA <- area_code_IA %>% select(-second, -third)


df_x <- area_code_IA %>%
  group_by(NPA) %>%
  count()


USA <- st_read("/Hotline_Shiny/AreaCode/AreaCode.shp")

names(USA)


iowa <- USA %>% filter(NPA %in% c("712", "641", "563", "515", "319"))

iowa <- left_join(iowa, df_x, by = "NPA")

iowa2 <- iowa %>% filter(NPA == '563')



#--------------------------------------------------------------------------------------------------------------#


#Sidebar menu and icons

sidebar <- dashboardSidebar(
  
  sidebarMenu(
    menuItem("Project Description", icon = icon("diagnoses"), tabName = "description"),
    menuItem("Sentiment Analysis", icon = icon("diagnoses"), tabName = 'sentiment'),
    menuItem("Call information Panel", icon = icon("bar-chart-o"), tabName = "bar"),
    menuItem("Call Numbers Panel", icon = icon("bar-chart-o"), tabName = "bubble")
    
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
                  p("Create an analytic strategy that maximizes the actionable insights yielded from the hotline data."),
                  
                  h2("Project Goals"),
                  p("Project goals include developing a system for extension that provides:
                    a) timely analysis of call logs, chats, and emails associated with the helplines;
                    b) insights about the needs of Iowan's by time, place, and topic;
                    c) additional infrastructure to support Extension Specialists that work with the helplines."),
                  
                  h2("Our Approach"),
                  p("- Review current hotline data architecture
                     - Collect current hotline data as well as other data sources
                     - Develope Shiny Dashboard application"),
                  
                  h2("Ethical Considerations"),
                  p("We took the utmost caution when it came to the privacy of our clients data.")
                )
              )
            
      ),
    
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
    
    tabItem(tabName = "bar",
            
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
            ),
            
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
            ),
            
            fluidRow(
              
              boxPlus(
                width = 7,
                title = "Distribution of Call Information by Date", 
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
              )
            ),
            
            fluidRow(
              
              boxPlus(
                width = 7,
                title = "Distribution of Referral by Date", 
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
              )
            ),
            
            fluidRow(
              
              boxPlus(
                width = 7,
                title = "Distribution of Web Statistics by Date", 
                closable = TRUE, 
                status = "primary", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                enable_sidebar = TRUE,
                sidebar_width = 10,
                sidebar_start_open = FALSE,
                sidebar_content = tagList(
                  selectInput(inputId = "month5", label = strong("Select a Month or Annual Report"),
                              choices = unique(web_stats_Long$Month_and_Annual),
                              selected = "January")
                ),
                plotlyOutput(outputId = "web_stats_plot")
              )
            ),
            
            fluidRow(
              
              boxPlus(
                width = 7,
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
              )
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
                  selectInput(inputId = "year1", label = strong("Year"),
                              choices = unique(stats_Long$Years),
                              selected = "FY2015")
                ),
                plotlyOutput(outputId = "stats_plot")
              )
            )
            
    ),
    
    tabItem(tabName = "bubble",
            
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
              )
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
              )
            ),
            
            fluidRow(
              rglwidgetOutput("MAPPlot")
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
  
  output$call_information_plot <- renderPlotly({
    ggplotly(ggplot(filtered_data7(), aes(x = information, y = Number_of_Cases )) +
               geom_bar(stat = "identity", fill = "darkorange2") +
               coord_flip() + 
               theme(legend.position = "top")+ theme_bw()) #Interactive Bar Chart
  })
  
  output$referral_plot <- renderPlotly({
    ggplotly(ggplot(filtered_data8(), aes(x = Referral, y = Number_of_Cases )) +
               geom_bar(stat = "identity", fill = "darkorange2") +
               coord_flip() + 
               theme(legend.position = "top")+ theme_bw()) #Interactive Bar Chart
  })
  
  output$web_stats_plot <- renderPlotly({
    ggplotly(ggplot(filtered_data9(), aes(x = information, y = Number_of_Cases )) +
               geom_bar(stat = "identity", fill = "darkorange2") +
               coord_flip() + 
               theme(legend.position = "top")+ theme_bw()) #Interactive Bar Chart
  })
  
  output$brochure_plot <- renderPlotly({
    ggplotly(ggplot(filtered_data10(), aes(x = information, y = Number_of_Cases )) +
               geom_bar(stat = "identity", fill = "darkorange2") +
               coord_flip() + 
               theme(legend.position = "top")+ theme_bw()) #Interactive Bar Chart
  })
  output$stats_plot <- renderPlotly({
    ggplotly(ggplot(filtered_data11(), aes(x = Months, y = Number_of_Cases )) +
               geom_bar(stat = "identity", fill = "darkorange2") +
               coord_flip() + 
               theme(legend.position = "top")+ theme_bw()) #Interactive Bar Chart
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
    
    plot_gg(gg2, width = 5, height = 3, scale = 300, multicore = TRUE, windowsize = c(500, 300))
    rglwidget()
    
  })
}


shinyApp(ui = ui, server = server) #Run Shiny App
