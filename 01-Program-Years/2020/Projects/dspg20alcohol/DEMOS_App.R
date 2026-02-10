#Load required Packages
#setwd("C:/Users/dgthomas/Documents")

library(DSPG)
packages <- c("plyr", "dplyr", "purrr", "readr", "stringr", "readr", "readxl",
              "magrittr", "stringr", "ggplot2", "shiny", "sentimentr",
              "shinythemes", "shinydashboard", "shinydashboardPlus", "plotly", "prophet","data.table", "dygraphs","ggthemes","lubridate","devtools","sf","leaflet","tidyr","tibble")

for (pkgs in packages){
  if(!require(pkgs, character.only = TRUE)){ # Condition
    install.packages(pkgs) # Install if not
    library(pkgs) # Load if installed
  }
}

load("demos.RData")

sidebar <- dashboardSidebar(
  sidebarMenu(
    #sliderInput("size", "Sample Size:", min = 2012, max = 2019,value = 2012),
    menuItem("Alcohol Abuse - Statistics", icon = icon("bar-chart-o"), tabName = "Introduction"),
    menuItem("Forecasts", icon = icon("bar-chart-o"), tabName = "Forecasts")
    
    #textInput("name", "Enter your name:", value = "Heike"),
    #selectInput("state", "Enter your State:", choices=c("Virginia", "Oregon", "Iowa", "Other"))
  )
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'Introduction',
            fluidRow(
              boxPlus(
                width = 10,
                title = "Iowa Alcohol Sales Map",
                closable = TRUE,
                solidHeader = TRUE,
                collapsible = TRUE,
                sliderInput(inputId = 'year',label = strong("Select a year"), 
                            min = 2012, max = 2019, value = 2012,sep = ""), 
                leafletOutput(outputId = "alcohol_map")
              )
            ),
    ),
    tabItem(tabName = 'Forecasts',
            navbarPage("Datasets",
                       tabPanel(("Alcohol Sales"),
                                fluidRow(
                                  boxPlus(
                                    width = 10,
                                    title = "Iowa Alcohol Sales Forecast",
                                    closable = TRUE,
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    dygraphOutput(outputId = "alcohol_forecast")
                                  )
                                ),
                                fluidRow(
                                  boxPlus(
                                    width = 10,
                                    title = "Iowa Alcohol Sales Trend",
                                    closable = TRUE,
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    plotOutput(outputId = "alcohol_trend")
                                  )
                                )),
                       tabPanel(("Campus Town Alcohol Sales"),
                                fluidRow(
                                  boxPlus(
                                    width = 10,
                                    title = "Sales Forecast",
                                    closable = TRUE,
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    dygraphOutput(outputId = "campus_alcohol_forecast")
                                  )
                                ),
                                fluidRow(
                                  boxPlus(
                                    width = 10,
                                    title = "Campus Alcohol Sales Trend",
                                    closable = TRUE,
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    plotOutput(outputId = "campus_alcohol_trend")
                                  )
                                )),
                       tabPanel(("Alcohol Related Crashes"),
                                fluidRow(
                                  boxPlus(
                                    width = 10,
                                    title = "Alcohol Related Crashes Forecast",
                                    closable = TRUE,
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    dygraphOutput(outputId = "crash_forecast")
                                  )
                                ),
                                fluidRow(
                                  boxPlus(
                                    width = 10,
                                    title = "Iowa Alcohol Sales Trend",
                                    closable = TRUE,
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    plotOutput(outputId = "crash_trend")
                                  )
                                ))
            ))))

ui <- dashboardPage(
  dashboardHeader(title = "DEMOS"),
  sidebar = sidebar,
  body = body
)

server <- function(input, output,session) {
  
  filter_mapData = reactive({filter(map_Data_Unemp, year ==input$year)})
  
  output$alcohol_forecast = renderDygraph(dyplot.prophet(m, forecast))
  output$alcohol_trend = renderPlot(prophet_plot_components(m, forecast))
  output$campus_alcohol_forecast = renderDygraph(dyplot.prophet(m2, forecast2))
  output$campus_alcohol_trend = renderPlot(prophet_plot_components(m2, forecast2))
  output$crash_forecast = renderDygraph(dyplot.prophet(m3, forecast3))
  output$crash_trend = renderPlot(prophet_plot_components(m3, forecast3))
  #output$alcohol_map = renderPlot(ggplot(filter_mapData()) +
  # geom_sf(aes(fill=sumVolumeSold/population)) +
  # geom_sf_text(aes(label=COUNTY), colour = "white", size=2.5)+
  # ggtitle("Liquor Sales per county per capita"))
  # output$alcohol_map <- renderLeaflet({
  #   leaflet(mymap)%>%addProviderTiles("CartoDB")%>%addTiles(group = "OSM (default)")%>%addPolygons(fill = 0)%>%addPolygons(fillColor = ~pal(x),group = "First")%>%addPolygons(fillColor = ~pal_1(y),group = "Second")%>%addLegend(pal = pal, values = ~x,group = "First")%>%addLayersControl(baseGroups = c("OSM (default)"),overlayGroups = c("First","Second"))
  #   
  # })
  # leafletProxy("alcohol_map", data = mapData) %>%
  #   clearShapes() %>%
  #   addPolygons(fillColor =  "red",
  #               #popup = state_popup,
  #               color = "#BDBDC3",
  #               fillOpacity = 1,
  #               weight = 1)
  #output$alcohol_map <- renderLeaflet({leaflet(mymap)%>%addProviderTiles("CartoDB")%>%addTiles(group = "OSM (default)")%>%addPolygons(fill = 0)%>%addPolygons(fillColor = ~pal(x),group = "First")%>%addPolygons(fillColor = ~pal_1(y),group = "Second")%>%addLegend(pal = pal, values = ~x,group = "First")%>%addLayersControl(baseGroups = c("OSM (default)"),overlayGroups = c("First","Second"))
  # })
  #output$alcohol_map = renderLeaflet({leaflet(map_Data_Unemp)%>%addProviderTiles("CartoDB")%>%addTiles(group = "OSM (default)")%>%addPolygons(fill = FALSE, weight = 1, color = "#000000",group = "Base Map")%>%addPolygons(color = ~pal(x), weight = 1,group = "Liquor Volume Sold per Capita")%>%addLayersControl(baseGroups = c("Base Map", "Liquor Volume Sold per Capita","Unemployment Rate"))})
  
  #Working Code
  output$alcohol_map = renderLeaflet({leaflet(filter_mapData())%>%addProviderTiles("CartoDB")%>%
      addTiles(group = "OSM (default)")%>%addPolygons(fill = 0, weight = 1, color = "#000000",group = "Base Map")%>%
      addPolygons(color = ~pal(Vol_solpercap), weight = 1,group = "Alcohol Consumption")%>%
      addPolygons(color = ~pal_1(mean_ratePerYear), weight = 1,group = "Unemployment Rate")%>%
      addLayersControl(baseGroups = c("Base Map", "Alcohol Consumption","Unemployment Rate"))
  })

# observe({
#   #pal = pal
#   leafletProxy("alcohol_map",data = mapData) %>%
#     clearShapes()%>%addPolygons(fillColor = ~pal(x),group = "Liquor Volume Sold per Capita") %>% showGroup("Liquor Volume Sold per Capita")
# })
# observe({
#   #pal = pal
#   leafletProxy("alcohol_map",data = map_Data_Unemp) %>%
#     clearShapes()%>%addPolygons(fillColor = ~pal_1(y),group = "Unemployment Rate")
# })
# ##Working Code
#   observe({
#     
#     
#     leafletProxy("alcohol_map", data = filter_mapData()) %>%
#       clearShapes() %>%
#       addPolygons(color = ~pal(Vol_solpercap), weight = 1,group = "Alcohol Consumption")
#   })
#   
#   # Use a separate observer to recreate the legend as needed.
#   observe({
#     leafletProxy("alcohol_map", data = filter_mapData()) %>%
#       clearShapes() %>%
#       addPolygons(color = ~pal_1(mean_ratePerYear), weight = 1,group = "Unemployment Rate")
#   })
}
shinyApp(ui = ui, server = server)

