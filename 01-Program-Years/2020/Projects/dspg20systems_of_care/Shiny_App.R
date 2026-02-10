#Load required Packages

library(dplyr)
library(purrr)
library(stringr)
library(magrittr)
library(ggplot2)
library(shiny)

library(shinythemes)
library(shinydashboardPlus)
library(shinyjs)
library(plotly)
library(leaflet)
library(sf)
 #packages <- c("dplyr", "purrr", "stringr", "readxl",
              # "magrittr", "stringr", "ggplot2", "shiny", "sentimentr",
             #  "shinythemes", "shinydashboard", "shinydashboardPlus", 
             #  "plotly", "leaflet", "sf")
 
 #for (pkgs in packages){
 # if(!require(pkgs, character.only = TRUE)){ # Condition 
    # install.packages(pkgs) # Install if not
   # library(pkgs) # Load if installed
  # }
# }

library(DT)
library(shinydashboardPlus)
library(shinydashboard)
library(mapproj)
library(DSPG)
library(tidyr)
library(lubridate)
library(DSPG)
library(shinyjs)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------#

load("Syscare_Recovery_data.RData")


#data_hospitals <- hospitals #Df Change Name

#counties <- ia_counties #Df Change Name


#data_hospitals <-  data_hospitals %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "identity") #create object sf

#data_hospitals <- data_hospitals %>% filter(!is.na(beds)) #filter out NAs

#combined <- st_join(data_hospitals, st_transform(counties, crs='+proj=longlat +datum=WGS84')) #Choose correct datum the user wants to use



#county_counts <- combined %>% group_by(co_fips) %>% summarise(bed_count = sum(beds)) #Create bed count column

#county_counts <- county_counts %>% st_set_geometry(NULL) 

#county_counts = left_join(st_transform(ia_counties, crs='+proj=longlat +datum=WGS84'), county_counts, by = "co_fips") #Join dataframes

#county_counts[is.na(county_counts$bed_count), "bed_count"] = 0 # Set all NAs to 0


#county_counts <- county_counts %>% mutate(beds_percap = bed_count/popestimate2019) #Create beds_percap column


#county_counts <- county_counts %>% mutate(beds_per1000 = beds_percap*1000) #Create beds_per1000 column
#county_counts = county_counts %>% mutate(ln_beds_percap = log(beds_percap)) #Create beds_percap column


#health_clinics <- health.clinics #Df change Name


#datasets = c("Area Substance Abuse Council", "Community and Family Resources", "Southwest MHDS") #For Multi Layer Plot





#df <- recovery



# Grouping Recovery centers data by type
#Recovery_Reco <- df[df$RecoveryHousing == "yes", ]
#Recovery_In <- df[df$InpatientTreatment == "yes", ]
#Recovery_Out <- df[df$OutpatientTreatment == "yes", ]


#df2 <- meetings #read.csv('All_Meetings_Geocoded.csv')


# Grouping Meetings data by type
#Meetings_AA <- df2[df2$type == "Alcoholics Anonymous", ]
#Meetings_AAnon <- df2[df2$type == "Al-anon", ]
#Meetings_AdultChildA <- df2[df2$type == "Adult children of alcoholic", ]
#Meetings_Celebrate <- df2[df2$type == "Celebrate", ]
#Meetings_Crush <- df2[df2$type == "CRUSH", ]
#Meetings_IDRA <- df2[df2$type == "Iowa Dual Recovery Anonymous (IDRA)", ]
#Meetings_NA <- df2[df2$type == "Narcotics Anonymous", ]
#Meetings_NAnon <- df2[df2$type == "Nar-Anon", ]
#Meetings_PA <- df2[df2$type == "Pills Anonymous", ]
#Meetings_RR <- df2[df2$type == "Refuge Recovery", ]
#Meetings_Smart <- df2[df2$type == "SMART", ]



#meetings_county <- meetings %>%
  #group_by(county) %>%
  #summarise(count = n())

#ia_counties_2 <- ia_counties %>%
  #select(county, popestimate2019)

#counties <- map_data('county', region = "Iowa") 

#counties <- counties %>% 
  #rename(county = subregion)

#join county map data with employment data
#meetings_county <- left_join(ia_counties_2, meetings_county)

#Set NAs to 0 (really are 0)
#meetings_county[is.na(meetings_county)] <- 0

#Figure Per 10,000 People Rate
#meetings_county <- meetings_county %>%
  #mutate(meeting_rate = count / popestimate2019 * 10000) %>%
  #mutate(county = tolower(county))

#Merge With counties shapes
#meetings_county <- left_join(meetings_county, counties)


#topCities  <- meetings %>%
  #filter(city %in% c("Des Moines", "Sioux City", "Cedar Rapids", "Davenport",
                     #"Iowa City", "Ames", "Council Bluffs", "Dubuque"))

#topCities$city <- ordered(topCities$city, c("Ames", "Council Bluffs", "Dubuque", "Iowa City",
                                          #  "Davenport", "Cedar Rapids", "Sioux City", "Des Moines"))

#topCities_pop <- left_join(topCities,ia_cities %>% select(-geometry),by="city")


#head(df2)
#df2$county <- as.factor(df2$county)
#df2$time <- as.factor(df2$time)
#df2$ampm <- as.factor(df2$ampm)



# CODE TO DETECT ORIGIN OF LINK AND CHANGE LOGO ACCORDINGLY
jscode <- "function getUrlVars() {
                var vars = {};
                var parts = window.location.href.replace(/[?&]+([^=&]+)=([^&]*)/gi, function(m,key,value) {
                    vars[key] = value;
                });
                return vars;
            }

           function getUrlParam(parameter, defaultvalue){
                var urlparameter = defaultvalue;
                if(window.location.href.indexOf(parameter) > -1){
                    urlparameter = getUrlVars()[parameter];
                    }
                return urlparameter;
            }

            var mytype = getUrlParam('type','Empty');

            function changeLinks(parameter) {
                links = document.getElementsByTagName(\"a\");

                for(var i = 0; i < links.length; i++) {
                   var link = links[i];
                   var newurl = link.href + '?type=' + parameter;
                   link.setAttribute('href', newurl);
                 }
            }

           var x = document.getElementsByClassName('logo');

           if (mytype != 'economic') {
             x[0].innerHTML = '<div><a href=\"https://datascienceforthepublicgood.org/events/symposium2020/poster-sessions\">' +
                              '<img src=\"DSPG_black-01.png\", alt=\"DSPG 2020 Symposium Proceedings\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('dspg');
           } else {
             x[0].innerHTML = '<div><a href=\"https://datascienceforthepublicgood.org/economic-mobility/community-insights/case-studies\">' +
                              '<img src=\"AEMLogoGatesColorsBlack-11.png\", alt=\"Gates Economic Mobility Case Studies\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('economic');
           }
           "

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------#
sidebar <- dashboardSidebar(
  
  sidebarMenu(
    
    menuItem("Project Overview", icon = icon(" fa-file-text "), tabName = "description"),
    
    
    menuItem("Health and Wellbeing Resources", icon = icon("leaf"),
             menuSubItem("About", tabName = "Syscare_1x", icon = icon("angle-right")),
             menuSubItem("Substance Use", tabName = "syscare_4", icon = icon("angle-right")),  # Moved to top
             menuSubItem("Work & Community", tabName = "syscare_1", icon = icon("angle-right")),
 #            menuSubItem("Health & Wellbeing", tabName = "syscare_2", icon = icon("angle-right")),
             menuSubItem("Mental Health", tabName = "syscare_3", icon = icon("angle-right"))),
    
    menuItem("Recovery Resources", icon = icon("leaf"),
             menuSubItem("About", tabName = "recovery_1", icon = icon("angle-right")),
             menuSubItem("Housing & Services Providers", tabName = "recovery_2", icon = icon("angle-right")),
             menuSubItem("Support Group Meetings", tabName = "recovery_3", icon = icon("angle-right"))),
    
    menuItem("Acknowledgements", icon = icon("star"), tabName = "credit")
  )
  
)

#------------------------------------------------------------------------------------------------------------------------------------------------------------#

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
                h1("2020 DSPG Linkage to Care"),
                h2("Project Description"),
                p("Government programs often develop within agency silos, making it difficult to support ‘whole community’ responses to pressing problems. To combat this issue, the Iowa Linkage to Care Advisory Board was created to embrace cross-agency evidence-based policymaking in statewide prevention, treatment, and public safety response efforts. This project identified, webscraped, and spatially mapped publicly available data reflecting formal and informal ‘Systems of Care’ that support resilience related to mental health, physical health, education, workforce development, and child care. Interactive data tools and insights catalog and improve awareness of state resources in support of the Board’s ongoing efforts to address systemic change."),
                h2("Project Goals"),
                p("The goal of the project is to describe the services and resources, that when present in a community, provide the necessary elements to promote the successful recovery process of individuals living and engaging within that community."),
                h2("Our Approach"),
                p("This project utilized the Data Analytics' Division's Data Science Framework to complete a literature review, Data Discovery Workshop, and define the formal systems of care impacting those practicing substance use recovery.  This project defined formal systems of care within the indicators of improved health, abstinence from substances, improved quality of life, social connectedness, and employment & education.  This definition allowed the project to include 17datasets in its interactive tool to visualize the formal systems of care in Iowa."),
                h2("Ethical Considerations"),
                p("We are trying to be very cognizant of the concerns and privacy of vulnerable populations.")
              )
            )),
    tabItem(tabName = "Syscare_1x",
            fluidRow(
              boxPlus(
                title = "The Syscare Team",
                closable = FALSE,
                width = NULL,
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                h1("Team Members"),
            #    h2("Project Description"),
            box(width=3, 
                img(src='kishor_kumar_sridhar.jpg', width="150px", height="200px"), 
                p("Kishor Kumar Sridhar, DSPG Summer Fellow, Information Systems")),
            box(width=3, 
                img(src='andrew_maloney.jpg', width="150px", height="200px"), 
                p("Andrew Maloney, DSPG Summer Intern, Data Science")),
            box(width=3, 
                img(src='joel_vb.jpg', width="150px", height="200px"), 
                p("Joel Von Behren, DSPG Summer Intern, Data Science")),
            box(width=3, 
                img(src='matthew_voss.jpg', width="150px", height="200px"), 
                p("Matthew Voss, DSPG Summer Intern, Statistics, Computer Science, Math")),
            box(width=3, 
                img(src='heike.jpg', width="150px", height="200px"), 
                p("Heike Hofmann, Professor of Statistics and Interim Professor in Charge of the Data Science Program"))
               )
            )),
    
    
    tabItem(tabName = 'syscare_1',  #Panel 4#######################################################
            
            fluidRow(
              
              box(
                title = "Find Community Resources Near You",
                status = "warning",
                width = NULL,
                fluidRow(
                  column(
                    width = 3,
                    
                    p("This map has locations of licensed childcare providers, colleges, and Iowa Works offices, to show availability of more informal systems of care. 
                        Please click on the points to see more information about each location."),
                    
                    boxPad(
                      color = "grey"
                      
                      
                    )
                  ),
                  column(
                    width = 9,
                    leafletOutput("resource", height = "500px")
                  )
                )
              )
            )
            
            
    ),
    
    tabItem(tabName = 'syscare_2',  #Panel 4#######################################################
            
            fluidRow(
              
              box(
                title = "Find Medical Facility locations",
                status = "warning",
                width = NULL,
                fluidRow(
                  column(
                    width = 3,
                    
                    p("The first map shows various clinical health resources in Iowa, including hospitals, rural health clinics, Veterans' Affairs clinics, and medical facilities generally, which also includes facilities of specialized health care, like dentists and chiropractors. 
                      Please click on the points to see more information about each facility."),
                    
                    
                    boxPad(
                      color = "grey"
                    )
                  ),
                  column(
                    width = 9,
                    leafletOutput("health", height = "500px")
                  )
                )
              )
            ),
            
            fluidRow(
              
              box(
                title = "Medical Facility locations",
                status = "warning",
                width = NULL,
                fluidRow(
                  column(
                    width = 3,
                    
                    p("The second map shows number of hospital beds per capita throughout the state of Iowa at the county level."),
                    
                    boxPad(
                      color = "grey"
                    )
                  ),
                  column(
                    width = 9,
                    leafletOutput("health_2", height = "500px")
                  )
                )
              )
            ),
            
            fluidRow(
              
              box(
                title = "Medical Facility locations",
                status = "warning",
                width = NULL,
                fluidRow(
                  column(
                    width = 3,
                    
                    p("The third map shows number of hospital beds per capita, 
                      using a logarithmic color scale to improve interpretation, along with hospital locations in the state overlayed in the dots."),
                    
                    boxPad(
                      color = "grey"
                    )
                  ),
                  column(
                    width = 9,
                    leafletOutput("health_3", height = "500px")
                  )
                )
              )
            )
            
            
    ),
    
    tabItem(tabName = 'syscare_3',  #Panel 4#######################################################
            
            fluidRow(
              
              box(
                title = "Find Mental Health Services Near You ",
                status = "warning",
                width = NULL,
                fluidRow(
                  column(
                    width = 3,
                    
                    p("Iowa is divided into 14 Mental Health and Disability Service regions. 
                      This map shows the regional access points for those MHDS regions that have regional access points, as well as the public-facing resource directories for the Southwest Iowa MHDS region and the South Central Behavioral Health Region. 
                      Please click on the points to see more information about each facility."),
                    
                    boxPad(
                      color = "grey"
                      
                      
                    )
                  ),
                  column(
                    width = 9,
                    leafletOutput("mental", height = "500px")
                  )
                )
              )
            )
            
            
    ),
    
    tabItem(tabName = 'syscare_4',  #Panel 4#######################################################
            
            fluidRow(
              
              box(
                title = "Find Substance Use Support Near You",
                status = "warning",
                width = NULL,
                fluidRow(
                  column(
                    width = 3,
                    
                    p("This map shows locations of drug drop off locations, MAT locations, facilities managed under Iowa's Comprehensive Substance Abuse Prevention Grant, and other substance use and problem gambling recovery locations. 
                      Please click on the points to see more information about each facility."),
                    
                    boxPad(
                      color = "grey"
                      
                      
                    )
                  ),
                  column(
                    width = 9,
                    leafletOutput("substance", height = "500px")
                  )
                )
              )
            )
            
            
    ),
    
    tabItem(tabName = "recovery_1",
            fluidRow(
              boxPlus(
                title = "The Recovery Team",
                closable = FALSE,
                width = NULL,
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                h1("Team Members"),
            #    h2("Project Description"),
            box(width=3, 
                img(src='atefeh_rajabalizadeh.jpg', width="150px", height="200px"), 
                p("Atefeh Rajabalizadeh, DSPG Summer Fellow, Industrial Engineering")),
            box(width=3, 
                img(src='jessie_bustin.jpg', width="150px", height="200px"), 
                p("Jessie Bustin, DSPG Summer Intern, Data Science and Statistics")),
            box(width=3, 
                img(src='grant_durbahn.jpg', width="150px", height="200px"),
                p("Grant Durbahn, DSPG Summer Intern, Economics and Math")),
            box(width=3, 
                img(src='vikram_magal.jpg', width="150px", height="200px"), 
                p("Vikram Magal, DSPG Summer Intern, Business Analytics and Management Information Systems")),
            box(width=3, 
                img(src='second_photo_for_shawn_0.jpg', width="150px", height="200px"),                 
                p("Shawn Dorius, Associate Professor of Sociology, Demography and Community Sociology"))
              )
            )),
    
  
    
    tabItem(tabName = 'recovery_2',  
            
            fluidRow(
              
              box(
                title = "Find Recovery Resources Near You",
                status = "warning",
                width = NULL,
                fluidRow(
                  column(
                    width = 3,
                    p("This map shows the locations of substance use housing providers as well as information on inpatient and 
                      outpatient service providers in Iowa. Please click on the points to see more information about each facility."),
                    boxPad(
                      color = "grey"
                      
                      
                    )
                  ),
                  column(
                    width = 9,
                    leafletOutput("change2", height = "500px")
                  )
                )
              )
            )
            
            
    ),
    
    tabItem(tabName = 'recovery_3',  
            
            fluidRow(
              
              box(
                title = "Find A Recovery Meeting Near You",
                status = "warning",
                width = NULL,
                fluidRow(
                  column(
                    width = 3,
                    boxPad(
                      color = "grey",
                      
                      # selectInput(inputId = "county_meetings", label = strong("Select a County"),
                      #             choices = unique(df2$county),
                      #             selected = NULL),
                      
                      # selectInput(inputId = "dayz", label = strong("Select a Day"),
                      #             choices = unique(df2$day),
                      #             selected = NULL),
                      
                      #selectInput(inputId = "time1", label = strong("Select a Time"),
                                  #choices = unique(df2$time),
                                  #selected = NULL),
                      
                      #selectInput(inputId = "time2", label = strong("Select Morning or Evening"),
                                  #choices = unique(df2$ampm),
                                  #selected = NULL),
                      
                      sliderInput(inputId = 'time', label = strong("Select Meeting Times"),
                                  min = now(), max = now()+days(7), value = c(now(), now()+hours(24)))
                    )
                  ),
                  column(
                    width = 9,
                    leafletOutput("change3", height = "500px")
                  )
                )
              )
            ),
            
            fluidRow(
              
              box(
                title = "Recovery Meeting Data Analysis",
                status = "warning",
                width = NULL,
                fluidRow(
                  column(
                    width = 3,
                    boxPad(
                      color = "grey" 
                    )
                  ),
                  column(
                    width = 9,
                    plotlyOutput("change4", height = "500px")
                  )
                )
              )
            ),
            
            fluidRow(
              
              box(
                title = "Recovery Meeting Data Analysis",
                status = "warning",
                width = NULL,
                fluidRow(
                  column(
                    width = 3,
                    boxPad(
                      color = "grey" 
                    )
                  ),
                  column(
                    width = 9,
                    plotlyOutput("change5", height = "500px")
                  )
                )
              )
            ),
            
            fluidRow(
              
              box(
                title = "Recovery Meeting Data Analysis",
                status = "warning",
                width = NULL,
                fluidRow(
                  column(
                    width = 3,
                    boxPad(
                      color = "grey" 
                    )
                  ),
                  column(
                    width = 9,
                    plotlyOutput("change6", height = "500px")
                  )
                )
              )
            ),
            
            fluidRow(
              
              box(
                title = "Recovery Meeting Data Analysis",
                status = "warning",
                width = NULL,
                fluidRow(
                  column(
                    width = 3,
                    boxPad(
                      color = "grey" 
                    )
                  ),
                  column(
                    width = 9,
                    plotlyOutput("change7", height = "500px")
                  )
                )
              )
            )
            
            
    ),
    
    tabItem(tabName = "credit",
     fluidRow(
       boxPlus(
         title = "Acknowledgements",
         closable = FALSE,
         width = NULL,
         status = "warning",
         solidHeader = TRUE,
         collapsible = TRUE,
         h2("Project Sponsors"),
         p("Substance Abuse Bureau, Iowa Department of Public Health. Monica Wilke-Brown, Kevin Gabbard"),
         h2("Project Leads"),
         img(src='second_photo_for_shawn_0.jpg', width="150px", height="200px"), 
         img(src='cass_dorius_0.jpg', width="150px", height="200px"),
         img(src='heike.jpg', width = "150px", height = "200px")
#         h2("Acknowledgements"),
#         p("[Optional: You can also include external collaborators in this section or a separate section.]")
       )
     ))
    )
  
)

















#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------#

ui <- dashboardPage(
  #dashboardHeader(title = "Linkage to Care"),
  header = dashboardHeaderPlus(left_menu = tagList(div("Pilot ‘Systems of Care’ Data Infrastructure to Inform a Health Information Platform", style="height:35px; display:flex; align-items: center;"))),
  sidebar = sidebar,
  body = body,
  skin = "black"
)
# left_menu = tagList(div("Project to Verify Something Something Using Something Else plus Something...", style="height:35px; display:flex; align-items: center;"))

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------#


server <- function(input, output, session){
  runjs(jscode)
  
  #---------------------------------------------------------------------------------------------------#
  filtered_data_meetings <- reactive({
    
    
    df2 <- get_meetings(from = input$time[1], to =  input$time[2])
    
    df2
    

  })
  
  

  
  
  
  
  Meetings_AA_data <- reactive({
    df <- filtered_data_meetings() %>% 
      filter(type == "Alcoholics Anonymous")
    
    if(nrow(df) == 0) {
      df <- filtered_data_meetings()[1,]
      df$longitude <- 0
      df$latitude <- 0
    }
    df
  })
  
  
 
  
   Meetings_AAnon_data <- reactive({
     df <- filtered_data_meetings() %>%
       filter(type == "Al-anon")
     
     if(nrow(df) == 0) {
       df <- filtered_data_meetings()[1,]
       df$longitude <- 0
       df$latitude <- 0
     }
     df
   })
   
  
   Meetings_AdultChildA_data <- reactive({
     df <- filtered_data_meetings() %>%
      filter(type == "Adult children of alcoholic")
     
     if(nrow(df) == 0) {
       df <- filtered_data_meetings()[1,]
       df$longitude <- 0
       df$latitude <- 0
     }
     df
   })
  
  
   Meetings_Celebrate_data <- reactive({
     df <- filtered_data_meetings() %>%
       filter(type == "Celebrate")
     
     if(nrow(df) == 0) {
       df <- filtered_data_meetings()[1,]
       df$longitude <- 0
       df$latitude <- 0
     }
     df
   })
  
   Meetings_Crush_data <- reactive({
     df <- filtered_data_meetings() %>%
       filter(type == "CRUSH")
     if(nrow(df) == 0) {
       df <- filtered_data_meetings()[1,]
       df$longitude <- 0
       df$latitude <- 0
     }
     df
   })
   
    Meetings_IDRA_data <- reactive({
      df <- filtered_data_meetings() %>%
        filter(type == "Iowa Dual Recovery Anonymous (IDRA)")
      
      if(nrow(df) == 0) {
        df <- filtered_data_meetings()[1,]
        df$longitude <- 0
        df$latitude <- 0
      }
      df
    })
    
   
    Meetings_NA_data <- reactive({
      df <- filtered_data_meetings() %>%
        filter(type == "Narcotics Anonymous")
      
      if(nrow(df) == 0) {
        df <- filtered_data_meetings()[1,]
        df$longitude <- 0
        df$latitude <- 0
      }
      df
    })
   
   
    Meetings_NAnon_data <- reactive({
      df <- filtered_data_meetings() %>%
        filter(type == "Nar-Anon")
      
      if(nrow(df) == 0) {
        df <- filtered_data_meetings()[1,]
        df$longitude <- 0
        df$latitude <- 0
      }
      df
    })
   
   Meetings_PA_data <- reactive({
     df <- filtered_data_meetings() %>%
      filter(type == "Pills Anonymous")
     
     if(nrow(df) == 0) {
       df <- filtered_data_meetings()[1,]
       df$longitude <- 0
       df$latitude <- 0
     }
     df
   })
   
  
   Meetings_RR_data <- reactive({
     df <- filtered_data_meetings() %>%
       filter(type == "Refuge Recovery")
     
     if(nrow(df) == 0) {
       df <- filtered_data_meetings()[1,]
       df$longitude <- 0
       df$latitude <- 0
     }
     df
  }) 
  
  
   Meetings_Smart_data <- reactive({
     df <- filtered_data_meetings() %>%
      filter(type == "SMART")
     
     if(nrow(df) == 0) {
       df <- filtered_data_meetings()[1,]
       df$longitude <- 0
       df$latitude <- 0
     }
     df
  })


  
  #-----------------------------------------------------------------------------------------------------#
  
  output$resource <- renderLeaflet({
    
    one = makeIcon(
      iconUrl = "stuff/Icons/1.png",
      iconWidth = 10, iconHeight = 10
    )
    
    two = makeIcon(
      iconUrl = "stuff/Icons/2.png",
      iconWidth = 10, iconHeight = 10
    )
    
    three = makeIcon(
      iconUrl = "stuff/Icons/3.png",
      iconWidth = 10, iconHeight = 10
    )
    
    four = makeIcon(
      iconUrl = "stuff/Icons/4.png",
      iconWidth = 10, iconHeight = 10
    )
    
    five = makeIcon(
      iconUrl = "stuff/Icons/5.png",
      iconWidth = 10, iconHeight = 10
    )
    
    six = makeIcon(
      iconUrl = "stuff/Icons/6.png",
      iconWidth = 10, iconHeight = 10
    )
    
    seven = makeIcon(
      iconUrl = "stuff/Icons/7.png",
      iconWidth = 10, iconHeight = 10
    )
    
    
    
    
    datasets = c("Childcare Providers", "Colleges and Universities", "Iowa Works Offices")
    colors <- RColorBrewer::brewer.pal(n = length(datasets), name="Dark2")
    
    pal <- colorFactor(
      palette = colors,
      levels = datasets)
    
    
    
    
    # Create point map with the regional datasets colored differently
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = st_transform(ia_counties, crs='+proj=longlat +datum=WGS84'),
                  weight = 1, color="#333333") %>%
      addMarkers(lng = ~longitude, lat = ~latitude, label = ~provider_business_name, icon = one,
                 popup = paste0(childcare$provider_business_name, "<br>", childcare$search_address, "<br>", childcare$phone),
                 data = childcare,
                 group = datasets[1]) %>%
      addMarkers(lng = ~longitude, lat = ~latitude, label = ~name, icon = two,
                 popup = paste0(colleges$name, "<br>", colleges$city, "<br>", colleges$type),
                 data = colleges,
                 group = datasets[2]) %>%
      addMarkers(lng = ~Longitude, lat = ~Latitude, label = ~NAME, icon = three,
                 popup = paste0(iowaworks$NAME, "<br>", iowaworks$formatted_, "<br>", iowaworks$PHONE, "<br>", iowaworks$LINK),
                 data = iowaworks,
                 group = datasets[3]) %>%
      addLayersControl(
        overlayGroups = datasets,
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addLegend(
        position = c("bottomleft"), pal = pal, values = datasets, opacity = 0.9
      ) %>% 
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 7",
        onClick=JS("function(btn, map){ map.setZoom(7); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true, zoom: 10}); }"))) %>%
      setView(lng = -93.645733, lat = 42.026234, zoom = 7)
    
  })
  
#---------------------------------------------------------------------#
#---------------------------------------------------------------------#
  
  output$health <- renderLeaflet({
    
    one = makeIcon(
      iconUrl = "stuff/Icons/1.png",
      iconWidth = 10, iconHeight = 10
    )
    
    two = makeIcon(
      iconUrl = "stuff/Icons/2.png",
      iconWidth = 10, iconHeight = 10
    )
    
    three = makeIcon(
      iconUrl = "stuff/Icons/3.png",
      iconWidth = 10, iconHeight = 10
    )
    
    four = makeIcon(
      iconUrl = "stuff/Icons/4.png",
      iconWidth = 10, iconHeight = 10
    )
    
    five = makeIcon(
      iconUrl = "stuff/Icons/5.png",
      iconWidth = 10, iconHeight = 10
    )
    
    six = makeIcon(
      iconUrl = "stuff/Icons/6.png",
      iconWidth = 10, iconHeight = 10
    )
    
    seven = makeIcon(
      iconUrl = "stuff/Icons/7.png",
      iconWidth = 10, iconHeight = 10
    )
    
    
    
    
    datasets = c("Rural Health Clinics", "Iowa Hospitals", "VA Hospitals/Clinics", "Medical Facilities") #Remove Medical Facilities
    colors <- RColorBrewer::brewer.pal(n = length(datasets), name="Dark2")
    
    pal <- colorFactor(
      palette = colors,
      levels = datasets)
    
    
    
    
    # Create point map with the regional datasets colored differently
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = st_transform(ia_counties, crs='+proj=longlat +datum=WGS84'),
                  weight = 1, color="#333333") %>%
      addMarkers(label = ~name, icon = four,
                 popup = paste0(hospital_buildings$name),
                 data = hospital_buildings,
                 group = datasets[4]) %>%
      addMarkers(lng = ~longitude, lat = ~latitude, label = ~name, icon = one,
                 popup = paste0(health.clinics$name, "<br>", health.clinics$address),
                 data = health.clinics,
                 group = datasets[1]) %>%
      addMarkers(lng = ~longitude, lat = ~latitude, label = ~name, icon = two,
                 popup = paste0(hospitals$name, "<br>", hospitals$type, "<br>", hospitals$address, "<br>", hospitals$website),
                 data = hospitals,
                 group = datasets[2]) %>%
      addMarkers(lng = ~longitude, lat = ~latitude, label = ~facility, icon = three,
                 popup = paste0(va_medical_centers$facility, "<br>", va_medical_centers$address, "<br>", va_medical_centers$phone),
                 data = va_medical_centers,
                 group = datasets[3]) %>%
      addLayersControl(
        overlayGroups = datasets,
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addLegend(
        position = c("bottomleft"), pal = pal, values = datasets, opacity = 0.9
      ) %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 7",
        onClick=JS("function(btn, map){ map.setZoom(7); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true, zoom: 10}); }"))) %>%
      setView(lng = -93.645733, lat = 42.026234, zoom = 7)
    

  })
  
  output$health_2 <- renderLeaflet({
    
      scale_range = c(0, max(county_counts$beds_percap))
      pal <- colorNumeric("Blues", scale_range, na.color = "#aaff56", reverse=FALSE)
      county_counts %>%
        leaflet() %>%
        addTiles() %>%
        addPolygons(data = county_counts$geometry, weight = 1, color="#333333", fillColor = pal(county_counts$beds_percap), fillOpacity = 0.9, label = county_counts$co_fips) %>%
        addLegend(pal = pal,
                  values = scale_range,
                  position = "topright",
                  title =  "Number of<br>Hospital Beds<br>Per Capita", opacity = 0.9)
   
    
    
  })
  
  
  output$health_3 <- renderLeaflet({
  
      bins = c(0, 0.5, 1, 2, 4, 8, 16, 32, 64)
      bins = exp(c(-Inf, -7, -6.5, -6, -5.5, -5, -4.5, -4, -3.5, -3, log(max(county_counts$beds_percap + 0.01))))*1000
      bins = round(bins, digits = 1)
      pal = colorBin("Blues", domain = county_counts$beds_per1000, bins = bins)
      county_counts %>%
        leaflet() %>%
        addTiles() %>%
        addPolygons(data = county_counts$geometry, weight = 1, color="#333333", fillColor = pal(county_counts$beds_per1000), fillOpacity = 0.9, label = county_counts$beds_per1000) %>%
        addCircleMarkers(data = data_hospitals$geometry, radius = 1, stroke = 0.1, color = "#000000") %>%
        addLegend(pal = pal,
                  values = bins,
                  position = "topright",
                  title = "Number of<br>Hospital Beds<br>Per 1000 People", opacity = 0.9)
    
    
  })
  
#--------------------------------------------------------------#
#--------------------------------------------------------------#
  
  output$mental <- renderLeaflet({
    
    one = makeIcon(
      iconUrl = "stuff/Icons/1.png",
      iconWidth = 10, iconHeight = 10
    )
    
    two = makeIcon(
      iconUrl = "stuff/Icons/2.png",
      iconWidth = 10, iconHeight = 10
    )
    
    three = makeIcon(
      iconUrl = "stuff/Icons/3.png",
      iconWidth = 10, iconHeight = 10
    )
    
    four = makeIcon(
      iconUrl = "stuff/Icons/4.png",
      iconWidth = 10, iconHeight = 10
    )
    
    five = makeIcon(
      iconUrl = "stuff/Icons/5.png",
      iconWidth = 10, iconHeight = 10
    )
    
    six = makeIcon(
      iconUrl = "stuff/Icons/6.png",
      iconWidth = 10, iconHeight = 10
    )
    
    seven = makeIcon(
      iconUrl = "stuff/Icons/7.png",
      iconWidth = 10, iconHeight = 10
    )
    
    
    
    
    datasets = c("MHDS Regional Access Points", "South Central Iowa Resource Directory Locations", "Southwest Iowa Resource Directory Locations")
    colors <- RColorBrewer::brewer.pal(n = length(datasets), name="Dark2")
    
    pal <- colorFactor(
      palette = colors,
      levels = datasets)
    
    
    
    
    # Create point map with the regional datasets colored differently
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = st_transform(ia_counties, crs='+proj=longlat +datum=WGS84'),
                  weight = 1, color="#333333") %>%
      addMarkers(lng = ~longitude, lat = ~latitude, label = ~name, icon = one,
                 popup = paste0(regional_MHDS$name, "<br>", regional_MHDS$regional_org, "<br>", regional_MHDS$street_address, "<br>", regional_MHDS$phone, "<br>", regional_MHDS$source),
                 data = regional_MHDS,
                 group = datasets[1]) %>%
      addMarkers(lng = ~longitude, lat = ~latitude, label = ~agency, icon = two,
                 popup = paste0(scbhr_mhds$agency, "<br>", scbhr_mhds$category, "<br>", scbhr_mhds$address, "<br>", scbhr_mhds$phone, "<br>", scbhr_mhds$website),
                 data = scbhr_mhds,
                 group = datasets[2]) %>%
      addMarkers(lng = ~longitude, lat = ~latitude, label = ~`service title`, icon = three,
                 popup = paste0(southwest_mhds$`service title`, "<br>", southwest_mhds$category, "<br>", southwest_mhds$address, "<br>", southwest_mhds$phone, "<br>", southwest_mhds$website),
                 data = southwest_mhds,
                 group = datasets[3]) %>%
      addLayersControl(
        overlayGroups = datasets,
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addLegend(
        position = c("bottomleft"), pal = pal, values = datasets, opacity = 0.9
      ) %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 7",
        onClick=JS("function(btn, map){ map.setZoom(7); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true, zoom: 10}); }"))) %>%
      setView(lng = -93.645733, lat = 42.026234, zoom = 7)
 
  })
  
  output$substance <- renderLeaflet({
    
    one = makeIcon(
      iconUrl = "stuff/Icons/1.png",
      iconWidth = 10, iconHeight = 10
    )
    
    two = makeIcon(
      iconUrl = "stuff/Icons/2.png",
      iconWidth = 10, iconHeight = 10
    )
    
    three = makeIcon(
      iconUrl = "stuff/Icons/3.png",
      iconWidth = 10, iconHeight = 10
    )
    
    four = makeIcon(
      iconUrl = "stuff/Icons/4.png",
      iconWidth = 10, iconHeight = 10
    )
    
    five = makeIcon(
      iconUrl = "stuff/Icons/5.png",
      iconWidth = 10, iconHeight = 10
    )
    
    six = makeIcon(
      iconUrl = "stuff/Icons/6.png",
      iconWidth = 10, iconHeight = 10
    )
    
    seven = makeIcon(
      iconUrl = "stuff/Icons/7.png",
      iconWidth = 10, iconHeight = 10
    )
    
    
    
    
    datasets = c("Drug Drop Off Sites", "Medication Assisted Treatment (MAT) Locations", "Regional Substance Use Treatment Facilities", "Substance Use/Problem Gambling Recovery")
    colors <- RColorBrewer::brewer.pal(n = length(datasets), name="Dark2")
    
    pal <- colorFactor(
      palette = colors,
      levels = datasets)
    
    
    
    
    # Create point map with the regional datasets colored differently
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = st_transform(ia_counties, crs='+proj=longlat +datum=WGS84'),
                  weight = 1, color="#333333") %>%
      addMarkers(lng = ~longitude, lat = ~latitude, label = ~name, icon = one,
                 popup = paste0(Rx_Drop_Off_Locations$name, "<br>", Rx_Drop_Off_Locations$type, "<br>", Rx_Drop_Off_Locations$search_address, "<br>", Rx_Drop_Off_Locations$phone),
                 data = Rx_Drop_Off_Locations,
                 group = datasets[1]) %>%
      addMarkers(lng = ~longitude, lat = ~latitude, label = ~doctor, icon = two,
                 popup = paste0(mat_locations$center, "<br>", mat_locations$doctor, "<br>", mat_locations$search_address, "<br>", mat_locations$phone),
                 data = mat_locations,
                 group = datasets[2]) %>%
      addMarkers(lng = ~longitude, lat = ~latitude, label = ~location_name, icon = three,
                 popup = paste0(regional_substance_treatment$location_name, "<br>", regional_substance_treatment$regional_org, "<br>", regional_substance_treatment$search_address, "<br>", regional_substance_treatment$phone, "<br>", regional_substance_treatment$source),
                 data = regional_substance_treatment,
                 group = datasets[3]) %>%
      addMarkers(lng = ~longitude, lat = ~latitude, label = ~name, icon = two,
                 popup = paste0(sud$name, "<br>", sud$address, "<br>", sud$phone),
                 data = sud,
                 group = datasets[4]) %>%
      addLayersControl(
        overlayGroups = datasets,
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addLegend(
        position = c("bottomleft"), pal = pal, values = datasets, opacity = 0.9
      ) %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 7",
        onClick=JS("function(btn, map){ map.setZoom(7); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true, zoom: 10}); }"))) %>%
      setView(lng = -93.645733, lat = 42.026234, zoom = 7)
    
    
  })
  
  
  

  #-----------------------------------------------------------------------------------------------------#
  
  
  output$change2 <- renderLeaflet({
    
    filter_checkbox <- c("Recovery Housing", "Inpatient Treatment", "Outpatient Treatment")
    
    # Color palette
    colors <- RColorBrewer::brewer.pal(n = length(filter_checkbox), name="Dark2")
    
    pal <- colorFactor(
      palette = colors,
      levels = filter_checkbox)
    
    # Edit starting dataframe and input for lng and lat inside addCircleMarkers to change data used
    df %>%
      leaflet() %>%
      addTiles() %>%
      # Use county border polygons from DSPG package
      addPolygons(data = st_transform(ia_counties, crs='+proj=longlat +datum=WGS84'),
                  weight = 1, color="#333333") %>%
      addCircleMarkers(lng = ~lon, lat = ~lat,
                       radius = 1, color = ~pal("Recovery Housing"), fillOpacity = 0.5,
                       popup = paste(sep = "<br>", Recovery_Reco$Name, Recovery_Reco$Address, Recovery_Reco$Phone), 
                       label= ~Name, data = Recovery_Reco,
                       group = "Recovery Housing") %>%
      addCircleMarkers(lng = ~lon, lat = ~lat,
                       radius = 1, color = ~pal("Inpatient Treatment"), fillOpacity = 0.5,
                       popup = paste(sep = "<br>", Recovery_In$Name, Recovery_In$Address, Recovery_In$Phone), 
                       label= ~Name, data = Recovery_In,
                       group = "Inpatient Treatment") %>%
      addCircleMarkers(lng = ~lon, lat = ~lat,
                       radius = 1, color = ~pal("Outpatient Treatment"), fillOpacity = 0.5,
                       popup = paste(sep = "<br>", Recovery_Out$Name, Recovery_Out$Address, Recovery_Out$Phone), 
                       label= ~Name, data = Recovery_Out,
                       group = "Outpatient Treatment") %>%
      addLayersControl(
        overlayGroups = filter_checkbox,
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addLegend(
        position = c("bottomright"), pal = pal, values = filter_checkbox
      ) %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 7",
        onClick=JS("function(btn, map){ map.setZoom(7); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true, zoom: 10}); }"))) %>%
      setView(lng = -93.645733, lat = 42.026234, zoom = 7)
    
    
  })
  
  output$change3 <- renderLeaflet({
    
    filter_checkbox <- c("Alcoholics Anonymous", "Al-anon", "Adult children of Alcoholic", "Celebrate", "CRUSH", "IDRA", "Narcotics Anonymous","Nar-anon", "Pills Anonymous", "Refuge Recovery", "SMART")
    
    # Color palette
    colors <- RColorBrewer::brewer.pal(n = length(filter_checkbox), name="Spectral")
    
    pal <- colorFactor(
      palette = colors,
      levels = filter_checkbox)
    
    # Create icons and datasets
    Icon_AdultChild <- makeIcon(
      iconUrl = "stuff/Icons/Icon_AdultChildren.png",
      iconWidth = 10, iconHeight = 10
    )
    meetings_AdultChild = Meetings_AdultChildA_data()
    
    Icon_AlcAnonymous <- makeIcon(
      iconUrl = "stuff/Icons/Icon_AlcAnonymous.png",
      iconWidth = 10, iconHeight = 10
    )
    meetings_AlcAnonymous = Meetings_AA_data()
    
    Icon_AlAnon <- makeIcon(
      iconUrl = "stuff/Icons/Icon_AlAnon.png",
      iconWidth = 10, iconHeight = 10
    )
    meetings_AlAnon = Meetings_AAnon_data()
    
    Icon_Celebrate <- makeIcon(
      iconUrl = "stuff/Icons/Icon_Celebrate.png",
      iconWidth = 10, iconHeight = 10
    )
    meetings_Celebrate = Meetings_Celebrate_data()
    
    Icon_Crush <- makeIcon(
      iconUrl = "stuff/Icons/Icon_Crush.png",
      iconWidth = 10, iconHeight = 10
    )
    meetings_Crush = Meetings_Crush_data()
    
    Icon_IDRA <- makeIcon(
      iconUrl = "stuff/Icons/Icon_IDRA.png",
      iconWidth = 10, iconHeight = 10
    )
    meetings_IDRA = Meetings_IDRA_data()
    
    Icon_NarAnon <- makeIcon(
      iconUrl = "stuff/Icons/Icon_NarAnon.png",
      iconWidth = 10, iconHeight = 10
    )
    meetings_NarAnon = Meetings_NAnon_data()
    
    Icon_NarcAnonymous <- makeIcon(
      iconUrl = "stuff/Icons/Icon_NarcoticsAnonymous.png",
      iconWidth = 10, iconHeight = 10
    )
    meetings_NarcAnonymous = Meetings_NA_data()
    
    Icon_PillsAnonymous <- makeIcon(
      iconUrl = "stuff/Icons/Icon_PillsAnonymous.png",
      iconWidth = 10, iconHeight = 10
    )
    meetings_PillsAnonymous = Meetings_PA_data()
    
    Icon_Refuge <- makeIcon(
      iconUrl = "stuff/Icons/Icon_Refuge.png",
      iconWidth = 10, iconHeight = 10
    )
    meetings_Refuge = Meetings_RR_data()
    
    Icon_Smart <- makeIcon(
      iconUrl = "stuff/Icons/Icon_Smart.png",
      iconWidth = 10, iconHeight = 10
    )
    meetings_Smart = Meetings_Smart_data()
    
    # Edit starting dataframe and input for lng and lat inside addCircleMarkers to change data used
      leaflet() %>%
      addTiles() %>%
      addPolygons(data = st_transform(ia_counties, crs='+proj=longlat +datum=WGS84'),
                  weight = 1, color="#333333") %>%
      addMarkers(lng = ~longitude, lat = ~latitude,
                 icon = Icon_AlcAnonymous, 
                 popup= paste(sep = "<br>", meetings_AlcAnonymous$meeting, meetings_AlcAnonymous$address, 
                              paste(sep = " ", meetings_AlcAnonymous$day, meetings_AlcAnonymous$time, meetings_AlcAnonymous$ampm)), 
                 label = ~meeting, data= meetings_AlcAnonymous,
                 group = "Alcoholics Anonymous") %>%
      addMarkers(lng = ~longitude, lat = ~latitude,
                 icon=Icon_AlAnon,
                 popup= paste(sep = "<br>", meetings_AlAnon$meeting, meetings_AlAnon$address, 
                              paste(sep = " ", meetings_AlAnon$day, meetings_AlAnon$time, meetings_AlAnon$ampm)), 
                 label = ~meeting, data= meetings_AlAnon,
                 group = "Al-anon") %>%
      addMarkers(lng = ~longitude, lat = ~latitude,
                 icon = Icon_AdultChild,
                 popup= paste(sep = "<br>", meetings_AdultChild$meeting, meetings_AdultChild$address, 
                              paste(sep = " ", meetings_AdultChild$day, meetings_AdultChild$time, meetings_AdultChild$ampm)), 
                 label = ~meeting, data= meetings_AdultChild,
                 group = "Adult children of alcoholic") %>%
      addMarkers(lng = ~longitude, lat = ~latitude, 
                 icon=Icon_Celebrate,
                 popup= paste(sep = "<br>", meetings_Celebrate$meeting, meetings_Celebrate$address, 
                              paste(sep = " ", meetings_Celebrate$day, meetings_Celebrate$time, meetings_Celebrate$ampm)), 
                 label = ~meeting, data= meetings_Celebrate,
                 group = "Celebrate") %>%
      addMarkers(lng = ~longitude, lat = ~latitude,
                 icon=Icon_Crush,
                 popup= paste(sep = "<br>", meetings_Crush$meeting, meetings_Crush$address, 
                              paste(sep = " ", meetings_Crush$day, meetings_Crush$time, meetings_Crush$ampm)), 
                 label = ~meeting, data= meetings_Crush,
                 group = "CRUSH") %>%
      addMarkers(lng = ~longitude, lat = ~latitude,
                 icon=Icon_IDRA,
                 popup= paste(sep = "<br>", meetings_IDRA$meeting, meetings_IDRA$address, 
                              paste(sep = " ", meetings_IDRA$day, meetings_IDRA$time, meetings_IDRA$ampm)), 
                 label = ~meeting, data= meetings_IDRA,
                 group = "Iowa Dual Recovery Anonymous (IDRA)") %>%
      addMarkers(lng = ~longitude, lat = ~latitude,
                 icon = Icon_NarcAnonymous,
                 popup= paste(sep = "<br>", meetings_NarcAnonymous$meeting, meetings_NarcAnonymous$address, 
                              paste(sep = " ", meetings_NarcAnonymous$day, meetings_NarcAnonymous$time, meetings_NarcAnonymous$ampm)), 
                 label = ~meeting, data= meetings_NarcAnonymous,
                 group = "Narcotics Anonymous") %>%
      addMarkers(lng = ~longitude, lat = ~latitude,
                 icon=Icon_NarAnon,
                 popup= paste(sep = "<br>", meetings_NarAnon$meeting, meetings_NarAnon$address, 
                              paste(sep = " ", meetings_NarAnon$day, meetings_NarAnon$time, meetings_NarAnon$ampm)), 
                 label = ~meeting, data= meetings_NarAnon,
                 group = "Nar-Anon") %>%
      addMarkers(lng = ~longitude, lat = ~latitude,
                 icon=Icon_PillsAnonymous,
                 popup= paste(sep = "<br>", meetings_PillsAnonymous$meeting, meetings_PillsAnonymous$address, 
                              paste(sep = " ", meetings_PillsAnonymous$day, meetings_PillsAnonymous$time, meetings_PillsAnonymous$ampm)), 
                 label = ~meeting, data= meetings_PillsAnonymous,
                 group = "Pills Anonymous") %>%
      addMarkers(lng = ~longitude, lat = ~latitude,
                 icon=Icon_Refuge,
                 popup= paste(sep = "<br>", meetings_Refuge$meeting, meetings_Refuge$address, 
                              paste(sep = " ", meetings_Refuge$day, meetings_Refuge$time, meetings_Refuge$ampm)), 
                 label = ~meeting, data= meetings_Refuge,
                 group = "Refuge Recovery") %>%
      addMarkers(lng = ~longitude, lat = ~latitude,
                 icon=Icon_Smart,
                 popup= paste(sep = "<br>", meetings_Smart$meeting, meetings_Smart$address, 
                              paste(sep = " ", meetings_Smart$day, meetings_Smart$time, meetings_Smart$ampm)), 
                 label = ~meeting, data= meetings_Smart,
                 group = "SMART") %>%
      addLayersControl(
        overlayGroups = filter_checkbox,
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(
        position = c("bottomleft"), pal = pal, values = filter_checkbox
      ) %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 7",
        onClick=JS("function(btn, map){ map.setZoom(7); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true, zoom: 10}); }"))) %>%
      setView(lng = -93.645733, lat = 42.026234, zoom = 7)
    
  })
  
  
  #mymap_proxy <- leafletProxy("change3")
  
  #observe({
    #fdata <- filtered_data_meetings()
    #mymap_proxy %>%
      #clearMarkers() %>%
      #addMarkers(lng = fdata$longitude, lat = fdata$latitude) %>%
      #flyTo(lng = fdata$longitude, lat = fdata$latitude, zoom = 10)
  #})
  

 output$change4 <- renderPlotly({
   
      
      plot <- ggplot() +
        geom_polygon(data = meetings_county, aes(x = long, y = lat, group = group, fill = meeting_rate), color = "black") +
        coord_map() +
        ggtitle("Recovery Support Meetings Per 10,000 People") +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("") +
        ylab("")
      
      plot <- plot + 
        theme_bw() + 
        theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text = element_blank(), axis.ticks = element_blank())
      
      plot2 <- plot + scale_fill_gradient(low = "#e5f5e0", high = "#31a354") +
        labs(fill = "Meetings Per\n10,000 People")
      
      ggplotly(plot2)
    
  })
 
 output$change5 <- renderPlotly({
   

     ggplotly(ggplot(meetings %>%
              separate(col = time, into = c("Hour", "Minute"), sep = "\\:") %>%
              mutate(Hour = as.numeric(Hour)) %>%
              mutate(DayNight = ifelse(ampm == "am" | Hour < 5, "Day", "Evening")) %>%
              mutate(DayNight = as.factor(DayNight)) %>%
              filter(is.na(DayNight)==FALSE), aes(x = day, fill = type)) +
     geom_bar() +
     facet_wrap(~DayNight) +
     ylab("Count") +
     ggtitle("Number of Meetings Each Day by Type") +
     theme(axis.text.x = element_text(angle = 90)))
   
   
 })
 
 output$change6 <- renderPlotly({
   
  
    ggplotly(ggplot(topCities) +
     geom_bar(aes(x = city, fill = city)) +
     coord_flip() +
     ggtitle("Number of Weekly Meetings By City") +
     theme(legend.position = "none")+labs(x="Top Cities"))
   
   
   
 })
 
 output$change7 <- renderPlotly({
   
     ggplotly(ggplot(topCities_pop %>% group_by(city,currentPop) %>% summarise(count=n()) %>% 
              mutate(meetings_per_10k=count*10000/currentPop))+
     geom_bar(aes(x=reorder(city,meetings_per_10k),y=meetings_per_10k),stat = "identity",fill = "#31A354")+
     coord_flip()+ggtitle("Number of Weekly Meetings, Top Cities, per 10000 people")+
     theme(legend.position = "none")+labs(x="Top Cities in Iowa",y="Meetings per 10000 people"))
   
 })

}

#----------------------------------------------#
shinyApp(ui = ui, server = server)
