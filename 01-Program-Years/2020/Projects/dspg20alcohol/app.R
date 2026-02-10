# setup of the shiny app
# run R script DEMOS_Data.R to create current version of
# file demos.RData
# copy following files/folders to server:
#   app.R
#   demos.RData
#   www

library("dplyr")
library("purrr")
library("readr")
library("stringr")
library("readr")
library("readxl")
library("magrittr")
library("stringr")
library("ggplot2")
library("shiny")
#library("sentimentr",
library("shinythemes")
library("shinydashboard")
library("shinydashboardPlus")
library("plotly")
library("prophet")
library("data.table")
library("dygraphs")
library("ggthemes")
library("lubridate")
library("devtools")
library("sf")
library("leaflet")
library("tidyr")
library("tibble")
library("mapview")
library("plainview")
library("shinyjs")
load("demos.RData")

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
                              '<img src=\"DSPG_white-01.png\", alt=\"DSPG 2020 Symposium Proceedings\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('dspg');
           } else {
             x[0].innerHTML = '<div><a href=\"https://datascienceforthepublicgood.org/economic-mobility/community-insights/case-studies\">' +
                              '<img src=\"AEMLogoGatesColors-11.png\", alt=\"Gates Economic Mobility Case Studies\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('economic');
           }
           "


sidebar <- dashboardSidebar(
  sidebarMenu(
    # menuItem(
    #   tabName = "overview_1",
    #   text = "Project Overview",
    #   icon = icon("info circle")
    # ),
    menuItem("Overview", icon = icon("bar-chart-o"), tabName = "Background"),

    # menuItem(
    #   tabName = "data",
    #   text = "Data & Methodology",
    #   icon = icon("database")
    # ),
    menuItem(
      tabName = "indicators",
      text = "Alcohol Related Indicators",
      icon = icon("chart-pie")
    ),
    menuItem(
      tabName = "crashes",
      text = "Alcohol Related Crashes",
      icon = icon("chart-pie")
    ),
    menuItem(
      tabName = "brfss",
      text = ("BRFSS Insights"),
      icon = icon("chart-pie")
    ),
    menuItem("Forecasts", icon = icon("bar-chart-o"), tabName = "Forecasts"),
    # menuItem(
    #   tabName = "findings",
    #   text = "Findings",
    #   icon = icon("chart-pie")
    # ),
    menuItem(
      tabName = "team",
      text = "Team",
      icon = icon("user-friends")
    )

    #textInput("name", "Enter your name:", value = "Heike"),
    #selecInput("state", "Enter your State:", choices=c("Virginia", "Oregon", "Iowa", "Other"))
  )
)
body <- dashboardBody(
  useShinyjs(),
  tabItems(
#     tabItem(tabName = "overview_1",
#             fluidRow(
#               boxPlus(
#                 title = "Project Overview",
#                 closable = FALSE,
#                 width = NULL,
#                 status = "warning",
#                 solidHeader = TRUE,
#                 collapsible = TRUE,
#                 #h2("Demographic and Spatial Analysis of Alcohol Use Population in Iowa"),
#                 h3("Project Description"),
#                 p("We attempt to answer the following questions - ", br(),
#                   "1) Who is alcohol-using population in Iowa?", br(),
#                     "2) Where do the alcohol-using and at-risk populations reside in the state?", br(),
#                     "3) What does the environment for high-risk alcohol use look like?"
#                 ),
#                 h3("Project Goals"),
#                 p("1) Provide a description for the demographic attributes of alcohol using population of Iowa: age, race, education, income, martial and family status",br(),
# "2) Spatially analyze the alcohol using population in Iowa", br(),
# "3) Develop graphics and maps to visually represent the prevalence, density, and the distribution of the alcohol using population by county", br(),
# "4) Estimate alcohol using or at-risk population size (total n and per capita) and establish measures/indicators to illuminate the substance use risk environment in each county (drug-related mortality, binge drinking rates)", br(),
#
# "5)Effectively communicate the findings to public health officials and the general public"),
#                 h3("Our Approach"),
#                 p("1) Identified national and state datasets containing measures of alcohol use", br(),
# "2) Wrangled and tidied data", br(),
# "3) Analyze the data to answer the questions of who and where alcohol-using and at-risk populations are in Iowa", br(),
# "4) Team used an interdisciplinary, data-driven approach to discover, acquire, and statistically analyze publicly available data on alcohol use in Iowa to surveille alcohol use and risk patterns in Iowa"),
#                 h3("Ethical Considerations"),
#                 p("We took the utmost caution when it came to the privacy of our clients data.")
#               )
#             )),
    tabItem(tabName = 'Background',
            fluidRow(
              boxPlus(
                  title = "Overview",
                  closable = FALSE,
                  width = "100%",
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  h3("Project Description"),
                                  p("We attempt to answer the following questions ", br(),
                                    "1) Who is alcohol-using population in Iowa?", br(),
                                      "2) Where do the alcohol-using and at-risk populations reside in the state?", br(),
                                      "3) What does the environment for high-risk alcohol use look like?"
                                  ),
                                  h3("Project Goals"),
                                  p("1) Provide a description for the demographic attributes of alcohol using population of Iowa: age, race, education, income, martial and family status",br(),
                  "2) Spatially analyze the alcohol using population in Iowa", br(),
                  "3) Develop graphics and maps to visually represent the prevalence, density, and the distribution of the alcohol using population by county", br(),
                  "4) Estimate alcohol using or at-risk population size (total n and per capita) and establish measures/indicators to illuminate the substance use risk environment in each county (drug-related mortality, binge drinking rates)", br(),

                  "5) Effectively communicate the findings to public health officials and the general public"),
                                  h3("Our Approach"),
                                  p("1) Identified national and state datasets containing measures of alcohol use", br(),
                  "2) Wrangled and tidied data", br(),
                  "3) Analyze the data to answer the questions of who and where alcohol-using and at-risk populations are in Iowa", br(),
                  "4) Team used an interdisciplinary, data-driven approach to discover, acquire, and statistically analyze publicly available data on alcohol use in Iowa to surveille alcohol use and risk patterns in Iowa"),
                                  h3("Ethical Considerations"),
                                  p("We took the utmost caution when it came to the privacy of our clients data utilizing public data throughout the project.")
              ),
              boxPlus(
                width = 10,
                title = strong(textOutput("titlePH")),
                closable = TRUE,
                solidHeader = TRUE,
                collapsible = TRUE,
                sliderInput(inputId = 'year',label = strong("Select a year"),
                            min = 2012, max = 2019, value = 2012,sep = "", animate = TRUE),
                selectInput(inputId = 'maptype', label = strong('Select a map'),
                            choices = c("Liquor Sold (Gallons) per Capita by County", "Liquor Sold (Gallons) per Capita by City", "Unemployment Rate"),selected = NULL,
                            multiple = FALSE, selectize = TRUE),
                leafletOutput(outputId = "alcohol_map"),
                p(br(),"Liquor sales in gallons per capita are represented in a map and as a line graph. Most of lines are between zero and five. The highest line is the city of Windsor Heights. The possible reasons is that while Windsor Heights is a small city with low population, it includes a supermarket, Sam's and Costco that are likely serving the adjacent cities. Note that there is large drop down between 2014 and 2017. The reason maybe due to road construction around the shopping area making access more difficult. Therefore, the liquor sales per capita went down."),
                p(br(),"Some of the counties with recent relatively high unemployment compared to others are Marshall and Lee counties. Unemployment rate is one of the indicators that we included in our substance use risk index because of the correlation between unemployment and substance use.")

              ),
              boxPlus(
                width = 10,
                title = " Iowa Liquor Sales (Gallons) per Capita for City ",
                closable = TRUE,
                solidHeader = TRUE,
                collapsible = TRUE,
                plotlyOutput(outputId = "alcohol_city"),
                br(),
                p("Interactive line chart above shows the liquor sales in gallons per capita at city-level. Each line represents a city, most of lines are between zero and five. The highest line highlighted is for the city Windsor Heights. The possible reasons here is that Windsor heights is a small city with a low population, but it is adjacent to several larger cities. Windsor Heights includes at least three large size retailers providing discounted liquor (Sam, Walmart, Hy-Vee). Note that there is large drop down between 2014 and 2017. The reason might due to road construction around 73rd Street making it more difficult to access these retail outlets.")
                              )
              # boxPlus(
              #   width = 10,
              #   title = "County Risk Due To Excessive Drinking",
              #   closable = TRUE,
              #   solidHeader = TRUE,
              #   collapsible = TRUE,
              #   tags$img(src="low_risk.png",width = "100%"),
              #   br(),
              #   tags$img(src="high_risk.png",width = "100%")
              #
              # ),
              # boxPlus(
              #   width = 10,
              #   title = "Alcohol Related Automobile Crashes 2010 - 2019",
              #   closable = TRUE,
              #   solidHeader = TRUE,
              #   collapsible = TRUE,
              #   tags$img(src="Alcohol Related Crashes.png",width = "100%")
              #
              # )
              ),
    ),
    tabItem(tabName = 'brfss',
            fluidRow(
              boxPlus(
                #title = "Project Overview",
                closable = FALSE,
                width = NULL,
                status = "warning",
                solidHeader = TRUE,
                collapsible = FALSE,
                h2(strong("Behavioral Risk Factor Surveillance System Insights"))
              ),
              boxPlus(
                width = 10,
                title = "Problem Drinking in Iowa 2017",
                closable = TRUE,
                solidHeader = TRUE,
                collapsible = TRUE,


                p("The data below was derived from the ",a(href = 'https://www.cdc.gov/brfss/index.html', 'Behavioral Risk Factor Surveillance (2017).'),"Binge drinking is represented by the darker grey shading and heavy drinking is represented by the lighter grey shading."),
                br(),
                tags$img(src="Problem Drinking in Iowa 2017.png",width = "70%"),
                br(),br(),
                p("Binge drinkers are defined as males having five or more drinks on one occasion and females having four or more drinks on one occasion. Heavy drinkers are defined as adult men having more than 14 drinks per week and adult women having more than 7 drinks per week.")
                #tags$img(src="image.png",width = "500px")
              ),

              boxPlus(
                width = 10,
                title = "Proportion Binge (dark) and Heavy(light) Drinkers by Demographic Characterisitcs",
                closable = TRUE,
                solidHeader = TRUE,
                collapsible = TRUE,



                tags$img(src="Age.png",width = "70%"),
                br(),
                tags$img(src="Sex.png",width = "70%"),
                br(),
                tags$img(src="Race.png",width = "70%"),
                br(),
                tags$img(src="BMI.png",width = "70%"),
                br(),
                p("Figures show the rates of binge and heavy drinking according to demographic characteristics. Young people, ages 18-24 and 25-29, had the highest rates of binge drinking. Males had higher rates of binge drinking than females. American Indian/ Alaskan Native and Hispanics self-reported higher rates of binge drinking than any other race categories.",br(),"Iowans ages 50-54 had highest rates of heavy drinking. Males had higher rates of binge drinking than females. Iowans who self- reported binge drinking were other race, non-Hispanic")
                #tags$img(src="image.png",width = "500px")
              ),
              boxPlus(
                width = 10,
                title = "Proportion Binge (dark) and Heavy(light) Drinkers by Family Characterisitcs",
                closable = TRUE,
                solidHeader = TRUE,
                collapsible = TRUE,



                tags$img(src="pregnancy.png",width = "70%"),
                br(),
                tags$img(src="marriage.png",width = "70%"),
                br(),
                tags$img(src="children_number.png",width = "70%"),
                br(),
                tags$img(src="BMI.png",width = "70%"),
                br(),
                p("The figures above show the rates of heavy and binge drinking according to family characteristics. Never married and cohabitators reported the highest rates of binge drinking. Parents of 4 or more children reported highest rates of binge drinking. Pregnant women report lower binge drinking than non-pregnant.",br(),"When it comes to heavy drinking, Iowans who reported they were a member of an unmarried couple reported higher rates of heavy drinking than other categories. Iowans who reported they had 6 children reported highest rates of heavy drinking.")
                #tags$img(src="image.png",width = "500px")
              ),
              boxPlus(
                width = 10,
                title = "Proportion Binge (dark) and Heavy(light) Drinkers by Socioeconomic Factors (1)",
                closable = TRUE,
                solidHeader = TRUE,
                collapsible = TRUE,



                tags$img(src="education.png",width = "70%"),
                br(),
                tags$img(src="veteran.png",width = "70%"),
                br(),
                tags$img(src="internet.png",width = "70%"),
                br(),
                p("These figures show the rates of heavy and binge drinking by socioeconomic factors. Iowans who reported they finished some college/tech school and those with a college degree had highest rates of binge drinking. Non-veterans had higher rates of binge drinking. Iowans who reported they have some college/tech school or college degree had highest rates of heavy drinking")

              ),
              boxPlus(
                width = 10,
                title = "Proportion Binge (dark) and Heavy(light) Drinkers by Socioeconomic Factors (2)",
                closable = TRUE,
                solidHeader = TRUE,
                collapsible = TRUE,



                tags$img(src="income.png",width = "70%"),
                br(),
                tags$img(src="employment.png",width = "70%"),
                br(),
                tags$img(src="Homeownership_ Status.png",width = "70%"),
                br(),
                p("These figures show the rates of heavy and binge drinking by income, employment and homeownership status. Iowans who self-reported income of $75k+ tend to have highest rates of binge drinking. Iowans who reported being unemployed for less than a year and students reported highest rates of binge drinking. Homeowners had lower rates of binge drinking than other groups. Iowans who self-reported income of $75k+ tend to report highest rates of heavy drinking. Those employed for wages and self-employed reported highest rates of heavy drinking. ")

              )
            ),
    ),
    tabItem(tabName = 'crashes',
            fluidRow(
              boxPlus(
                width = 10,
                title = strong("Alcohol Related Automobile Crashes 2010 - 2019"),
                closable = TRUE,
                solidHeader = TRUE,
                collapsible = TRUE,
                p("Data for the previous ten years was collected from the",a(href = 'https://icat.iowadot.gov/', 'Iowa DOT Crash Analysis Tool (ICAT).')," The data was filtered to identify all alcohol related crashes. Attributes available included: Driver Gender, Age, Condition (under influence of alcohol), Date, Time, Day of Week, Weather Influence (if any), Vehicle Type, License State, Roadway Type, Injuries Reported. Analysis of the crashes in Iowa revealed that of the 533,909 total crashes over the 10-year period 22,626 where alcohol related. Alcohol related crashes included 34,657 vehicles and resulted in 477 fatalities and 2,124 serious injuries."),
                tags$img(src="crash figure 1.png",width = "70%"),
                br(),
                p("The location of all crashes were mapped using QGIS over a map of Iowa counties. The density of crashes clearly delineated the Iowa road network. Crashes that involved alcohol were symbolized in red color. The crashes involving alcohol were then extracted and mapped over a county map of Iowa to reveal the clustered location of alcohol related crashes."),
                br(),
                tags$img(src="crash figure 2.png",width = "70%"),
                br(),
                p("The map above identifies area of concern in counties in Western Iowa based on a 10-year average alcohol related crash's per capita."),
                br(),
                tags$img(src="crash figure 3.png",width = "70%"),
                br(),
                p("Saturday and Sunday account for 46.5% of all crashes where the driver was under the influence of alcohol.")
              )
            )),

    tabItem(tabName = 'indicators',
            fluidRow(
              boxPlus(
                width = 10,
                title = strong(textOutput("alcoholTitle")),
                closable = TRUE,
                solidHeader = TRUE,
                collapsible = TRUE,
                selectInput(inputId = 'indicatortype', label = strong('Select a Measure'),
                            choices = c("Alcohol Risk Measure", "Substance Use Measure"),selected = NULL,
                            multiple = FALSE, selectize = TRUE),
                uiOutput("alcohol_index")
              ),
              boxPlus(
                  width = 10,
                  title = "County Risk Due To Excessive Drinking",
                  closable = TRUE,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  tags$img(src="countyDrinking1.png",width = "70%"),
                  br(),
                  p("Map illustrates the percent of excessive drinking in each county, which allows us to identify the counties that have the highest percentage of excessive drinking rate. Excessive Drinking Percentage contributes to our Alcohol Risk Index that helps to identify the counties at risk. "),
                  br(),
                  tags$img(src="countyDrinking2.png",width = "70%"),
                  br(),
                  p("Figure above shows the scale of alcohol index based on three indicators (percentage of excessive drinking, alcohol impaired driving death, and liquor sales per capita) using a regression-based model. This method involved factor analysis and lets each indicator contribute to the scale differently. If one variable is strongly associated with the theorized latent factor (alcohol risk), it gets more weight. The opposite is true for weakly correlated variables. The higher value of risk index and darker color in the map represent which county has higher risk of alcohol abuse use."),
                  br(),
                  tags$img(src="countyDrinking3.png",width = "70%"),
                  br(),
                  p("The Alcohol Risk Sum Model is an alcohol index calculated based on additive scale of three indicators which are percentage of excessive drinking, alcohol impaired driving death, and liquor sales per capita. This scale treats each of the three indicators equally, in that they equally contribute to the final index score. The higher value of risk index and darker color in the map represent which county has higher risk of alcohol abuse use."),
                  br(),
                  tags$img(src="low_risk.png",width = "90%"),
                  br(),
                  p("Figure shows the lowest 5 riskiest counties based on the regression model index. On the y-axis, we have the excessive drinking % that is affecting the height of the bar. The shade of the color is determined by the Alcohol Impaired Driving Deaths, with darkest blue being the highest number of deaths. The size of the lollipop is determined by the sales of liquor per capita with the smallest in the smallest size and vice versa. "),
                  br(),
                  tags$img(src="high_risk.png",width = "90%"),
                  br(),
                  p("Figure shows top 5 riskiest counties based on the regression model index. On the y-axis, we have the excessive drinking % that is affecting the height of the bar. Dubuque is one of the high risk counties that we have identified based on the different analysis that we have done with different measures and indexes.  "),
                  br(),
                  tags$img(src="countyDrinking4.png",width = "70%"),
                  br(),
                  p("Figure shows the unemployment rate based on 2018. Some of the counties with relatively high unemployment compared to others are Marshall and Lee counties. Unemployment rate is one of the indicators that we included in our substance use risk index because of the correlation between unemployment and substance use."),
                  br(),
                  tags$img(src="countyDrinking5.png",width = "70%"),
                  br(),
                  p("Figure above shows the percent of children living in poverty. We can see that a number of counties in the southern part of the state have relatively higher poverty as compared to the other counties in other regions. Children living in poverty is included in the substance use risk index because the environment that you grow up in will have an effect on your current and future behavior."),
                  br()
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
                                )))),
    tabItem(tabName = "data",
            fluidRow(
              boxPlus(
                title = "Data & Methodology",
                closable = FALSE,
                width = NULL,
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                h2("Data Sources"),
                img(src = "data_sets.png", width = "450px", align = "right"),
                h3("Data Source 1"),
                p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                h3("Data Source 2"),
                p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                h3("Data Source 3"),
                p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                h2("Methodology"),
                h3("Data Preparation"),
                p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                h3("Data Modeling"),
                p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante.")
              )
            )),
    tabItem(tabName = "findings",
            fluidRow(
              boxPlus(
                title = "Findings",
                closable = FALSE,
                width = NULL,
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                h2("Summary of Findings"),
                p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                h3("Results Section One"),
                img(src = "irrational_venn_diagram.png", width = "360px", align = "right"),
                p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                h3("Results Section Two"),
                p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                h3("Results Section Three"),
                img(src = "food_reality_chart.png", width = "400px", align = "right"),
                p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante.")
              )
            )),
    tabItem(tabName = "team",
            fluidRow(
              boxPlus(
                title = "Teams",
                closable = FALSE,
                width = NULL,
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
#                 h4("Fellow - Deepak George Thomas"),h4("Interns - Katie Thompson, Kok Kent Chong, Xinyi Zhu"),h4("Domain Expert - Ilma Jahic
# "), h4("Team leaders - Christopher Seeger, Heike Hofmann, Shawn Dorius"),
#                 #h3("UVA SDAD Team Members"),
#                 h4("Project Sponsors - Substance Abuse Bureau, Iowa Department of Public Health")
                tags$img(src="DSPG Team Slide.png", width ="90%"),
                  br()
              )

            ))
  ))


ui <- dashboardPage(
  title = "DashboardPage",

  # HEADER ----------------------------------------------------------
  header = dashboardHeaderPlus(
    left_menu = tagList(div("IDENTIFY COMMUNITIES IN GREATEST NEED OF EXCESSIVE ALCOHOL-PREVENTION EFFORTS", style="height:35px; display:flex; align-items: center; color:white;"))
  ),# dashboardHeader(title = "Demographic and Spatial Analysis of Alcohol Use Population in Iowa", titleWidth = 100),
  sidebar = sidebar,
  body = body
)

server <- function(input, output,session) {
  # Run JavaScript Code
  runjs(jscode)

  output$titlePH <- renderText(input$maptype)


  filter_mapData = reactive({filter(map_Data_Unemp_sales, year ==input$year)})
  #filter_sales = reactive({filter(city_lvl_liquor_sales_tbbl, year ==input$year)})
  output$alcohol_forecast = renderDygraph(dyplot.prophet(Iowa_Liquor_Sales_model, Iowa_Liquor_Sales_forecast))
  output$alcohol_trend = renderPlot(prophet_plot_components(Iowa_Liquor_Sales_model, Iowa_Liquor_Sales_forecast))
  output$campus_alcohol_forecast = renderDygraph(dyplot.prophet(Campus_Town_Agg_model, Campus_Town_Agg_forecast))
  output$campus_alcohol_trend = renderPlot(prophet_plot_components(Campus_Town_Agg_model, Campus_Town_Agg_forecast))
  output$crash_forecast = renderDygraph(dyplot.prophet(Crashes_model, Crashes_forecast))
  output$crash_trend = renderPlot(prophet_plot_components(Crashes_model, Crashes_forecast))
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
  output$alcohol_map = renderLeaflet({
    if(input$maptype == 'Liquor Sold (Gallons) per Capita by County'){
      leaflet(filter_mapData())%>%
        addProviderTiles("CartoDB")%>%
        addTiles(group = "OSM (default)")%>%
        addPolygons(color = ~pal(Vol_solpercap), weight = 1,group = "Alcohol Consumption", label = ~COUNTY, fillOpacity = 0.6, opacity = 0.6)%>%
        addLegend("bottomright", pal = pal, values = ~Vol_solpercap,
                  title = "Alcohol Sold per Capita",
                  opacity = 0.5
        )
    }
    else if (input$maptype == 'Unemployment Rate'){
      leaflet(filter_mapData())%>%
        addProviderTiles("CartoDB")%>%
        addTiles(group = "OSM (default)")%>%
        addPolygons(fillOpacity = 0.6, opacity = 0.6,color = ~pal_1(mean_ratePerYear), weight = 2,group = "Unemployment Rate", label = ~COUNTY)%>%
        addLegend("bottomright", pal = pal_1, values = ~mean_ratePerYear,
                  title = "Unemployment Rate (%)",
                  opacity = 0.5,
        )}
    else if (input$maptype == 'Liquor Sold (Gallons) per Capita by City'){
      yearText = paste(c("pc_g", input$year), collapse = "")
      leaflet()%>%
        addProviderTiles("CartoDB")%>%
        addTiles(group = "OSM (default)")%>%
        addCircleMarkers(lng = city_lvl_liquor_sales$longitude, lat = city_lvl_liquor_sales$latitude, radius = city_lvl_liquor_sales %>% pull(yearText) *2, weight = 3, label = city_lvl_liquor_sales %>% pull(yearText), popup = city_lvl_liquor_sales$city, fillOpacity = 0.5 , stroke = FALSE)
        # addLegend("bottomright", pal = pal_sales, values = ~sales,
        #           title = "City Gallons per capita 2019",
        #           opacity = 0.5,
        # )
      #addPolygons(map = leaflet(filter_mapData()), fill = 0, weight = 1, color = "#000000")
    }
  })
  # output$brfss_plot = renderPlotly({
  #   Age = brfss %>% select(hvydrinker,bingedrinker,age5) %>% group_by_at(vars(hvydrinker,bingedrinker,age5)) %>%
  #     pivot_longer(cols=c("hvydrinker","bingedrinker"), names_to="type",values_to="value") %>%
  #     filter(value %in% c("Heavy drinker","Binge drinker")) %>% select(age5, value) %>% group_by(age5,value) %>%
  #     summarise(count=n()) %>% ggplot(aes(x=age5, y=count, fill=value)) +
  #     geom_bar(stat='identity', position='dodge') + theme_bw()
  # #   Sex = brfss %>% select(hvydrinker,bingedrinker,sex) %>% group_by_at(vars(hvydrinker,bingedrinker,sex)) %>%
  #     pivot_longer(cols=c("hvydrinker","bingedrinker"), names_to="type",values_to="value") %>%
  #     filter(value %in% c("Heavy drinker","Binge drinker")) %>% select(sex, value) %>% group_by(sex,value) %>%
  #     summarise(count=n()) %>% ggplot(aes(x=sex, y=count, fill=value)) +
  #     geom_bar(stat='identity', position='dodge') + theme_bw()
  #   Race = brfss %>% select(hvydrinker,bingedrinker,race6) %>% group_by_at(vars(hvydrinker,bingedrinker,race6)) %>%
  #     pivot_longer(cols=c("hvydrinker","bingedrinker"), names_to="type",values_to="value") %>%
  #     filter(value %in% c("Heavy drinker","Binge drinker")) %>% select(race6, value) %>% group_by(race6,value) %>%
  #     summarise(count=n()) %>% ggplot(aes(x=race6, y=count, fill=value)) +
  #     geom_bar(stat='identity', position='dodge') + theme_bw()
  #   BMI = brfss %>% select(hvydrinker,bingedrinker,bmi4) %>% group_by_at(vars(hvydrinker,bingedrinker,bmi4)) %>%
  #     pivot_longer(cols=c("hvydrinker","bingedrinker"), names_to="type",values_to="value") %>%
  #     filter(value %in% c("Heavy drinker","Binge drinker")) %>% select(bmi4, value) %>% group_by(bmi4,value) %>%
  #     summarise(count=n()) %>% ggplot(aes(x=bmi4, y=count, fill=value)) +
  #     geom_bar(stat='identity', position='dodge') + theme_bw()
  #   marriage = brfss %>% select(hvydrinker,bingedrinker,marital) %>% group_by_at(vars(hvydrinker,bingedrinker,marital)) %>%
  #     pivot_longer(cols=c("hvydrinker","bingedrinker"), names_to="type",values_to="value") %>%
  #     filter(value %in% c("Heavy drinker","Binge drinker")) %>% select(marital, value) %>% group_by(marital,value) %>%
  #     summarise(count=n()) %>% ggplot(aes(x=marital, y=count, fill=value)) +
  #     geom_bar(stat='identity', position='dodge') + theme_bw()
  #   children = brfss %>% select(hvydrinker,bingedrinker,child5) %>% group_by_at(vars(hvydrinker,bingedrinker,child5)) %>%
  #     pivot_longer(cols=c("hvydrinker","bingedrinker"), names_to="type",values_to="value") %>%
  #     filter(value %in% c("Heavy drinker","Binge drinker")) %>% select(child5, value) %>% group_by(child5,value) %>%
  #     summarise(count=n()) %>% ggplot(aes(x=child5, y=count, fill=value)) +
  #     geom_bar(stat='identity', position='dodge') + theme_bw()
  #   education = brfss %>% select(hvydrinker,bingedrinker,educ4) %>% group_by_at(vars(hvydrinker,bingedrinker,educ4)) %>%
  #     pivot_longer(cols=c("hvydrinker","bingedrinker"), names_to="type",values_to="value") %>%
  #     filter(value %in% c("Heavy drinker","Binge drinker")) %>% select(educ4, value) %>% group_by(educ4,value) %>%
  #     summarise(count=n()) %>% ggplot(aes(x=educ4, y=count, fill=value)) +
  #     geom_bar(stat='identity', position='dodge') + theme_bw()
  #   veteran = brfss %>% select(hvydrinker,bingedrinker,veteran3) %>% group_by_at(vars(hvydrinker,bingedrinker,veteran3)) %>%
  #     pivot_longer(cols=c("hvydrinker","bingedrinker"), names_to="type",values_to="value") %>%
  #     filter(value %in% c("Heavy drinker","Binge drinker")) %>% select(veteran3, value) %>% group_by(veteran3,value) %>%
  #     summarise(count=n()) %>% ggplot(aes(x=veteran3, y=count, fill=value)) +
  #     geom_bar(stat='identity', position='dodge') + theme_bw()
  #   Internet = brfss %>% select(hvydrinker,bingedrinker,internet) %>% group_by_at(vars(hvydrinker,bingedrinker,internet)) %>%
  #     pivot_longer(cols=c("hvydrinker","bingedrinker"), names_to="type",values_to="value") %>%
  #     filter(value %in% c("Heavy drinker","Binge drinker")) %>% select(internet, value) %>% group_by(internet,value) %>%
  #     summarise(count=n()) %>% ggplot(aes(x=internet, y=count, fill=value)) +
  #     geom_bar(stat='identity', position='dodge') + theme_bw()
  #   Income = brfss %>% select(hvydrinker,bingedrinker,income2) %>% group_by_at(vars(hvydrinker,bingedrinker,income2)) %>%
  #     pivot_longer(cols=c("hvydrinker","bingedrinker"), names_to="type",values_to="value") %>%
  #     filter(value %in% c("Heavy drinker","Binge drinker")) %>% select(income2, value) %>% group_by(income2,value) %>%
  #     summarise(count=n()) %>% ggplot(aes(x=income2, y=count, fill=value)) +
  #     geom_bar(stat='identity', position='dodge') + theme_bw()
  #   Employment = brfss %>% select(hvydrinker,bingedrinker,employ1) %>% group_by_at(vars(hvydrinker,bingedrinker,employ1)) %>%
  #     pivot_longer(cols=c("hvydrinker","bingedrinker"), names_to="type",values_to="value") %>%
  #     filter(value %in% c("Heavy drinker","Binge drinker")) %>% select(employ1, value) %>% group_by(employ1,value) %>%
  #     summarise(count=n()) %>% ggplot(aes(x=employ1, y=count, fill=value)) +
  #     geom_bar(stat='identity', position='dodge') + theme_bw()
  #   Home = brfss %>% select(hvydrinker,bingedrinker,renthom1) %>% group_by_at(vars(hvydrinker,bingedrinker,renthom1)) %>%
  #     pivot_longer(cols=c("hvydrinker","bingedrinker"), names_to="type",values_to="value") %>%
  #     filter(value %in% c("Heavy drinker","Binge drinker")) %>% select(renthom1, value) %>% group_by(renthom1,value) %>%
  #     summarise(count=n()) %>% ggplot(aes(x=renthom1, y=count, fill=value)) +
  #     geom_bar(stat='identity', position='dodge') + theme_bw()
  #
  #
  #
  #
  #
  # })
  # output$brfss_plot <- renderUI({
  #
  # })
  #
  output$alcoholTitle <- renderText(input$indicatortype)
  output$alcohol_index = renderUI({
    if(input$indicatortype == 'Substance Use Measure'){
      indexmap = dplyr::rename(indexmap, phy_unhlthy = 'Average Number of Physically Unhealthy Days')
      bins_phy_unhlthy <- c(min(indexmap$phy_unhlthy), (mean(indexmap$phy_unhlthy)-1*sd(indexmap$phy_unhlthy)), (mean(indexmap$phy_unhlthy)),(mean(indexmap$phy_unhlthy)+1*sd(indexmap$phy_unhlthy)),max(indexmap$phy_unhlthy))
      pal_phy_unhlthy <- colorBin(c("#badcf7", "#3A7AB2", "#2E6392", "#255075", "#012340"), domain = indexmap$phy_unhlthy, bins = 5, pretty = FALSE)
      #Average Number of Physically Unhealthy Days
      m1 = leaflet(indexmap)%>%
        addProviderTiles("CartoDB")%>%
        addTiles(group = "OSM (default)")%>%
        addPolygons(color = ~pal_phy_unhlthy(phy_unhlthy), weight = 1, label = ~COUNTY, fillOpacity = 0.6) %>%addLegend("bottomright", pal = pal_phy_unhlthy, values = ~phy_unhlthy,
                                                                                                     title = "Average Number of Physically Unhealthy Days",
                                                                                                     opacity = 0.5)

      #Average Number of Mentally Unhealthy Days
      indexmap = dplyr::rename(indexmap, mntl_unhlthy = 'Average Number of Mentally Unhealthy Days')
      bins_mntl_unhlthy <- c(min(indexmap$mntl_unhlthy), (mean(indexmap$mntl_unhlthy)-1*sd(indexmap$mntl_unhlthy)), (mean(indexmap$mntl_unhlthy)),(mean(indexmap$mntl_unhlthy)+1*sd(indexmap$mntl_unhlthy)),max(indexmap$mntl_unhlthy))
      pal_mntl_unhlthy <- colorBin(c("#badcf7", "#3A7AB2", "#2E6392", "#255075", "#012340"), domain = indexmap$mntl_unhlthy, bins = 5, pretty = FALSE)
      m2 = leaflet(indexmap)%>%
        addProviderTiles("CartoDB")%>%
        addTiles(group = "OSM (default)")%>%
        addPolygons(color = ~pal_mntl_unhlthy(mntl_unhlthy), weight = 1, label = ~COUNTY, fillOpacity = 0.6)%>%addLegend("bottomright", pal = pal_mntl_unhlthy, values = ~mntl_unhlthy,
                                                                                                      title = "Average Number of Mentally Unhealthy Days",
                                                                                                      opacity = 0.5)

      #High school graduation
      indexmap = dplyr::rename(indexmap, grad_rate = 'High School Graduation Rate')
      bins_grad_rate <- c(min(indexmap$grad_rate), (mean(indexmap$grad_rate)-1*sd(indexmap$grad_rate)), (mean(indexmap$grad_rate)),(mean(indexmap$grad_rate)+1*sd(indexmap$grad_rate)),max(indexmap$grad_rate))
      pal_grad_rate <- colorBin(c("#badcf7", "#3A7AB2", "#2E6392", "#255075", "#012340"), domain = indexmap$grad_rate, bins = 5, pretty = FALSE)
      m3 = leaflet(indexmap)%>%
        addProviderTiles("CartoDB")%>%
        addTiles(group = "OSM (default)")%>%
        addPolygons(color = ~pal_grad_rate(grad_rate), weight = 1, label = ~COUNTY, fillOpacity = 0.6)%>%addLegend("bottomright", pal = pal_grad_rate, values = ~grad_rate,
                                                                                                title = "High School Graduation Rate",
                                                                                                opacity = 0.5)

      #% Unemployed
      indexmap = dplyr::rename(indexmap, prcnt_unemp = '% Unemployed')
      bins_prcnt_unemp <- c(min(indexmap$prcnt_unemp), (mean(indexmap$prcnt_unemp)-1*sd(indexmap$prcnt_unemp)), (mean(indexmap$prcnt_unemp)),(mean(indexmap$prcnt_unemp)+1*sd(indexmap$prcnt_unemp)),max(indexmap$prcnt_unemp))
      pal_prcnt_unemp <- colorBin(c("#badcf7", "#3A7AB2", "#2E6392", "#255075", "#012340"), domain = indexmap$prcnt_unemp, bins = 5, pretty = FALSE)
      m4 = leaflet(indexmap)%>%
        addProviderTiles("CartoDB")%>%
        addTiles(group = "OSM (default)")%>%
        addPolygons(color = ~pal_prcnt_unemp(prcnt_unemp), weight = 1, label = ~COUNTY, fillOpacity = 0.6)%>%addLegend("bottomright", pal = pal_prcnt_unemp, values = ~prcnt_unemp,
                                                                                                    title = "Unemployment Percentage",
                                                                                                    opacity = 0.5)


      #Children in poverty
      indexmap = dplyr::rename(indexmap, chld_pover = "% Children in Poverty")
      bins_chld_pover <- c(min(indexmap$chld_pover), (mean(indexmap$chld_pover)-1*sd(indexmap$chld_pover)), (mean(indexmap$chld_pover)),(mean(indexmap$chld_pover)+1*sd(indexmap$chld_pover)),max(indexmap$chld_pover))
      pal_chld_pover <- colorBin(c("#badcf7", "#3A7AB2", "#2E6392", "#255075", "#012340"), domain = indexmap$chld_pover, bins = 5, pretty = FALSE)
      m5 = leaflet(indexmap)%>%
        addProviderTiles("CartoDB")%>%
        addTiles(group = "OSM (default)")%>%
        addPolygons(color = ~pal_chld_pover(chld_pover), weight = 1, label = ~COUNTY, fillOpacity = 0.6)%>%addLegend("bottomright", pal = pal_chld_pover, values = ~chld_pover,
                                                                                                  title = "% Children in Poverty",
                                                                                                  opacity = 0.5)

      #Single Parent HouseHolds
      indexmap = dplyr::rename(indexmap, sing_par = '% Single-Parent Households')
      bins_sing_par <- c(min(indexmap$sing_par), (mean(indexmap$sing_par)-1*sd(indexmap$sing_par)), (mean(indexmap$sing_par)),(mean(indexmap$sing_par)+1*sd(indexmap$sing_par)),max(indexmap$sing_par))
      pal_sing_par <- colorBin(c("#badcf7", "#3A7AB2", "#2E6392", "#255075", "#012340"), domain = indexmap$sing_par, bins = 5, pretty = FALSE)
      m6 = leaflet(indexmap)%>%
        addProviderTiles("CartoDB")%>%
        addTiles(group = "OSM (default)")%>%
        addPolygons(color = ~pal_sing_par(sing_par), weight = 1, label = ~COUNTY, fillOpacity = 0.6)%>%addLegend("bottomright", pal = pal_sing_par, values = ~sing_par,
                                                                                              title = "% Single-Parent Households",
                                                                                              opacity = 0.5)


      #Social Association Rate
      indexmap = dplyr::rename(indexmap, scl_ass_rt = 'Social Association Rate')
      bins_scl_ass_rt <- c(min(indexmap$scl_ass_rt), (mean(indexmap$scl_ass_rt)-1*sd(indexmap$scl_ass_rt)), (mean(indexmap$scl_ass_rt)),(mean(indexmap$scl_ass_rt)+1*sd(indexmap$scl_ass_rt)),max(indexmap$scl_ass_rt))
      pal_scl_ass_rt <- colorBin(c("#badcf7", "#3A7AB2", "#2E6392", "#255075", "#012340"), domain = indexmap$scl_ass_rt, bins = 5, pretty = FALSE)
      m7 = leaflet(indexmap)%>%
        addProviderTiles("CartoDB")%>%
        addTiles(group = "OSM (default)")%>%
        addPolygons(color = ~pal_scl_ass_rt(scl_ass_rt), weight = 1, label = ~COUNTY, fillOpacity = 0.6)%>%addLegend("bottomright", pal = pal_scl_ass_rt, values = ~scl_ass_rt,
                                                                                                  title = "Social Association Rate",
                                                                                                  opacity = 0.5)


      leafsync::sync(m1, m2, m3, m4,m5,m6,m7)}
    else if(input$indicatortype == 'Alcohol Risk Measure'){

      # % excessive drinking
      index_map_final = dplyr::rename(index_map_final, xsdrnks = '%ExcessiveDrinking')
      bins_xsdrnks <- c(min(index_map_final$xsdrnks), (mean(index_map_final$xsdrnks)-1*sd(index_map_final$xsdrnks)), (mean(index_map_final$xsdrnks)),(mean(index_map_final$xsdrnks)+1*sd(index_map_final$xsdrnks)),max(index_map_final$xsdrnks))
      pal_xsdrnks <- colorBin(c("#badcf7", "#3A7AB2", "#2E6392", "#255075", "#012340"), domain = index_map_final$xsdrnks, bins = 5, pretty = FALSE)
      m8 = leaflet(index_map_final)%>%
        addProviderTiles("CartoDB")%>%
        addTiles(group = "OSM (default)")%>%
        addPolygons(color = ~pal_xsdrnks(xsdrnks), weight = 1, label = ~COUNTY, fillOpacity = 0.6)%>%addLegend("bottomright", pal = pal_xsdrnks, values = ~xsdrnks,
                                                                                            title = "Excessive Drinking (%)",
                                                                                            opacity = 0.5)

      # Total Crashes
      index_map_final = dplyr::rename(index_map_final, tot_crsh = '#Alcohol-ImpairedDrivingDeaths')
      bins_tot_crsh <- c(min(index_map_final$tot_crsh), (mean(index_map_final$tot_crsh)-1*sd(index_map_final$tot_crsh)), (mean(index_map_final$tot_crsh)),(mean(index_map_final$tot_crsh)+1*sd(index_map_final$tot_crsh)),max(index_map_final$tot_crsh))
      pal_tot_crsh <- colorBin(c("#badcf7", "#3A7AB2", "#2E6392", "#255075", "#012340"), domain = index_map_final$tot_crsh, bins = 5, pretty = FALSE)
      m9 = leaflet(index_map_final)%>%
        addProviderTiles("CartoDB")%>%
        addTiles(group = "OSM (default)")%>%
        addPolygons(color = ~pal_tot_crsh(tot_crsh), weight = 1, label = ~COUNTY, fillOpacity = 0.6)%>%addLegend("bottomright", pal = pal_tot_crsh, values = ~tot_crsh,
                                                                                              title = "Total Crashes",
                                                                                              opacity = 0.5)


      # Sales Per Capita
      index_map_final = dplyr::rename(index_map_final, sls_pc = 'SalesPerCapita')
      bins_sls_pc <- c(min(index_map_final$sls_pc), (mean(index_map_final$sls_pc)-1*sd(index_map_final$sls_pc)), (mean(index_map_final$sls_pc)),(mean(index_map_final$sls_pc)+1*sd(index_map_final$sls_pc)),max(index_map_final$sls_pc))
      pal_sls_pc <- colorBin(c("#badcf7", "#3A7AB2", "#2E6392", "#255075", "#012340"), domain = index_map_final$sls_pc, bins = 5, pretty = FALSE)
      #Average Number of Physically Unhealthy Days
      m10 = leaflet(index_map_final)%>%
        addProviderTiles("CartoDB")%>%
        addTiles(group = "OSM (default)")%>%
        addPolygons(color = ~pal_sls_pc(sls_pc), weight = 1,label = ~COUNTY, fillOpacity = 0.6)%>%addLegend("bottomright", pal = pal_sls_pc, values = ~sls_pc,
                                                                                         title = "Sales Per Capita",
                                                                                         opacity = 0.5)


      # Alcohol index with Shawn weighted
      index_SFD_final = dplyr::rename(index_SFD_final, AlcIndex3 = 'AlcIndex3')
      bins_AlcIndex3 <- c(min(index_SFD_final$AlcIndex3), (mean(index_SFD_final$AlcIndex3)-1*sd(index_SFD_final$AlcIndex3)), (mean(index_SFD_final$AlcIndex3)),(mean(index_SFD_final$AlcIndex3)+1*sd(index_SFD_final$AlcIndex3)),max(index_SFD_final$AlcIndex3))
      pal_AlcIndex3 <- colorBin(c("#badcf7", "#3A7AB2", "#2E6392", "#255075", "#012340"), domain = index_SFD_final$AlcIndex3, bins = 5, pretty = FALSE)
      m11 = leaflet(index_SFD_final)%>%
        addProviderTiles("CartoDB")%>%
        addTiles(group = "OSM (default)")%>%
        addPolygons(color = ~pal_AlcIndex3(AlcIndex3), weight = 1, label = ~COUNTY, fillOpacity = 0.6)%>%addLegend("bottomright", pal = pal_AlcIndex3, values = ~AlcIndex3,
                                                                                                title = "Weighted Alcohol Index",
                                                                                                opacity = 0.5)


      # Alcohol index with Shawn weighted -regression
      index_SFD_final = dplyr::rename(index_SFD_final, AlcIndexReg = 'AlcIndexReg')
      bins_AlcIndexReg <- c(min(index_SFD_final$AlcIndexReg), (mean(index_SFD_final$AlcIndexReg)-1*sd(index_SFD_final$AlcIndexReg)), (mean(index_SFD_final$AlcIndexReg)),(mean(index_SFD_final$AlcIndexReg)+1*sd(index_SFD_final$AlcIndexReg)),max(index_SFD_final$AlcIndexReg))
      pal_AlcIndexReg <- colorBin(c("#badcf7", "#3A7AB2", "#2E6392", "#255075", "#012340"), domain = index_SFD_final$AlcIndexReg, bins = 5, pretty = FALSE)
      m12 = leaflet(index_SFD_final)%>%
        addProviderTiles("CartoDB")%>%
        addTiles(group = "OSM (default)")%>%
        addPolygons(color = ~pal_AlcIndexReg(AlcIndexReg), weight = 1, label = ~COUNTY, fillOpacity = 0.6)%>%addLegend("bottomright", pal = pal_AlcIndexReg, values = ~AlcIndexReg,
                                                                                                    title = "Alcohol Index (Regression)",
                                                                                                    opacity = 0.5)

      leafsync::sync(m8, m9, m10, m11, m12)}

  })


  #
  #
  # output$alcohol_map = renderLeaflet({leaflet(filter_mapData())%>%
  #     # addProviderTiles("CartoDB")%>%
  #     # addTiles(group = "OSM (default)")%>%
  #     #addPolygons(fill = 0, weight = 1, color = "#000000",group = "Base Map")%>%
  #     # addPolygons(color = ~pal(Vol_solpercap), weight = 1,group = "Alcohol Consumption")%>%
  #     # addPolygons(color = ~pal_1(mean_ratePerYear), weight = 1,group = "Unemployment Rate")%>%
  #     addLayersControl(baseGroups = c("Alcohol Consumption","Unemployment Rate"))
  # })

  output$alcohol_city = renderPlotly(a)


  # Working Code
  # observe({
  #   leafletProxy("alcohol_map", data = filter_mapData()) %>%
  #     clearShapes() %>%
  #     addPolygons(color = ~pal(Vol_solpercap), weight = 1,group = "Alcohol Consumption")
  #
  # })
  #
  #   observe({
  #     leafletProxy("alcohol_map", data = filter_mapData()) %>%
  #       clearShapes() %>%
  #       addPolygons(color = ~pal_1(mean_ratePerYear), weight = 1,group = "Unemployment Rate")
  #   })
}
shinyApp(ui = ui, server = server)

