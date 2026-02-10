library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(dashboardthemes)
library(tidyverse)
library(gt)
library(shinyjs)

source("theme.R")

font = 'Arial'

# Setting up data for map
#ffu_merged <- readRDS('data/ffu_merged.RDS')

vacounties_labels <- readRDS('data/vacounties_labels.RDS')
vacounties <- readRDS('data/vacounties.RDS')

va_table_long <- readRDS('data/va_table_long.RDS')

ffu_va_value <- readRDS('data/ffu_va_value.RDS')
ffu_va_housing <- readRDS('data/ffu_va_housing.RDS')
ffu_va_yrbuilt <- readRDS('data/ffu_va_yrbuilt.RDS')

ruca_def <- read_csv('data/ruca_def.csv', col_types = cols(Code = col_factor()))

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

shinyApp(
  ui = dashboardPagePlus(
    title = "DashboardPage",
    header = dashboardHeaderPlus(
      title = "DSPG 2020 ERS",
      left_menu = tagList(div("EVALUATING RESIDENTIAL PROPERTY DATA QUALITY", style="height:35px; display:flex; align-items: center; color: silver;"))
      ),

# SIDEBAR (LEFT) ----------------------------------------------------------
    sidebar = dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem(
          tabName = "overview",
          text = "Project Overview",
          icon = icon("info circle")
        ),
        menuItem(
          tabName = "data",
          text = "Data Sources & Methodology",
          icon = icon("database")
        ),
        menuItem(
          tabName = "profiling",
          text = "Data Profiling",
          icon = icon("chart-pie")
        ),
        menuItem(
          tabName = "vatable",
          text = "Virginia Counties",
          icon = icon("bar-chart")
        ),
        menuItem(
          tabName = "vamap",
          text = "Virginia Maps",
          icon = icon("map-marked-alt")
        ),
        menuItem(
          tabName = "team",
          text = "Team",
          icon = icon("user-friends")
        ),
        menuItem(
          tabName = "download",
          text = "Data Download",
          icon = icon("cloud-download")
        )
      )
    ),

# BODY --------------------------------------------------------------------
    body = dashboardBody(
      useShinyjs(),
      customTheme,
      fluidPage(
      tabItems(

# OVERVIEW----------------------------------------------------------------------------------------
        tabItem(tabName = "overview",
                fluidRow(
                  boxPlus(
                    title = "Project Overview",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    h1("2020 DSPG USDA-ERS: Evaluating Residential Property Data Quality"),
                    h2("Project Description"),
                    column(12,
                           img(src = "dspg-logo.png", style = "display: block; float: right;", width = "200"),
                           p("To explore the influence of broadband access on rural property values, this project evaluates the quality of CoreLogic property data, which is aggregated commercial and residential property data based on county tax assessments and property deeds. We are comparing American Community Survey estimates to CoreLogic counts and estimates for relevant housing variables. Property-level comparisons at the national level would not be possible without access to the underlying local data. We additionally focus on two Virginia counties for which we have local property-level data, Fairfax County and New Kent County, as case studies for additional data quality evaluations. This project will enable the Social and Decision Analytics team and other CoreLogic data users to learn about the strengths and limitations of CoreLogic data."),
                           h2("Project Goals"),
                           p("The project sought to address the following questions:"),
                           tags$ul(
                             tags$li("What is the coverage and representativeness of CoreLogic 2018 property data compared to benchmark datasets, particularly for rural areas?"),
                             tags$li("How can we scale this meaningfully to the state level when we only have property-level data for certain counties?")
                           )
                           )
                  ),

                  boxPlus(
                    title = "Our Approach",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    p("Our project process included the following elements of the data science framework (pictured below):"),
                    h3("Data Discovery"),
                    column(12,
                           img(src = "ds-framework1.png", style = "display: inline; float: right;", width = "500px"),
                           p("We undertook a data discovery process to find datasets to benchmark against the proprietary CoreLogic housing dataset. We considered sample size, accessibility, geography, variables provided, and unit of analysis to ensure the datasets were appropriate and useful to the project. We narrowed down our findings to:"),
                           tags$ul(
                             tags$li("Property data from Fairfax County, Virginia, which is primarily affluent and urban"),
                             tags$li("Property data from New Kent County, Virginia, which is smaller and less affluent than Fairfax County"),
                             tags$li("Housing data from the American Community Survey (ACS), a national survey undertaken by the Census Bureau")),
                           p("We also evaluated the following sources, but determined they were not as comprehensive as ACS:"),
                           tags$ul(
                             tags$li("American Housing Survey"),
                             tags$li("Black Knight"),
                             tags$li("Multiple Listing Service"),
                             tags$li("Zillow"),
                             tags$li("USPS Vacancy Data"),
                             tags$li("Housing Assistance Council"),
                             tags$li("Housing Mortgage Disclosure Act"))),
                    h3("Data Profiling"),
                    p("We profiled the CoreLogic data subset for Fairfax County and New Kent County in Virginia, in addition to the Fairfax County and New Kent County property data, to compare the completeness and variables included in each dataset."),
                    h3("Data Preparation and Linkage"),
                    p("We used the Census geocoder to geocode the subsets of the CoreLogic data for Fairfax and New Kent that did not have a property-level latitude and longitude. We also geocoded the New Kent County data as latitude and longitude was not provided in the original data. We attempted record linkage between the county and CoreLogic data and learned that linkage on raw latitude and longitude is not possible because CoreLogic data records a latitude and longitude in the middle of the parcel, while the geocoder we used places the latitude and longitude at the border of the property. We discuss further record linkage attempts in the methodology section."),
                    p("We additionally grouped the CoreLogic data by census tract and created equivalent variables to ACS estimates for total housing counts, vacancy status, year built, and assessed property value. We then linked these estimates at the census tract level and began exploring the differences between CoreLogic and ACS estimates."),
                    h3("Statistical Analysis"),
                    p("Using the linked CoreLogic and ACS data, we benchmarked the CoreLogic data using a \"fitness-for-use\" metric derived from a previous Census partnership. This is discussed more in the methodology section. The fitness-for-use metric takes into account both the ACS estimate and the ACS margin of error when comparing with the CoreLogic values.")
                  ),
                  boxPlus(
                    title = "Ethical Considerations and Limitations",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    p("This project will give enable researchers to better understand the coverage and representativeness of CoreLogic data. Many traditional components of an ethical analysis (for example, whether data is geographically representative) are actually inherently part of our project goals."),
                    p("However, we still made assumptions in the framing of our project that may affect the quality of our results. These assumptions included:"),
                    tags$ul(
                      tags$li("Results from just a few counties or states can be meaningfully generalized to a larger sample (while we only currently have results for Virginia, the data quality for a different state may look very different)"),
                      tags$li("Housing units are defined similarly across counties and geographic areas (we had to make assumptions about which property types were residential in order to draw comparisons between CoreLogic and ACS data)"),
                      tags$li("Data taken directly from counties (such as the Fairfax County county-level data we accessed) can serve as \"truth\" in evaluating data quality (these data may have its own quality issues)"),
                      tags$li("It is meaningful to compare property-level data (i.e. CoreLogic data) and survey aggregations (e.g. ACS data) (we had to make several assumptions in order to directly compare geographic levels of these data types)")
                    ),
                    p("Finally, it is worth noting that aggregated data can pose a greater threat to privacy than its individual counterpart. When working with a large proprietary dataset with personally identifiable information, such as the CoreLogic data, we have an ethical responsibility to maintain privacy and anonymity of individuals in our presentation of results.")
                  )

                )),

# VIRGINIA MAP ------------------------------------------------------------------------------------
        tabItem(tabName = "vamap",
                fluidRow(
                  boxPlus(
                    title = "Background and Maps",
                    closable = FALSE,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = NULL,
                    p("The maps on this page present a fitness-for-use metric that compares 2018 CoreLogic property data to the American Community Survey for three variables (housing type, year built, and value); see Data Sources and Methodology for explanation of how this is calculated. If the fitness-for-use value:"),
                    tags$ul(
                      tags$li("is negative, this indicates that the CoreLogic value is larger than the ACS estimate."),
                      tags$li("is positive, this indicates that the ACS estimate is larger than the CoreLogic value."),
                      tags$li("falls outside of the -1 to 1 range, this indicates that the CoreLogic value does not fall between the 90 percent ACS margin of error.")
                    ),
                    p('The census tract variable in the 2018 Virginia CoreLogic data was 93 percent complete (out of 1907 tracts, 133 were missing). The missing census tracts have led to some county-level missing data in the fitness-for-use calculation. If the tract in the map is greyed out, we were unable to calculate the fitness-for-use as a result of this missing data.')
                    #(img(src = "static_maps.jpg", width = 1100))
                    #p('Hover over the map to see information on the county and RUCA code of each census tract as well as the exact fitness-for-use value. RUCA codes (discussed more in the Data & Methodology tab) have the following definitions:'),
                    #tableOutput('rucatable2')
                    )
                  ),

                fluidRow(
                  boxPlus(
                    title = "Housing Units",
                    closable = FALSE,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = NULL,
                    (img(src = "ffu_housing_units_2.jpg", width = 1100)),
                    p(""),
                    p("Compared to ACS Housing Units, CoreLogic data tend to have:"),
                    tags$ul(
                      tags$li("Higher counts of housing units (gold shades)"),
                      tags$li("Higher counts of occupied units (gold shades)"),
                      tags$li("Lower counts of vacant units (blue shades)")
                    )
                  )
                ),

                fluidRow(
                  boxPlus(
                    title = "Year Built",
                    closable = FALSE,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = NULL,
                    (img(src = "ffu_year_built_2.jpg", width = 1100)),
                    p("Compared to ACS counts of housing units by Year Built, CoreLogic data tend to have lower counts (blue shades), except for housing units built since 2014.")
                  )
                ),

                fluidRow(
                  boxPlus(
                    title = "Property Values",
                    closable = FALSE,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = NULL,
                    (img(src = "ffu_property_value_2.jpg", width = 1100)),
                    p("Compared to ACS counts of housing units by Property Value, CoreLogic data tend to have:"),
                    tags$ul(
                      tags$li("Higher counts of housing units (gold shades) for property values up $150,000"),
                      tags$li("Similar counts of housing units for property values from $150,000 to $750,00K with some variation"),
                      tags$li("Nearly identifical counts of housing units for property values above $750,000")
                    )
                  )
                )

#                fluidRow(
#                  boxPlus(
#                    title = "Map Takeaways",
#                    closable = FALSE,
#                    status = "warning",
#                    solidHeader = TRUE,
#                    collapsible = TRUE,
#                    width = NULL,
#                   p("Compared to ACS Housing Units, CoreLogic data tend to have:"),
#                   tags$ul(
#                     tags$li("Higher counts of housing units (gold shades)"),
#                     tags$li("Higher counts of occupied units (gold shades)"),
#                     tags$li("Lower counts of vacant units (blue shades)")
#                   ),
#                   p("Compared to ACS counts of housing units by Year Built, CoreLogic data tend to have lower counts (blue shades), except for housing units built since 2014."),
#                   p("Compared to ACS counts of housing units by Property Value, CoreLogic data tend to have:"),
#                   tags$ul(
#                     tags$li("Higher counts of housing units (gold shades) for property values up $150,000"),
#                     tags$li("Similar counts of housing units for property values from $150,000 to $750,00K with some variation"),
#                     tags$li("Nearly identifical counts of housing units for property values above $750,000")
#                   )
#                  )
#                )

),
        #             column(4,
        #                  radioButtons("variable", "Variable", c("Number of Housing Units", "Property Value", "Year Built", "Occupancy Status"))),
        #             column(4,
        #                  selectInput("range", "Value Range", subset(colnames(ffu_merged@data), str_detect(colnames(ffu_merged@data), "value")))),
        #
        #            leafletOutput("va_map")
        #            )

# VIRGINIA TABLE  ---------------------------------------------------------------------------------
        tabItem(tabName = "vatable",
                fluidRow(
                  boxPlus(
                    title = "Virginia Counties",
                    closable = FALSE,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    width = NULL,
                    p("The charts on this page presents a fitness-for-use metric that compares 2018 CoreLogic property data to the American Community Survey for three variables (housing type, year built, and value) for a specific county in Virginia (see Data Sources and Methodology for explanation of how this is calculated). If the fitness-for-use value:"),
                    tags$ul(
                      tags$li("is negative, this indicates that the CoreLogic value is larger than the ACS estimate."),
                      tags$li("is positive, this indicates that the ACS estimate is larger than the CoreLogic value."),
                      tags$li("falls outside of the -1 to 1 range, this indicates that the CoreLogic value does not fall between the 90 percent ACS margin of error.")
                    ),
                    p('The census tract variable in the 2018 Virginia CoreLogic data was 93 percent complete (out of 1907 tracts, 133 were missing). The missing census tracts have led to some county-level missing data in the fitness-for-use calculation. If the county in the dropdown is listed as "No Data Available", we were unable to calculate the fitness-for-use as a result of this missing data, and the associated table and plots are blank.'),
                    selectInput(
                      inputId = 'vacty',
                      label = 'Select a Virginia County',
                      choices = vacounties_labels,
                      selected = 'Albemarle County, 0% rural'),
                    uiOutput("ruralText")),
                  boxPlus(
                    title = "County Housing Units",
                    closable = FALSE,
                    status = 'warning',
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = NULL,
                    plotOutput('ctyhousing')
                  ),
                  boxPlus(
                    title = "County Year Built",
                    closable = FALSE,
                    status = 'warning',
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = NULL,
                    plotOutput('ctyyrbuilt')
                  ),
                  boxPlus(
                    title = "County Property Values",
                    closable = FALSE,
                    status = 'warning',
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = NULL,
                    plotOutput('ctyvalue', height = 600)
                  ),
                  boxPlus(
                    title = "County Table - All Variables",
                    closable = FALSE,
                    status = 'warning',
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = NULL,
                    #DT::dataTableOutput("countyTable")
                    #tableOutput('countyTable')
                    gt_output(outputId = "countyTable")
                  ),
                  br()
                )),

# DATA AND METHODOLOGY ---------------------------------------------------------------------------
        tabItem(tabName = "data",
                fluidRow(
                  boxPlus(
                    title = "Data Sources",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,

                    h3("CoreLogic Property Data"),
                    p("CoreLogic is a supplier of proprietary US real estate and specialized business data at the property level. This company provides data spanning over 50 years at the latitude and longitude level. Information available in the dataset includes property characteristics, mortgage, foreclosures and performance. We used the tax assessment data purchased by the Social and Decision Analytics Division  for 2018."),
                    #p("While multiple years of tax assessment data were available, we limited the data to properties assessed in 2018 for consistency across the other datasets we were using. While we profiled all the available CoreLogic variables initially, our final analysis and ACS comparisons focused on the number of bedrooms, the assessed total value, the year a property was built, and the property type. We additionally used the census tract and FIPS variables in the CoreLogic data to enable merging with ACS estimates. For our geocoding analysis, we also focused on the latitude and longitude of a property."),

                    h3("American Community Survey (ACS)"),
                    p("The American Community Survey (ACS) is an ongoing yearly survey conducted by the U.S Census Bureau. ACS samples households to compile 1-year and 5-year datasets providing information on population sociodemographic and socioeconomic characteristics including housing data. We used ACS 5-year (2014-18) estimates to obtain census tract and census block group-level to explore Virginia housing data. Data were accessed using the `tidycensus` package."),
                    #p("Our analysis used the 2018 five-year ACS estimates at the census tract level. We used estimates of the number of housing units in the tract, whether that housing unit was vacant or occupied, and the year a housing unit was built, and the value of a housing unit for comparison with CoreLogic."),

                    h3("Fairfax County Data"),
                    p("Fairfax County provides property assessment data on its open data platform. We used data that had been collected from this platform in 2018 that included 18 property variables."),
                    p("We profiled the 18 variables in the Fairfax dataset. For our analysis, we focused on the following variables of interest:"),
                    tags$ul(
                      tags$li("Assessed Value"),
                      tags$li("Year Built"),
                      tags$li("Square Footage"),
                      tags$li("Lot Size"),
                      tags$li("Number of Bedrooms"),
                      tags$li("Number of Bathrooms"),
                      tags$li("Property Type")),
                    p("The county also provided information on the latitude and longitude of a property, so geocoding this dataset was not necessary."),
                    #p("While we profiled the full Fairfax data, for our analysis, we focused on the variables for property type, assessed value, year built, number of bedrooms, number of bathrooms, lot size, and square footage. The county also provided information on the latitude and longitude of a property, so geocoding this dataset was not necessary."),

                    h3("New Kent County Data"),
                    p("New Kent County provides property assessment data on their website as a downloadable Excel file. We downloaded the most recent version of this data, which was the 2020 property tax assessments for the county."),
                    p("We profiled the full New Kent data. For our analysis, we focused on the following variables:"),
                    tags$ul(
                      tags$li("Assessed Value"),
                      tags$li("Year Built"),
                      tags$li("Square Footage"),
                      tags$li("Lot Size"),
                      tags$li("Number of Bedrooms"),
                      tags$li("Number of Bathrooms")),
                    p("New Kent County did not provide a property type variable or latitude and longitude. See description of geocoding in the Methodology section."),

                    h3(tags$a(href="https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes.aspx", "Rural-Urban Commuting Area (RUCA) Codes")),
                    p("Rural-Urban Commuting Area (RUCA) Codes categorize census tracts based on their population density and commuting flows. They are a product of the United States Department of Agriculture Economic Research Service."),
                    p('We used RUCA codes to classify census tracts as urban or rural. While the codes range from one to ten (see table below for further definitions) we considered "rural" tracts as tracts with codes seven or higher.'),
                    tableOutput('rucatable')),

                  boxPlus(
                    title = "Methodology",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,

                    h3("ACS Linkage"),
                    p("ACS provides estimates of counts for a selected geography (in our case, counts in a given census tract), while CoreLogic provides property-level data. Some assumptions were therefore necessary in order to match counts of CoreLogic property characteristics to ACS estimates."),
                    p('To get the overall count of housing units and the occupancy status for each tract, we considered properties in CoreLogic that were coded as single family dwellings, condos, duplexes, apartments, or commercial condos to be "residential" properties. These residential properties were summed by census tract to get the "occupied" count, and properties coded in CoreLogic as "vacant" were summed to get the "vacant" count. "Total housing units" were considered to be the sum of the occupied and vacant counts. It is worth noting that CoreLogic does not provide much information about how they categorize a property, so for some variables it was difficult to determine whether they should be considered "residential" or not. For example, one of the categories was commercial condo, which could be considered either a commerical or residential property type. Our decisions in grouping the data here could have resulted in a worse fit later when we linked the data to ACS.'),
                    p('To get the count of the year built variables, CoreLogic data were grouped based on its year built. The bins were constructed to match ACS bins; for example, a property constructed in 1975 would be grouped in the "1970-1979" year-built bin. The properties in each bin were then counted for each census tract'),
                    p('To get the count of the housing value variables, CoreLogic data were grouped based on its assessed total value variable. The bins were constructed to match ACS bins; for example, a property valued at $130,000 would be grouped in the "125,000 - 150,000" value bin. The properties in each bin were then counted for each census tract.'),

                    h3("Fitness-for-Use Metric"),
                    p('One challenge of evaluating the coverage of CoreLogic data is that there is no "gold standard" comparison. While ACS data is rigorously collected and evaluated, it is still survey data and may be unreliable, particularly in the rural areas we are most interested in. CoreLogic data may be more accurate than ACS data for variables that are more consistently reported in tax assessments than by individuals on a survey. Therefore, we need to make comparisons in a way that accounts for these differences in the datasets while also providing information about how these differences are exhibited.'),
                    p('We use the following "fitness-for-use" metric to make these comparisons (Keller, Shipp, Orr, et al. 2016).'),
                    (img(src = "ffu_equation.png", width = 450)),
                    p('If the resulting value is negative, this indicates that the CoreLogic value is larger than the ACS estimate. If the value is positive, this indicates that the ACS estimate is larger than the CoreLogic value. When the value falls outside of the -1 to 1 range, this indicates that the CoreLogic value does not fall between the 90 percent ACS margin of error.'),

                    h3("Geocoding"),
                    p("In an attempt to link the properties in the Fairfax and New Kent County data to their CoreLogic counterparts, we used the tidygeocoder R package to access two geocoding engines: the Census geocoder and the Open Street Map (OSM) geocoder. We ran the Census geocoder first, followed by the OSM geocoder for the portion of properties that could not be geocoded by the Census engine, to geocode the portion of the CoreLogic data for which latitude and longitude was missing. We also did this for the New Kent County data, which did not include a latitude and longitude."),
                    p("However, a direct join of the data on the latitude and longitude was not possible because of the differences in the geocoding of the data. CoreLogic's method for geocoding places the location in the center of the property, while the Census geocoder places the location where the property mailbox is located. For larger parcels, these locations can be quite different."),
                    p("Since a direct join didn't work, we attempted to use a minimum distance algorithm to join the properties. The basic idea behind this algorithm is to calculate the minimum distance between every address in a set (in our case, using the st_distance() function in the R package `sf`) and join based on this distance. However, this also was not effective, as a point at the boundary of a large parcel may actually be closer to a point at the middle of a different parcel. This could mean that properties would join incorrectly."),
                    p("Given that two methods of linking the properties on longitude and latitude did not work, we decided to attempt joining on the addresses themselves. This presented some problems of its own, as the addresses could be formatted inconsistently across datasets. One possible solution to this would be to use a USPS API to standardize these addresses. However, this standardization only worked for a subset of addresses. At this point, this would require a combination of manual encoding and encoding with the API to join more of the data. Instead, we present our data profiling results for the county data as another comparison to CoreLogic.")
                  ),
                  boxPlus(
                    title = "References",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,

                    p("Cambon, J. 2020. tidygeocoder: Tidyverse-Style Interface for Geocoding. R packageversion 0.2.5. https://CRAN.R-project.org/package=tidygeocoder"),
                    p("Keller, S. and Shipp, S., Orr, M., et. al. 2016. Leveraging External Data Sources to Enhance Official Statistics and Products.  Report prepared for the U.S. Census Bureau. Social and Decision Analytics Laboratory (SDAL), Biocomplexity Institute of Virginia Tech."),
                    p("Pebesma, E., 2018. Simple Features for R: Standardized Support for Spatial Vector Data. The R Journal 10 (1), 439-446, https://doi.org/10.32614/RJ-2018-009"),
                    p("USPS Web Tools Application Programming Interface User Guide. (2020, June 6). USPS.Com. https://www.usps.com/business/web-tools-apis/address-information-api.htm"),
                    p("Walker, K. 2020. tidycensus: Load US Census Boundary and Attribute Data as 'tidyverse' and 'sf'-Ready Data Frames. R package version 0.9.9.5. https://CRAN.R-project.org/package=tidycensus")

                )
                )),

# PROFILING -------------------------------------------------------------------------------------------
        tabItem(tabName = "profiling",
                fluidRow(
                        boxPlus(title = "Data Profiling",
                                closable = FALSE,
                                width = NULL,
                                status = "warning",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                p("We profiled the CoreLogic data subset for Fairfax County and New Kent County in Virginia. In addition, we profiled local Fairfax County and New Kent County property data. The profiling goal was to compare the completeness of the variables included in each dataset.
                                   Our profiling process focused on the six variables that our sponsors at the USDA highlighted as having the greatest effect on property prices:"),
                                tags$ul(
                                    tags$li("Lot Size"),
                                    tags$li("Square Footage"),
                                    tags$li("Number of Bedrooms"),
                                    tags$li("Number of Bathrooms"),
                                    tags$li("Year Built"),
                                    tags$li("Assessed Value")),
                                    p("To profile these data, we considered the completeness of these variables and the distribution of property types.")
                                           ),
                       boxPlus(title = "Fairfax Profiling - Missing Values",
                               closable = FALSE,
                               width = NULL,
                               status = "warning",
                               solidHeader = TRUE,
                               collapsible = TRUE,
                               p("The following plots compare the percent of rows that are missing in the Fairfax County and Fairfax CoreLogic subset. CoreLogic is missing a greater percentage of rows in Fairfax County than the original data."),
                               img(src = "ffx_county.png", width = 800, align = "center"),
                               img(src = "ffx_cl.png", width = 800, align = "center")),
                       boxPlus(title = "New Kent Profiling - Missing Values",
                               closable = FALSE,
                               width = NULL,
                               status = "warning",
                               solidHeader = TRUE,
                               collapsible = TRUE,
                               p("The following plots compare the percent of rows that are missing in the New Kent County and New Kent CoreLogic subset. CoreLogic is missing a greater percentage of rows in New Kent County than the original data."),
                               img(src = "nk_county.png", width = 800, align = "center"),
                               img(src = "nk_cl.png", width = 800, align = "center")),
                       boxPlus(title = "Fairfax and New Kent Property Value Histograms",
                               closable = FALSE,
                               width = NULL,
                               status = "warning",
                               solidHeader = TRUE,
                               collapsible = TRUE,
                               p('The following plots are overlaid histograms of the property value variable from the original county data and the CoreLogic subset from that county. They are presented on a log scale to more easily see the full range of values. The brown areas on the plots are areas of overlap. Overall, the New Kent CoreLogic subset fits the New Kent original data better than the Fairfax CoreLogic subset fits the Fairfax original data.'),
                               img(src = "ffx_hist.png", width = 840, align = "center"),
                               img(src = "nk_hist2.png", width = 840, align = "center"))
                       )

                ),

# TEAM -------------------------------------------------------------------------------------------
        tabItem(tabName = "team",
                fluidRow(
                  boxPlus(
                    title = "Meet the DSPG Team",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    p("The Data Science for the Public Good (DSPG) Young Scholars program is a summer immersive program held at the Biocomplexity Institute’s Social and Decision Analytics division (SDAD). In its seventh year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around critical social issues relevant in the world today.
                      DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how information generated within every community can be leveraged to improve quality of life and inform public policy."),

                    column(4,
                           h4(tags$a(href="https://github.com/mclaire19", "Madeline Pickens")),
                           (img(src = "maddie.jpg", width = 250, height = 270)),
                           p(""),
                           p("DSPG 2020 Fellow, Georgetown University,"),
                           p("Data Science for Public Policy")
                           ),
                    column(4,
                           h4(tags$a(href="https://github.com/mklutzke", "Morgan Klutzke")),
                           (img(src = "morgan.jpg", width = 250, height = 270)),
                           p(""),
                           p("DSPG 2020 Intern, Indiana University,"),
                           p("Psychology and Cognitive Science")
                           ),
                    column(4,
                           h4(tags$a(href="https://github.com/v-ramanan", "Vatsala Ramanan")),
                           (img(src = "Vatsala_Ramanan.jpg", width = 250, height = 270)),
                           p(""),
                           p("DSPG 2020 Intern, Smith College,"),
                           p("Quantitative Economics and Government")
                           )
                           ),

                  boxPlus(
                    title = "UVA SDAD members",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    h4(tags$a(href="https://biocomplexity.virginia.edu/stephanie-shipp", "Stephanie Shipp")),
                    p("Project Lead, Deputy Division Director and Professor (economics)"),
                    h4(tags$a(href="https://biocomplexity.virginia.edu/joshua-goldstein", "Josh Goldstein")),
                    p("Research Assistant Professor, Statistics"),
                    h4(tags$a(href="https://biocomplexity.virginia.edu/neil-kattampallil", "Neil Kattampallil")),
                    p("Research Scientist"),
                    h4(tags$a(href="https://biocomplexity.virginia.edu/devika-mahoney-nair", "Devika Mahoney-Nair")),
                    p("Research Scientist")),
                  boxPlus(
                    title = "Sponsor",
                    closeable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    h4(tags$a(href="https://www.ers.usda.gov/authors/ers-staff-directory/john-pender/", "John Pender")),
                    p("Rural Liaison, Rural Economy Branch,  Economic Research Service, US  Department of Agriculture"),
                    (img(src = "ers.jpeg", width = 220))
                  )
                )),
# DOWNLOAD -------------------------------------------------------------------------------------------
        tabItem(tabName = "download",
            fluidRow(
              boxPlus(
              title = "Data Download",
              closable = FALSE,
              width = NULL,
              status = "warning",
              solidHeader = TRUE,
              collapsible = FALSE,
              p("The following data sources are provided for download. They present our calculated fitness-for-use statistics for housing variables by Virginia census tracts and for housing variables by Virginia counties. The county-level data includes the percent rurality measure calculated from the RUCA code of the census tracts it contains. The tract-level data contains the RUCA code and county of each census tract."),
              p(tags$a(href='https://github.com/DSPG-Young-Scholars-Program/dspg20broadbandERS/blob/master/output/Public-Data/fitness-for-use-county-level-va.csv', "Download county-level Virginia data here")),
              p(tags$a(href='https://github.com/DSPG-Young-Scholars-Program/dspg20broadbandERS/blob/master/output/Public-Data/fitness-for-use-tract-level-va.csv', "Download tract-level Virginia data here"))
              )))
      )
    ))
),



# SERVER ------------------------------------------------------------------
  server = function(input, output, session) {
    # Run JavaScript Code
    runjs(jscode)

    # Render Housing Plot
    output$ctyhousing <- renderPlot({
      filtervar <- strsplit(input$vacty, "[,]")[[1]][1]
      cty <- ffu_va_housing %>%
        filter(County == filtervar) %>%
        filter(variable != 'Percent Rural')
      cty %>%
        ggplot(aes(value, variable)) +
        geom_point(color = '#2C4F6B', size = 3) +
        geom_vline(xintercept = -1, linetype = "dashed", color = "#E57200") +
        geom_vline(xintercept = 1, linetype = "dashed", color = "#E57200") +
        theme_minimal() +
        labs(title = paste('Fitness-for-Use by Housing Type in', filtervar),
             subtitle = 'When the fitness-for-use metric falls outside the ±1 range (indicated by the dashed orange line), \nthe CoreLogic estimates were not within the 90% ACS margin of error.',
             caption = 'Source: Fitness-for-use metric calculated from 2018 CoreLogic data \n(aggregated by census tract) and 2014-2018 ACS 5-year estimates.') +
        xlab('Average Fitness-for-Use Value') +
        ylab('Variable') +
        theme(text=element_text(size=16,  family=font)) +
        theme(plot.title = element_text(face = "bold"))
    })

    # Render Value Plot
    output$ctyvalue <- renderPlot({
      filtervar <- strsplit(input$vacty, "[,]")[[1]][1]
      cty <- ffu_va_value %>%
        filter(County == filtervar) %>%
        filter(variable != 'Percent Rural')
      cty %>%
        ggplot(aes(value, variable)) +
        geom_point(color = '#2C4F6B', size = 3) +
        geom_vline(xintercept = -1, linetype = "dashed", color = "#E57200") +
        geom_vline(xintercept = 1, linetype = "dashed", color = "#E57200") +
        theme_minimal() +
        labs(title = paste('Fitness-for-Use by Property Value in', filtervar),
             subtitle = 'When the fitness-for-use metric falls outside the ±1 range (indicated by the dashed orange line), \nthe CoreLogic estimates were not within the 90% ACS margin of error.',
             caption = 'Source: Fitness-for-use metric calculated from 2018 CoreLogic data \n(aggregated by census tract) and 2014-2018 ACS 5-year estimates.') +
        xlab('Average Fitness-for-Use Value') +
        ylab('Variable') +
        theme(text=element_text(size=16,  family=font)) +
        theme(plot.title = element_text(face = "bold"))
    })

    # Render Year Built Plot
    output$ctyyrbuilt <- renderPlot({
      filtervar <- strsplit(input$vacty, "[,]")[[1]][1]
      cty <- ffu_va_yrbuilt %>%
        filter(County == filtervar) %>%
        filter(variable != 'Percent Rural')
      cty %>%
        ggplot(aes(value, variable)) +
        geom_point(color = '#2C4F6B', size = 3) +
        geom_vline(xintercept = -1, linetype = "dashed", color = "#E57200") +
        geom_vline(xintercept = 1, linetype = "dashed", color = "#E57200") +
        theme_minimal() +
        labs(title = paste('Fitness-for-Use by Year Built in', filtervar),
             subtitle = 'When the fitness-for-use metric falls outside the ±1 range (indicated by the dashed orange line), \nthe CoreLogic estimates were not within the 90% ACS margin of error.',
             caption = 'Source: Fitness-for-use metric calculated from 2018 CoreLogic data \n(aggregated by census tract) and 2014-2018 ACS 5-year estimates.') +
        xlab('Average Fitness-for-Use Value') +
        ylab('Variable') +
        theme(text=element_text(size=16,  family=font))+
        theme(plot.title = element_text(face = "bold"))
    })

    # render County % Rural Text
    output$ruralText <- renderUI({
      filtervar <- strsplit(input$vacty, "[,]")[[1]][1]
      cty <- va_table_long %>%
        filter(County == filtervar) %>%
        filter(variable == 'Percent Rural')
      pctrural <- round(cty$value*100,1)

      p(paste(filtervar,
               " is ", pctrural, " percent rural based on the RUCA codes of the census tracts it contains. See Data Sources and Methodology for a further explanation of RUCA codes.",
               sep = ""))
    })

    output$countyTable <- render_gt({
      filtervar <- strsplit(input$vacty, "[,]")[[1]][1]
      cty <- va_table_long %>%
        filter(County == filtervar) %>%
        filter(variable != 'Percent Rural') %>%
        mutate(value = round(value, 2)) %>% ungroup() %>%
        dplyr::select(-County) %>%
        rename('Variable' = 'variable') %>%
        gt() %>%
        tab_style(
          style = cell_fill(color = "#D1E0BF"),
          locations = cells_body(
            columns = vars(Variable, value),
            rows = abs(value) <= 1)
        ) %>%
        cols_label(value = 'Fitness-for-Use Metric') %>%
        tab_header(
          title = paste(filtervar, 'Variables'),
          subtitle = "Highlighted (green) rows indicate variables for which CoreLogic estimates fall within the 90% ACS margin of error. Data are not available for every county."
        ) %>%
        fmt_missing(
          columns = 2,
          missing_text = "Data not available"
        ) %>%
        tab_source_note(
          source_note = 'Source: Fitness-for-use metric calculated from 2018 CoreLogic data \n(aggregated by census tract) and 2014-2018 ACS 5-year estimates.'
        )
    })

    output$rucatable <- renderTable({
      ruca_def
    }, striped = TRUE)

    output$rucatable2 <- renderTable({
      ruca_def
    }, striped = TRUE)

    # # Map inputs
    # observe({
    #   x <- input$variable
    #
    #   if (x == "Number of Housing Units") {
    #     y <- character(0)
    #   }
    #   if (x == "Property Value") {
    #     y <- subset(colnames(ffu_merged@data), str_detect(colnames(ffu_merged@data), "value"))
    #   }
    #   if (x == "Year Built") {
    #     y <- subset(colnames(ffu_merged@data), str_detect(colnames(ffu_merged@data), "yrbuilt"))
    #   }
    #   if (x == "Occupancy Status") {
    #     y <- subset(colnames(ffu_merged@data), str_detect(colnames(ffu_merged@data), "occupancy"))
    #   }
    #
    #   updateSelectInput(session, "range", choices = y)
    # })
    #
    # filteredData <- reactive({
    #   if (input$variable == "Number of Housing Units") {
    #     ffu_merged[["ffu_housing_units_total"]]
    #   } else {
    #     ffu_merged[[input$range]]
    #   }
    # })
    #
    # # Render leaflet map
    # output$va_map <- renderLeaflet({
    #   bins <- c(-Inf, -10, -5, -2, -1, 1, 2, 5, 10, Inf)
    #   pal <- colorBin(palette = "BrBG", domain = ffu_merged$ffu_housing_units_total, bins = bins)
    #   label <- paste(ffu_merged$County, "<br/>",
    #                  "Census tract: ", str_sub(ffu_merged$GEOID, 6), "<br/>",
    #                  "RUCA Code: ", ffu_merged$RUCA1, "<br/>",
    #                  "Fitness for Use: ", ifelse(test = is.na(ffu_merged$ffu_housing_units_total),
    #                                              yes = "No Data",
    #                                              no = round(ffu_merged$ffu_housing_units_total, 2)),
    #                  sep = "") %>%
    #     lapply(htmltools::HTML)
    #
    #
    #   leaflet() %>%
    #     addProviderTiles(providers$CartoDB.Positron) %>%
    #     setView(lng = -79.4, lat = 38.177751, zoom = 7) %>%
    #     addPolygons(data = ffu_merged,
    #                 fillColor = ~pal(ffu_housing_units_total),
    #                 color = "#808080",
    #                 fillOpacity = 0.7,
    #                 weight = 1,
    #                 smoothFactor = 0.2,
    #                 label = label) %>%
    #     addLegend(pal = pal,
    #               values = ffu_merged$ffu_housing_units_total,
    #               position = "topleft",
    #               title = "Fitness for Use")
    # })
    #
    # observe({
    #   dat <- filteredData()
    #
    #   bins <- c(-Inf, -10, -5, -2, -1, 1, 2, 5, 10, Inf)
    #   pal <- colorBin(palette = "BrBG", domain = dat, bins = bins)
    #   label <- paste(ffu_merged$County, "<br/>",
    #                  "Census tract: ", str_sub(ffu_merged$GEOID, 6), "<br/>",
    #                  "RUCA Code: ", ffu_merged$RUCA1, "<br/>",
    #                  "Fitness for Use: ", ifelse(test = is.na(dat),
    #                                              yes = "No Data",
    #                                              no = round(dat, 2)),
    #                  sep = "") %>%
    #     lapply(htmltools::HTML)
    #
    #   leafletProxy("va_map", data = dat) %>%
    #     clearShapes() %>%
    #     clearControls() %>%
    #     addPolygons(data = ffu_merged,
    #                 fillColor = ~pal(dat),
    #                 color = "#808080",
    #                 fillOpacity = 0.7,
    #                 weight = 1,
    #                 smoothFactor = 0.2,
    #                 label = label) %>%
    #     addLegend(pal = pal,
    #               values = dat,
    #               position = "topleft",
    #               title = "Fitness for Use")
    # })

  }
)
