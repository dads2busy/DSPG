library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(ggthemes)
library(RColorBrewer)
library(sjmisc)
library(shinythemes)
library(DT)
library(data.table)
library(rsconnect)
library(shinycssloaders)
library(readxl)
library(readr)
library(rgdal)
library(stringr)
library(shinyjs)
library(leaflet)
library(leafem)
library(raster) 
library(stars)
library(rmapshaper)
prettyblue <- "#232D4B"
navBarBlue <- '#427EDC'
options(spinner.color = prettyblue, spinner.color.background = '#ffffff', spinner.size = 3, spinner.type = 7)

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

virginiaCounty <- ms_simplify(st_read("data/VirginiaCounty.shp"))

geotiffFile = "./www/2021_NN_Predictions_2.tif"


# Load raster

my_file = raster(geotiffFile)



# set 0 to NA

my_file[!(my_file > 0)] = NA



# Get Floyd and transform to same CRS as the raster

floyd <- virginiaCounty[5,] %>% st_transform(crs = "+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs")



# crop the raster to the extent of floyd - reduces size greatly again

my_file_crop = raster::crop(my_file, floyd)



# transform floyd to crs that leaflet can plot

floyd <- st_transform(floyd, crs = "+init=epsg:4326")

tags$cite()

# CODE TO DETECT ORIGIN OF LINK AND CHANGE LOGO ACCORDINGLY
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

           var x = document.getElementsByClassName('navbar-brand');

           if (mytype != 'economic') {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/events/symposium2020/poster-sessions\">' +
                              '<img src=\"DSPG_black-01.png\", alt=\"DSPG 2020 Symposium Proceedings\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('dspg');
           } else {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/economic-mobility/community-insights/case-studies\">' +
                              '<img src=\"DSPG_black-01.png\", alt=\"Gates Economic Mobility Case Studies\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('economic');
           }
           "

# user -------------------------------------------------------------
ui <- navbarPage(title = "Analyzing Vegetative Health using Landsat 8 Satellite Imagery",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                 useShinyjs(),
                 # main -----------------------------------------------------------
                 # tabPanel("Home", value = "home",
                 #          fluidRow(style = "margin: 6px;",
                 #                   align = "center",
                 #                   br("", style = "padding-top:10px;"),
                 #                   img(src = "uva-dspg-logo.jpg", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                 #                   br(""),
                 #                   h2(strong("Addressing Barriers to Health in Patrick County, Virginia"),
                 #                   br(""),
                 #                   h4("Data Science for the Public Good Program"),
                 #                   h4("University of Virginia"),
                 #                   h4("Biocomplexity Insititute"),
                 #                   br(),
                 #                   br(),
                 #                   br(),
                 #                   br(),
                 #                   br(),
                 #                   p(tags$small(em('Last updated: August 2020')))
                 #                   )
                 #          )
                 # ),
                 
                 # main -----------------------------------------------------------
                 tabPanel("Overview", value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   # br("", style = "padding-top:2px;"),
                                   # img(src = "uva-dspg-logo.jpg", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                   br(""),
                                   h1(strong("Analyzing Vegetative Health using Landsat 8 Satellite Imagery"),
                                      h3("Data Science for the Public Good Program"),
                                      h4(em("Virginia Polytechnic Institute and State University")),
                                      br(),
                                      br(),
                                   )
                          ),
                          fluidRow(style = "margin: 6px;",
                                   column(3),
                                   column(3,
                                          h4(strong("Project Background")),
                                          p("Human-nature interaction has long been a source of interest, study and analysis in the scientific community. As remote sensing and data analysis technology become more sophisticated, the ability to create robust models to accurately measure and predict environmental health increases. This project was completed through Data Science for the Public Good program at Virginia Tech and seeks to use modern data-science techniques to analyze, predict and draw conclusions from remote-sensing quantifications of vegetation health and water distribution. "),
                                          
                                          
                                          
                                   ),
                                   column(3,
                                          img(src = "Landsat Satellite.jpg", height="100%", width="100%"),
                                          tags$small("The Landsat 8 Satellite. Image Source: United States Geological Survey")
                                   )
                                   
                          ),
                          fluidRow(
                            column(3),
                            column(3, align = "right",
                                   img(src = "Google Earth NDWI.jpg", height="100%", width="100%", align = "right"),
                                   tags$small("An aerial view of NDWI. Image Source: Google Earth Engine"),
                            ),
                            column(3,
                                   h4(strong("Brief Overview")),
                                   
                                   p("The data used throughout this project was primarily scraped from the United States Geological Survey’s database of Landsat 8 Satellite Images. These images were then used to calculate specific indicators of vegetative health and water distribution. Images of the same region over time were used to train a machine learning model to be able to predict these indicators over time and, lastly, the model was applied to a case study in Floyd County, Virginia."),
                                   p("The research team also delved into attempting to predict depth-to-water in remote communities based on elevation, indicators of water-density and precipitation data collected from the USGS and Google Earth Explorer.")
                                   
                            )
                            
                          ),
                          
                          fluidRow(
                            column(3),
                            column(6,
                                   align = "center",
                                   tags$br(),
                                   h3(strong("Dashboard Aims"), align = "center"),
                                   h4(em("Our Dashboard is Aimed at:"), align = "center"),
                                   tags$br(),
                            )
                          ),
                          fluidRow(
                            column(3),
                            column(6,
                                   h4(strong("Researchers and practitioners working on remote-sensing data: ")),
                                   p("Information available through the interface would provide insights on utilization of Landsat 8 satellite imagery for construction of images relevant to gauging vegetative health. Information on construction of these indices and their various usage would provide both researchers and practitioners preliminary guidelines on using Landsat 8 data. Our Neural Network (NN) model also provides information on how to use these data to form prediction models which can be further utilized in designing research-informed policies. "),
                                   tags$br(),
                                   h4(strong("Researchers working on hydrology in data scarce areas: ")),
                                   p("Our interface provides insights into potential avenues to explore for indirect estimation of water resources in areas which suffer from a lack of sufficient data. This would help researchers identify ways to model available data to estimate and map out the water resources in such data scare areas. "),
                                   tags$br(),
                                   h4(strong("Agriculture and Environmental Policy Designing Agencies: ")),
                                   p("These and similar stakeholders might use the models designed and presented through the interface to build insights into how agriculture and environmental policy can be designed using remote-sensing data in the fields of precision agriculture, planned farming, and climate change impact measurement and management.")
                                   
                            )
                          )
                 ),
                 
                 # older -----------------------------------------------------------
                 tabPanel("Landsat 8 Data", value = "landsat",
                          fluidRow(style = "margin: 6px;", align = "center",
                                   h1(strong("Using Landsat 8 Images"), align = "center"),
                                   #p("", style = "padding-top:10px;"),
                          ),
                          
                          fluidRow(
                            column(3),
                            column(3,
                                   h4(strong("Introduction")),
                                   p("The Landsat 8 Satellite was launched in 2013 by NASA to collect high-resolution and electromagnetically diverse remote radiation data about the Earth’s surface. The Landsat senses eleven distinct wavelength ranges of light, from the visible red, green and blue wavelengths to infrared wavelengths for thermal imaging. These diverse ranges of sensing data hold the ability to filter and provide insight into aspects of regions that do not appear visible in a standard RGB photograph. Landsat 8 also has a relatively high resolution, with each pixel in most captured images corresponding to 30 meters of land area. In the panchromatic channel, used for detail, the satellite reaches a detail rating of 15 meters per pixel.")
                            ),
                            column(3,
                                   img(src = "Picture1.png", style = "text-align:left;", height="100%", width="100%"),
                                   tags$small("Image Source: United States Geological Survey")
                            )
                            
                          ),
                          fluidRow(
                            column(3),
                            column(3, align = "right",
                                   img(src = "Picture2.png", style = "text-align:left;", height="100%", width="100%", align = "right")
                            ),
                            column(3,
                                   h4(strong("Landsat Bands")),
                                   p("The second significant advantage of using Landsat 8 satellite imagery is the diversity of wavelengths of light captured in each photograph. These wavelengths of light allow for the construction of specific indicators and insight into a wide array of information like aerosols, clouds, water and temperature. To the left is a table that details the different bands and their respective uses. ")
                            )
                            
                            
                          ),
                          
                          fluidRow(
                            column(3),
                            column(3,
                                   h4(strong("GeoTiff Files")),
                                   p("The Landsat 8 Data can be downloaded via the",  a(href = 'https://earthexplorer.usgs.gov/', ' USGS Earth Explorer '), "into large GeoTiff files. These files are then read as matrices of specific intensity values depending on which wavelength is being examined. Landsat GeoTiff files tend to get rather large, often being over 1gb in size because of the detail they capture of a particular region without falling victim to any kind of noise compression. "),
                                   p("The Landsat captures images corresponding to roughly 250x250 kilometer sections of earth. The different bands can be 
                                          combined to form all sorts of useful secondary images synthesized from the raw wavelengths. The image to the right is a true-color 
                                          synthesis of the red, green and blue bands of an image of Los Angeles, California. ")
                            ),
                            column(3,
                                   img(src = "LALandsatImg.jpg", style = "display: inline; float: center;", height="100%", width="100%"),
                                   tags$small("Image Source: United States Geological Survey")
                            )
                            
                          )
                          
                 ),
                 
                 # wifi-----------------------------------------------------------
                 tabPanel("Derived Indices", value = "connectivity",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Derived Indices"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(3),
                                   column(6, align = "center",
                                          #h4(strong("Computing Device Ownership and Internet Access Type")),
                                          p(style = "text-align: justify;", "Oftentimes, measurements of natural phenomena are too complex to be accurately described solely with remote sensing. Calculated indices bridge the gap between satellite imagery and internal vegetative processes. The indices of particular interest to this project are the Normalized Difference Vegetative Index (NDVI) and the Normalized Difference Water Index (NDWI). NDVI is strongly correlated to the overall health of plant foliage. The Normalized Difference Water Index is strongly correlated to the amount of water held in plant leaves. Foliage health and water density are both complex phenomena that would be impossible to calculate for each individual pixel of a satellite image, but with the help of these indicators, we can synthesize a good idea of vegetative health using relatively little input. The different wavelengths of light captured by the Landsat 8 satellite can be used to produce the NDVI and NDWI indices of interest for each individual pixel of a remote sensing image.")       
                                   )
                                   
                          ),
                          fluidRow(
                            column(3),
                            column(3,
                                   p(strong(style = "text-align: justify;","Normalized Difference Vegetative Index")),
                                   p(style = "text-align: justify;","The Normalized Difference Vegetative Index is derived from the Near Infrared light and Red light emitted from plants. It is described in detail in Nathalie Pettorelli’s book “The Normalized Difference Vegetative Index”. The Landsat 8 satellite captures both wavelengths of light and the USGS provides a formula for producing this index:"),
                                   
                                   p(strong(align = "center","NDVI = (NIR - R) / (NIR + R)")),
                                   p(style = "text-align: justify;", "This allows us to create aerial maps of NDVI for a particular region by combining individual pixel values. A Map of the NDVI of Southwest Virginia and Southern West Virginia is shown to the right:"),
                                   
                                   p(style = "text-align: justify;", "From these types of aerial maps of derived indices, conclusions about distribution and trends in the vegetative health over time and throughout the region can be made. The Normalized Difference Vegetative Index has been used in applications such as precision agriculture, drought monitoring, flooding and precipitation patterns. The aim of this project is to predict the NDVI by combining the band data from the Landsat 8 satellite to give an accurate prediction of how the NDVI will change over time.")
                            ),
                            column(3,
                                   img(src = "NDVI_2016_NRV.png", style = "display: inline", height="120%", width="120%"))
                          ),
                          fluidRow(
                            column(3),
                            column(3, align = "right",
                                   img(src = "NDWI_2016_NRV.png", style = "display: inline;", height="100%", width="100%")),
                            column(3,
                                   p(strong(style = "text-align: justify;","Normalized Difference Water Index")),
                                   p(style = "text-align: justify;","The Normalized Difference Water Index (NDWI) is highly correlated with the amount of water stored in the foliage of plants, as described in Bo-cai Gao’s paper, “A normalized difference water index for remote sensing of vegetation liquid water from space”. The NDWI is sometimes described as the Normalized Difference Water Index. The USGS also provides a formula to calculate the NDWI by combining the Short-Wave Infrared with the Near Infrared wavelengths of light captured from the Landsat 8 satellite in the following formula:"),
                                   
                                   p(strong(align = "center","NDWI = (NIR – SWIR) / (NIR + SWIR)")),  
                                   
                                   p(style = "text-align: justify;","Like the NDVI, this formula allows for per-pixel calculation of this index to describe the distribution of water in vegetation throughout the new river valley, as shown in the image to the left:"))
                          )
                 ),
                 
                 # ems -----------------------------------------------------------
                 tabPanel("Machine Learning", value = "ml",
                          
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Machine Learning Methodology"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(3),
                                   column(6,
                                          p(style = "text-align: justify;", "The
                                          main goal of this project is to use modern 
                                          data-science techniques to produce a model 
                                          that is able to predict the Normalized 
                                          Difference Vegetative Index and the Normalized 
                                          Difference Water Index to a high degree 
                                          of accuracy at a high resolution using 
                                          existing wavelength data captured by the 
                                          Landsat 8. This opens the door to identify 
                                          trends and areas of concern within a region 
                                          that might otherwise look healthy. The 
                                          tool used to accomplish this process is 
                                          a feed-forward neural network that is trained 
                                          off of existing satellite pairs. The model 
                                          trained in this paper looks at an ability 
                                          to predict vegetative health in two year 
                                          intervals. The area of interest in this 
                                          project is the New River Valley of Southwest 
                                          Virginia and specifically Floyd County, 
                                          Virginia. This is the region that the model 
                                          was initially trained on. Raw satellite 
                                          images were downloaded from the Earth Explorer 
                                          via the United States Geological survey 
                                          as Geotiff images of the different wavelengths 
                                          of light captured. A great deal of data 
                                          cleaning, conditioning and filtering had 
                                          to be performed before the images could 
                                          be fed into a machine learning model. The 
                                            first such problem came when the raw 
                                            images were encoded as 16-bit positive 
                                            integer values, and needed to be converted 
                                            to 64-bit floating point values in order 
                                            to be manipulated. Otherwise, non-natural 
                                            values result in high-values instead 
                                            of negative ones that make the resulting 
                                            and processing useless. The other 
                                            technical nuances encountered when using 
                                            the raw satellite images are described in the following sections.")),
                                   
                                   
                          ),
                          tags$br(),
                          tags$br(),
                          fluidRow(
                            column(3),
                            column(3,
                                   h4(strong("Top of Atmosphere Reflectance")),
                                   p("Because this model seeks to forecast vegetative health in two year intervals, it is imperative that “before” and “after” data is gathered. Therefore, our team chose Landsat images taken of the exact same region of Southwest Virginia. The problem we encountered was that the satellite images were macroscopically identical, but the team soon realized upon close examination that the images were ever so slightly displaced from each other. This makes sense, as it is quite nearly impossible for the satellite to make its way back to the exact same location and orientation two years after taking a photograph. The original GeoTiff Images overlaid upon one another is shown to the right to visualize the distortion:")
                            ),
                            column(6,
                                   img(src = "Picture10.png", style = "display: inline"),
                                   p(tags$small("Reference: United States Geological Survey ")))
                          ),
                          tags$br(),
                          tags$br(),
                          fluidRow(
                            column(3),
                            column(3, align = "right",
                                   img(src = "Picture11.png", height="100%", width="100%", align = "right"),
                                   p(tags$small("A measure of NDVI after the region was filtered with the filtration algorithm. Note the absence of Urban areas such as Roanoke and Blacksburg/Christiansburg, Smith Mountain Lake and a large overhanging Cloud on the Scene.  ")),
                            ),
                            column(3,
                                   h4(strong("Filtering Data")),
                                   p(style = "text-align: justify;", "It is important to note that the focus of this project is on vegetation. As it happens, when a satellite snaps a photograph of the earth, it tends to capture much more than just vegetation. Therefore, the team needed a way to filter out clouds and their shadows, water and non-vegetative urban areas. The team used the 9th Band of the Landsat 8 satellite to filter out clouds and developed an algorithm that searched for shadows based on dips in reflectance around clouds. The team also employed a method to filter out the lakes, ponds and rivers in a scene primarily using the Near Infrared Band (Band 5) in combination with others. This method is described in the paper Identification of Water Bodies in a Landsat 8 OLI Image Using a J48 Decision Tree. Once all of these filtration algorithms were applied, the satellite image of Southwest Virginia looked accordingly: "),
                            )
                          ),
                          tags$br(),
                          tags$br(),
                          fluidRow(
                            column(3),
                            column(3,
                                   h4(strong("Aligning Photographs")),
                                   p(style = "text-align: justify;", "Because this model seeks to predict vegetative health in 2-year intervals, it is imperative that “before” and “after” data is gathered. Therefore, we chose landsat images taken of the exact same region of Southwest Virginia. The only problem was that the satellite images were virtually identical, but once the team began examining them closely, we realized that they were ever so slightly off. A detailed image of the displacement is shown below: "),
                            ),
                            column(6,
                                   img(src = "Picture12.png", style = "display: inline"))
                          ),
                          tags$br(),
                          tags$br(),
                          fluidRow(
                            column(3),
                            column(3,
                                   img(src = "Picture13.png", style = "display: inline", height="100%", width="100%", align = "right")),
                            column(3,
                                   p("Because of this technical hurdle, the team had to develop an algorithm to align the before and after images to line up at the individual pixel level. This was imperative because the resolution was so high and each pixel of the image only accounts for a 30m x 30m section of land. Therefore, the team had to be very precise in our corrections between these two images. We managed to accomplish this by subtracting the Band 5 Near Infrared (NIR) light reflectance for the two years and examining the change. This produced large color discrepancies in major landforms like water, lakes, ponds and urban areas which were easy to identify and correct for. Once the pictures were properly aligned, the distinct changes in color disappeared. A visualization of an example correction via subtracting the NIR bands is shown to the left. Correcting for these disparities in alignment turned out to be crucial for the accuracy of our neural network in its ability to truly predict the change from one year to another. ")
                            )
                            
                          ),
                          tags$br(),
                          tags$br(),
                          fluidRow(
                            column(3),
                            column(6,
                                   h4(strong("Creating Subsets")),
                                   p("After the necessary filtration was performed, the team had to be specific about how the machine learning model would interact with the GeoTiff images. The method the team settled on was to split up the rasterized image into tiny 20x20 pixel subsets. Those subsets would form the basis for what the neural network would be able to see of a particular area. The output that the neural network tries to predict would be the average change in NDVI and NDWI in that particular 20x20 square. It is also important to note that these 20x20 pixel sections each contain 11 different wavelengths of light. The team elected to exclude the Panchromatic band because its pixel dimensions did not match the original images and offered little extra information other than geographic detail. However, this meant that each 20x20 image contained 400 pixels, and each pixel contained 10 bands, which meant that at least 4000 inputs of just pixel data would be fed to the neural network. An example of the input and outputs data available to the model is visualized below.")
                            ),
                            
                          ),
                          fluidRow(
                            column(3),
                            column(6,
                                   align = "center",
                                   img(src = "Subset.JPG", align = "center", width = "100%")
                            )
                          ),
                          tags$br(),
                          tags$br(),
                          fluidRow(
                            column(3),
                            column(3,
                                   img(src = "Picture15.png", style = "display: inline", height="100%", width="100%", align = "right")),
                            column(3,
                                   h4(strong("Building and Training a Neural Network")),
                                   p("Once the 20 x 20 subsets of input and output were identified and assembled, they needed to be conditioned in order to be fed to a model. The team used the MinMaxScaler from the Sklearn package in python to scale the intensities down to a continuous feature range between 0-1. These inputs had to be linked with the corresponding changes in NDVI and NDWI observed two years later and then saved as compressed numpy arrays. "),
                                   p("As mentioned previously, a 20x20 pixel square with 10 distinct bands of data means a minimum of an input size of 4000 for a neural network. Not only this, but when the sorting algorithm finished identifying possible subsets, it identified 100,000 of them in a single photo. This means that over 400 million data points would need to be passed through the model. The team also included an input for the time of year that the photo was taken. Because the size of the input was so large, the team constructed a feed-forward neural network with four hidden layers and a total of 5,475 neurons, connected by over 4,000,000 weights. "),
                                   p("The training data was randomized and then split up into 85,000 subsets to train the data. The remaining 16,000 were set aside to be the validation for the model’s accuracy. This process was repeated with different time periods and regions of Virginia to increase the versatility of the model. The structure of the Neural Network is shown to the left.")
                                   
                            )
                            
                          ),
                          tags$br(),
                          tags$br(),
                          fluidRow(
                            column(3),
                            column(3,
                                   h1(strong("Machine Learning Results"), align = "center"),)
                          ),
                          fluidRow(
                            column(3),
                            column(3,
                                   h4(strong("Testing Accuracy ")),
                                   p("The testing results below are for two different time periods of Southwest Virginia and an Image of Central Virginia. The accuracy percentage is the average percentage correct of predicted results and on the true values. The Loss Function used in this training was Means Absolute Error, which is the absolute value of the distance from the predicted values to the true values. The Loss values in the table are the average losses for all 16,000 test samples. The accuracy for Central Virginia is likely lower than the Southwest Virginia samples because it contained the least amount of training data compared to the previous two.")
                            ),
                            column(3,
                                   tags$br(),
                                   img(src = "Picture16.png", style = "display: inline", height="100%", width="100%")),
                            
                          ),
                          fluidRow(
                            column(3),
                            column(3,
                                   img(src = "Picture17.png", align = "right", height="100%", width="100%")),
                            column(3,
                                   h4(strong("Visualizing Accuracy ")),
                                   p("The error in training the model over time is visualized to the left for the data in Southwest Virginia from 2014 to 2016. This shows how the model efficiency decreased the error at an exponential rate and after multiple epochs honed in an astoundingly accurate model at over 99% accuracy. The size of the dataset also ensures that this is not an overfitting problem as this model performed at 99% or better for over 100,000 data points. To compare this to statistics, this is the equivalent of having a P-Value on the order of 1e-17 or a Z-Score 83 standard deviations above the mean. This has huge implications for policy and being able to predict environmental health, which will be spoken about in detail in the latter tabs of this project.")
                            )
                            
                          )
                 ),
                 
                 tabPanel("Predicting NDVI", value = "predictions",
                          fluidRow(
                            h1(strong("Predicting Vegetative Health in Floyd County, Virginia"), align = "center"),
                            column(3),
                            column(6,
                                   h4(strong("Overview")),
                                   p("Floyd County is located in Southwest Virginia, suffers from seasonal water scarcity and has observed a gradual decline in groundwater level over the last few decades. The county relies heavily on well water and natural springs for the bulk of its residential and commercial water requirements. This makes estimation of the county’s water resources essential for any potential residential and industrial growth in order to make informed water management plans. However, the county currently has very limited data on its water resources."),
                                   p("The Normalized Difference Vegetation Index is a powerful indicator to describe vegetative health. With the model synthesized in the aforementioned chapters, it is now possible to provide a high-resolution prediction for the year 2023 of the distribution of vegetative health in Floyd County, Virginia.")
                            )
                          ),
                          fluidRow(
                            column(3),
                            column(2,
                                   h4(strong("Methodology")),
                                   p("A Landsat 8 satellite image taken of Southwest Virginia in 2021 was broken up into 20x20 subsets and decomposed into the 11 different bands of light captured by the Landsat 8 satellite. For each 20x20 square, the data was fed into the neural network trained in the previous section and the predicted value for NDVI for the year 2023 was stored. The predictions were then combined as their own GeoTiff file and then plotted on a map within Floyd County, as shown in the graphic to the right."),
                                   h4(strong("Observations")),
                                   p("Notice how the prediction of NDVI stays true to the contours of the mountain range at the southeast edge of the county. This is indicative of the robustness of the model and how different landforms, elevations and vegetative conditions are taken into account to produce an accurate prediction of a given area. Drag and Zoom out to see the predictions for the entire region of Southwest Virginia.")
                                   
                            ),
                            column(4,
                                   h4(strong("NDVI Predictions for August 2023")),
                                   withSpinner(leafletOutput("NDVIMap")), 
                                   tags$small("Please allow up to a minute for the graphic to load. Refresh if nothing loads.", align = "center")
                            )
                            
                            
                          )
                 ),
                 
                ##NDVI Predictions -----------------------------------------------------------
                 tabPanel("Well-Depth Prediction", value = "welldepth",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Predicting Well-Depth in Floyd County, Virginia"), align = "center"),
                                   column(3),
                                   column(6,
                                          h4(strong("Overview")),
                                          p("The Landsat 8 imagery provide a rich dataset that can be utilized for various purposes. One major application of the constructed indices in our project was in the prediction of water levels in areas which suffer from scarcity of available data. Groundwater consumption has become a critical element of development in areas with overall and/or seasonal water scarcity. Excessive withdrawal from groundwater sources might prove to be unsustainable unless the groundwater aquifers are regularly replenished."),
                                          p("Unfortunately, groundwater use is difficult to monitor globally and even in the U.S., wells that are drilled on private property can be exempt from official monitoring. This is also the case for the region under study here, Floyd County, Virginia. Due to lack of official monitoring, there is an acute dearth of well water data for the entire county."),
                                          p("Because Floyd County suffers from seasonal water scarcity and a gradual decline in groundwater level, this makes estimation of the county’s water resources essential for any potential residential and industrial growth in order to make informed water management plans. The aim of this section of the project is to use a machine learning model to give an accurate predictor of well-levels on areas without established well-sites.")
                                   )
                          ),
                          fluidRow(
                            column(3),
                            column(6,
                                   h4(strong("Well-Depth Methodology")),
                                   p("Our project employed a model to predict the well water level within the county utilizing the constructed indices and other readily available data on elevation and precipitation for the county. The NDWI values were hypothesized to be indicative of changes in groundwater levels across seasons over years. Elevation changes significantly impact NDWI values and were hence included in the model. Precipitation is also a major source of groundwater replenishment within most areas in Virginia and was hence also include in the model. "),
                                   p("The estimation of the water table level was performed through a Long Short-Term Memory (LSTM) network, which is a Recurrent Neural Network (RNN) architecture used in machine learning. The model used data on well water levels (measured in feet below land surface), taken from ten well sites documented under USGS for counties surrounding Floyd. Based on the location (latitude and longitude) of these well sites, corresponding data on NDWI values, elevation of the sites as well as precipitation values from the year 2012 to 2021 was added from Google Earth Engine. The resulting panel dataset was analyzed using a LSTM model, to get temporal predictions of well water level from the training data for the various well sites. This model was also used to spatially and temporally predict the well water level at Floyd given the county’s location, and the elevation, NDWI and precipitation values for the county. ")
                            )
                          ),
                          fluidRow(
                            column(3),
                            column(3,
                                   h4(strong("Well-Depth Prediction Results")),
                                   p("Unfortunately, the model was not able to provide an accurate prediction of the true well-depth for a given area. The model hovered around a 75% accuracy in the training sets, and a 48.27% accuracy when being tested. A graph of the model’s prediction of well-depth over time is shown in the graph to the right.  This shows that well-depth depends on more than the data our team had access to and the need for further research to be able to predict water-table levels is high. This is extremely important to regions like Floyd who do not have the means to drill wells in private locations and need a remote sensing tool to estimate groundwater levels in these inaccessible areas. The results for the last well to be tested are shown to the right")
                                   
                            ),
                            column(3,
                                   # This is where well-depth prediction graph goes
                                   img(src = "WellDepthPredictions.png", width = "100%")
                            )
                          ),
                          
                          fluidRow(
                            column(3),
                            column(6,
                                   h4(strong("Limitations")),
                                   p("One major limitation of the model was the lack of training data used in the LSTM model. Given the dearth of data in counties surrounding Floyd, the data used for training the model came from only ten well sites. Even within these well sites, the well water level data is sporadic across months. This is often coupled with the lack of corresponding NDWI data for the specific date ranges due to the limitations of satellite data collection, which often suffers due to any kind of atmospheric disturbances. This lack of sufficient data required for training the model might result in significant underfitting of the model which would result in biased predictions.  "),
                                   p("Our model also does not include other variables relevant variables which might significantly impact water table level predictions. These factors include geological variables like soil type, permeability of the soil, as well as topology changes within the county, which all determine the extent to which groundwater can be replenished. Other relevant variables would include vegetation type and the extent of homogeneity of the vegetation, which would also impact the NDWI variations within a region. These factors can be further explored in future research on well water predictions using NDWI. ")
                            )
                          )
                 ),
                 
                 tabPanel("Policy Implications", value = "policy",
                          fluidRow(
                            h1(strong("Policy Implications"), align = "center"),
                            tags$br()
                          ),
                          
                          fluidRow(
                            column(3),
                            column(6,
                                   p("Our Neural Network (NN) model tests the possibility of predicting NDVI and NDWI values for a particular geographic region given past NDVI and NDWI values for the region. The high degree of predictability of our model opens up myriad possibilities for the application of these constructed indices in various environmental as well as agriculture-related policy designing. The existing literature on these constructed indices apply these indices in precision agriculture and planned farming. These measures are also used in various environmental policy design, including management of forest fires, drought management, coastal and inland flooding, urban green space management, etc. NDVI is an index primarily related to canopy chlorophyll content (Jackson et al., 2004) and one of the most widely used indexes for the remote sensing of vegetation (Piragnolo et al., 2014; Gao et al., 1996). The NDWI is a measure of liquid water molecules in vegetation canopies that interact with the incoming solar radiation (Gao et al., 1996), specially conceived for the estimate of soil moisture and canopy water content (Jackson et al., 2004; Sánchez-Ruiz, 2014). The NDWI is often a function of local climate and soil properties controlling water availability (Sánchez-Ruiz, 2014), and sensitive to changes in liquid water because it incorporates a short-wave infrared (SWIR) band. This green vegetation spectra region is dominated by water absorption effects that capture important information on seasonally variable water status (Sánchez-Ruiz, 2014; Wang et al., 2011). NDWI has been used extensively for monitoring the status of the vegetation water content over large areas from space (Serrano et al., 2019).  "),
                                   tags$br()
                            ),
                            
                          ),
                          
                          fluidRow(
                            column(3),
                            column(6,
                                   h4(strong("Applications in Precision Agriculture and Planned Agriculture")),
                                   tags$br(),
                                   p("Agricultural applications in current times require more and more computer vision technologies for continuous monitoring and analysis of crop health and yield. Machine learning has thus become one of the mechanisms that make farming more efficient by using high-precision algorithms. Precision agriculture technology enables better identification, analysis, and management of temporal and spatial in-field variability of crop production. Precision agriculture is all about reducing this variability through more focused and targeted efforts which increase production by maintaining crop quality and quantity. This can be made more efficient with aerial imagery collected with drones using specialized sensors. In precision agriculture, NDVI is used to measure biomass. Vegetation indices can be averaged over time in order to establish growing conditions for a given time of the year. By studying the time dependence of vegetation indices, we can reveal vegetation stress as well as the influence of human activities."),
                                   p("Previous studies found that NDVI was appropriate to reveal soil moisture (Gu et al. 2008), which is an important factor for crop management. NDVI has been shown to be an effective and widely used indicator of spatio-temporal changes in vegetation growth and distribution (Fensholt, 2009), vegetation stress (Karnieli, 2010), and vegetation productivity (Gitelson, 2014).")
                            )
                          ),
                          fluidRow(
                            column(3),
                            column(6,
                                   h4(strong("Applications in Environmental Policy Design ")),
                                   tags$br(),
                                   p("NDVI and NDWI have proved to be useful variables in assessing urban heat island. The analysis of the correlation of these spectral indices by land use and land cover types and found that NDWI varied between 0.74–1.00 (lowest was in case water bodies and largest was in case of urban cover pattern) (Ogashawara and Bastos, 2012). NDVI and NDWI also were successfully applied in the study of analyzing urban heat island and land cover change (Chen et al., 2006). Prior studies have also found both NDVI and NDWI useful indicators in environment monitoring (Liu et al. 2009; Bakar et al. 2016). "),
                                   p("The temporal tracking of vegetation mass with spectral measures has been widely investigated, especially in water-limited regions. The accurate assessment of the seasonal dynamic of drought in these regions by the use of remote-sensing and field observations is essential to determination of the major constraints of such ecosystems (Chakroun et al., 2015). "),
                                   p("Modern techniques of remote sensing provide tremendous potential for monitoring and managing dynamic changes in large surface water bodies, extracting hydrological parameters, and modeling the water balance (El Bastawesy et al., 2015, El-Gamily et al., 2010; Memon et al., 2012). The Normalized Difference Water Index (NDWI) using Near Infrared (NIR) and green channels of Landsat that can delineate and enhance open water features was proposed as a suitable dataset for monitoring flooding and performing flood damage assessment (McFeeters, 1996). There also exists models which look into flood monitoring through integrated water body mapping method through combination of difference between NDVI and NDWI (NDVI–NDWI) with slope and NIR band using HJ-1A/B satellite images (Lu et al. 2011). "),
                                   p("In forestry, NDVI is often used to quantify forest supply and leaf area index. This alongside NDWI is often used in management of forest fires. Plant humidity is an important indicator for wildfires monitoring and for identifying possibly dangerous regions. Low humidity contributes to environment susceptible to wild fires, especially if it corresponds to ecosystems where live and dry vegetation coexist."),
                                   p("NDWI can also be of great interest as a support to decision making in terms of pasture and grazing management (Serrano et al., 2019). ")
                            )
                          )
                          
                 ),
                 
                 # contact -----------------------------------------------------------
                 tabPanel("Our Team", value = "team",
                          fluidRow(column(3),
                                   column(6,
                                          h1(strong("Contact"), align = "center"),
                                          br(),
                                          h4(strong("Virginia Tech Data Science for the Public Good")),
                                          p("The", a(href = 'https://aaec.vt.edu/academics/undergraduate/beyond-classroom/dspg.html', 'Data Science for the Public Good (DSPG) Young Scholars program', target = "_blank"), 
                                            "is a summer immersive program held at the", a(href = 'https://aaec.vt.edu/s', 'Virginia Tech Department of Agricultural and Applied Economics.'), 
                                            "In its second year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program highlights, how to apply, and our annual symposium, please visit", a(href = 'https://aaec.vt.edu/academics/undergraduate/beyond-classroom/dspg.html', 'the official VT DSPG website.', target = "_blank")),
                                          p("", style = "padding-top:10px;")
                                   )
                                   
                          ),
                          fluidRow(
                            column(3),
                            column(2, align = "center",
                                   h4(strong("Graduate Fellows")), tags$br(),
                                   img(src = "fellow-Esha.JPG", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width="75%"),
                                   tags$br(), p(a(href = 'https://www.linkedin.com/in/esha-dwibedi-83a63476/', 'Esha Dwibedi', target = '_blank'), "(Virginia Tech, Behavioral and Experimental Economics)"),
                                   tags$br(), img(src = "fellow-seth.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width="65%"),
                                   tags$br(), p(a(href = 'https://www.linkedin.com/in/aviseth/', 'Avi Seth', target = '_blank'), "(Virginia Tech, Computer Science);")
                            ),
                            column(2, align = "center",
                                   h4(strong("Faculty Advisor")), tags$br(),
                                   img(src = "faculty-posadas.jpg", width="50%"), tags$br(),
                                   p(a(href = 'https://www.linkedin.com/in/briannaposadas/', 'Dr. Brianna Posadas', target = '_blank'), "(Postdoctoral Associate, Department of Agricultural, Leadership, & Community Education, Virginia Tech)"),
                                   img(src = "VT Logo.jpg", width="75%")
                            ),
                            column(2, align = "center",
                                   h4(strong("Undergraduate Interns")), tags$br(),
                                   img(src = "team-rex.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width="75%"), tags$br(),
                                   p(a(href = "https://www.linkedin.com/in/atticus-rex-717581191/", 'Atticus Rex', target = '_blank'), "(Virginia Tech, Mechanical Engineering & Computational Modeling and Data Analytics)"), tags$br(),
                                   img(src = "team-mukora.PNG", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width="75%"),
                                   p(a(href = "www.linkedin.com/in/victormukora", 'Victor Mukora', target = '_blank'), "(Virginia Tech, Computational Modeling and Data Analytics)")
                                   
                            )
                            
                          ),
                          fluidRow(column(3),
                                   column(6,
                                         
                                          h4(strong("Acknowledgments:")),
                                          p("The team would also like to extend its gratitude to", a(href = 'https://spes.vt.edu/faculty-staff/faculty/seyyedhasani-hasan.html', 'Dr. Hasan Seyyedhasani', target = "_blank"), 
                                            "and", a(href = 'http://www.uwyo.edu/wygisc/people/yang_di/di-short-cv.html', 'Dr. Di Yang'), 
                                            "who helped us immensely with the data collection process and making us understand the nuances behind the project goals.")),
                                          p("", style = "padding-top:20px;"),
                                          br()
                                   )
                 ),
                 tabPanel("References", value = "references",
                          column(3),
                          column(6, 
                                 h1(strong("References"), align = "center"),
                                 p("Acharya, T., Lee, D., Yang, I., & Lee, J. (2016). Identification of water bodies in a Landsat 8 Oli image using A j48 decision tree. Sensors, 16(7), 1075. https://doi.org/10.3390/s1607107"),
                                 p("Akhtar Ali Memon, Sher Muhammad, Said Rahman, Mateeul Haq, Flood monitoring and damage assessment using water indices: A case study of Pakistan flood-2012, The Egyptian Journal of Remote Sensing and Space Science, Volume 18, Issue 1, 2015, Pages 99-106, ISSN 1110-9823. "),
                                 p("Bakar, S.B.A. – Pradhan, B. – Lay, U.S., Abdullahi, S. (2016): Spatial assessment of land surface temperature and land use/land cover in Langkawi Island. 8th IGRSM International Conference and Exhibition on Remote Sensing & GIS (IGRSM 2016) IOP Publishing IOP Conf. Series: Earth and Environmental Science 37 (2016) 012064 doi:10.1088/1755- 1315/37/1/012064"),
                                 p("Bao, Z., Zhang, J., Wang, G., Guan, T., Jin, J., Liu, Y., Li, M., & Ma, T. (2021). The sensitivity of vegetation cover to climate change in multiple climatic zones using machine learning algorithms. Ecological Indicators, 124, 107443. https://doi.org/10.1016/j.ecolind.2021.107443 "),
                                 p("Chakroun, H.; Mouillot, F.; Hamdi, A. Regional equivalent water thickness modeling from remote sensing across a tree cover/LAI gradient in Mediterranean forests of Northern Tunisia. Remote Sens. 2015, 7, 1937–1961. "),
                                 p("Chen, X.-L. – Zhao, H.-M. – Li, P.-X. – Yin, Z.-Y. (2006): Remote sensing image-based analysis of the relationship between urban heat island and land use/cover changes. Remote Sensing of Environment. 104: 133-146"),
                                 p("Conversion to TOA Radiance. Using the USGS Landsat Level-1 Data Product. (n.d.). https://www.usgs.gov/core-science-systems/nli/landsat/using-usgs-landsat-level-1-data-product."),  
                                 p("Dawson, T., Sandoval, J. S., Sagan, V., & Crawford, T. (2018). A spatial analysis of the relationship between vegetation and poverty. ISPRS International Journal of Geo-Information, 7(3), 83. https://doi.org/10.3390/ijgi7030083"),
                                 p("El Bastawesy, M., Gabr, S., Mohamed, Ihab, 2015. Assessment of hydrological changes in the Nile River due to the construction of Renaissance Dam in Ethiopia. Egypt. J. Remote Sens. Space Sci. 18 (1), 65–75."),
                                 p("Fensholt, R.; Rasmussen, K.; Nielsen, T.T.; Mbow, C. Evaluation of earth observation based long term vegetation trends—Ntercomparing NDVI time series trend analysis consistency of Sahel from AVHRR GIMMS, Terra MODIS and SPOT VGT data. Remote Sens. Environ. 2009, 113, 1886–1898"),
                                 p("Gao, B.-C. NDWI—A normalized difference water index for remote sensing of vegetation liquid water from space. Remote Sens. Environ. 1996, 58, 257–266."),
                                 p("Gao, B.-cai. (1996). NDWI—A normalized Difference WATER index for remote sensing of VEGETATION liquid water from space. Remote Sensing of Environment, 58(3), 257–266. https://doi.org/10.1016/s0034-4257(96)00067-3"), 
                                 p("Gitelson, A.A.; Peng, Y.; Huemmrich, K.F. Relationship between fraction of radiation absorbed by photosynthesizing maize and soybean canopies and NDVI from remotely sensed data taken at close range and from MODIS 250m resolution data. Remote Sens. Environ. 2014, 147, 108–120."),
                                 p("Gu, Y. – Hunt, E. – Wardlow, B. – Basara, J.B. – Brown, J.F. - Verdin, J.P. (2008): Evaluation of MODIS NDVI and NDWI for vegetation drought monitoring using Oklahoma Mesonet soil moisture data, Geophysical Research Letters 35: L22401, doi:10.1029/2008GL035772."),
                                 p("Herrero, H., Waylen, P., Southworth, J., Khatami, R., Yang, D., & Child, B. (2020). A healthy Park NEEDS HEALTHY Vegetation: The story OF Gorongosa National Park in the 21st century. Remote Sensing, 12(3), 476. https://doi.org/10.3390/rs12030476"),  
                                 p("I.H.El-Gamily,G.Selim,E.A.Hermas, Wireless mobile field-based GIS science and technology for crisis management process: a case study of a fire event, Cairo, Egypt, Egypt. J. Remote Sens. Space Sci.,13(1)(2010), pp.21-29"), 
                                 p("Jackson, T.J.; Chen, D.; Cosh, M.; Li, F.; Anderson, M.; Walthall, C.; Doriaswamy, P.; Hunt, E.R. Vegetation water content mapping using Landsat data derived normalized difference water index for corn and soybeans. Remote Sens. Environ. 2004, 92, 475–482. "),
                                 p("Karnieli, A.; Agam, N.; Pinker, R.T.; Anderson, M.; Imhoff, M.L.; Gutman, G.G.; Panov, N.; Goldberg, A. Use of NDVI and land surface temperature for drought assessment: Merits and limitations. J. Clim. 2010, 23, 618–633."),
                                 p("Landsat surface REFLECTANCE-DERIVED SPECTRAL Indices. Landsat Normalized Difference Vegetation Index. (n.d.). https://www.usgs.gov/core-science-systems/nli/landsat/landsat-normalized-difference-vegetation-index?qt-science_support_page_related_con=0#qt-science_support_page_related_con. "),  
                                 p("Liu, W. – Lu, L. – Ye, C. – Liu, Y. (2009.): Relating urban surface temperature to surface characteristics in Beijing area of China. Proc. SPIE 7498, MIPPR 2009: Remote Sensing and GIS Data Processing and Other Applications, 74982I (30 October 2009); doi: 10.1117/12.833679"), 
                                 p("Ogashawara, I. – Bastos, V.S.B. (2012): A Quantitative Approach for Analyzing the Relationship between Urban Heat Islands and Land Cover. Remote Sensing. 4: 3596-3618. "), 
                                 p("Ouzemou, J.-E., El Harti, A., Lhissou, R., El Moujahid, A., Bouch, N., El Ouazzani, R., Bachaoui, E. M., & El Ghmari, A. (2018). Crop type mapping FROM pansharpened Landsat 8 NDVI data: A case of a highly fragmented and intensive agricultural system. Remote Sensing Applications: Society and Environment, 11, 94–103. https://doi.org/10.1016/j.rsase.2018.05.002"),
                                 p("Pettorelli, N. (2013). The normalized difference vegetation index. Oxford University Press. "),  
                                 p("Piragnolo, M.; Pirotti, F.; Guarnieri, A.; Vettore, A.; Salogni, G. Geo-spatial support for assessment of anthropic impact on biodiversity. Int. J. Geo-Inf. 2014, 3, 599–618. "),
                                 p("Pravalie, R., Sîrodoev, I., & Peptenatu, D. (2014). Detecting climate change effects on forest ecosystems in southwestern Romania USING Landsat TM Ndvi data. Journal of Geographical Sciences, 24(5), 815–832. https://doi.org/10.1007/s11442-014-1122-2"), 
                                 p("S.Lu,B.Wu,N.Yan,H.Wang, Water body mapping method with HJ-1A/B satellite imagery, Int. J. Appl. Earth Obs. Geoinf.,13(3)(2011), pp.428-434"), 
                                 p("S.K.McFeeters, The use of the normalized difference water index (NDWI) in the delineation of open water features, Int. J. Remote Sens.,17(7)(1996), pp.1425-1432"), 
                                 p("Sánchez-Ruiz, S.; Piles, M.; Sánchez, N.; Martínez-Fernández, J.; Vall-llossera, M.; Camps, A. Combining SMOS with visible and near/shortwave/thermal infrared satellite data for high resolution soil moisture estimates. J. Hidrol. 2014, 516, 273–283."),
                                 p("Serrano, J; Shahidian, S.; Marques da Silva, J. (2019) Evaluation of Normalized Difference Water Index as a Tool for Monitoring Pasture Seasonal and Inter-Annual Variability in a Mediterranean Agro-Silvo-Pastoral System. Water, 11, 62; doi:10.3390/w11010062"), 
                                 p("Su, H., Yang, D., & Yong, Y. (2015). MODIS-Landsat data fusion for Estimating Vegetation dynamics - a case study for Two ranches in SOUTHWESTERN TEXAS. Proceedings of 1st International Electronic Conference on Remote Sensing. https://doi.org/10.3390/ecrs-1-d016"), 
                                 p("Wang, X.; Fuller, D.O.; Setemberg, L.; Miralles-Wilhelm, F. Foliar nutrient and water content in subtropical tree islands: A new chemohydrodynamic link between satellite vegetation indices and foliar δ 15N values. Remote Sens. Environ. 2011, 3, 923–930."),
                                 p("Zhu, Y., Yang, K., Pan, E., Yin, X., & Zhao, J. (2018). Extraction and analysis of urban vegetation information based on remote sensing image. 2018 26th International Conference on Geoinformatics. https://doi.org/10.1109/geoinformatics.2018.8557075"))
                 ),
                 inverse = T)



# server -----------------------------------------------------------
server <- function(input, output, session) {
  # Run JavaScript Code
  runjs(jscode)
  
  
  #NDVI Predictions
  
  output$NDVIMap <- renderLeaflet({
    
    ## outline of Floyd
    floyd <- virginiaCounty[5,] %>% st_transform(crs = "+init=epsg:4326")
    m <- leaflet(options = leafletOptions(minzoom = 19))
    
    
    
    
    # geotiffFile = "./www/2021_NN_Predictions_2.tif"
    # 
    # my_file = raster(geotiffFile)
    # 
    # my_file[!(my_file > 0)] = NA
    
    pal = colorNumeric(
      palette = 'viridis',
      domain = c(0, 1)
    )
    
    m <- leaflet()
    
    m <- addGeoRaster(m, my_file_crop,
                      opacity = 0.55,
                      autozoom = FALSE,
                      colorOptions = colorOptions(
                        palette = hcl.colors(256, palette = "viridis")
                        , na.color = "transparent"
                      ))
    
    addPolygons(m, data = floyd,
                fillColor = "Transparent",
                weight = 4,
                opacity = 1,
                color = "white") %>%
      addLegend(pal = pal, values = c(0, 1), opacity = 0.7, title = "Predicted NDVI Value",
                position = "bottomright") %>%
      setView(m, lng = -80.301, lat = 36.91, zoom = 9.5) %>%
      addProviderTiles("CartoDB")
  })
  
  output$ndviPredictions <- renderImage({
    list(src = "www/FloydPredictions.JPG")
  }, deleteFile = FALSE)
  
  
  
  
  
}

shinyApp(ui = ui, server = server)