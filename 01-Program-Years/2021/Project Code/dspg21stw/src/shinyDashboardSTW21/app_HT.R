library(shiny)
library(dplyr)
library(ggplot2)
library(data.table)
library(rsconnect)
library(shiny)
library(forcats)
library(plotly)

merged_cert_fam <- read.csv("merged_cert_fam.csv" ) 


ui <- fluidPage(
  theme ="themes.css",
  
  navbarPage(title = span("Skilled Technical Workforce", style = "color:#232D4B"),
             tabPanel("About",style = "margin:45px",
                      fluidRow(
                        #column(3, tags$img(height = "80%", width = "80%", src = "biilogo.png")),
                        column(6, h1("Skilled Technical Workforce (STW)")),
                        column(3, tags$img(height = "80%", width = "80%", src = "partnerlogos.png", align = "right"))
                      ),
                      
                      h5("SDAD/DSPG"),
                      p("The Social and Decision Analytics Division (SDAD) is one of three research divisions within the Biocomplexity Institute and Initiative at the University of Virginia.
                        SDAD combines expertise in statistics and social and behavioral sciences to develop evidence-based research
                        and quantitative methods to inform policy decision-making and evaluation.
                        The researchers at SDAD span many disciplines including statistics, economics, sociology, psychology,
                        political science, policy, health IT, public health, program evaluation, and data science.
                        The SDAD office is located near our nation's capital in Arlington, VA. You can learn more about us at",
                        tags$a(href="https://biocomplexity.virginia.edu/social-decision-analytics.", "https://biocomplexity.virginia.edu/social-decision-analytics."), style = "color:#232D4B"),
                      p("The Data Science for the Public Good (DSPG) Young Scholars program is a summer immersive program held at SDAD. Entering its seventh year, the program engages students from across the country
                        to work together on projects that address state, federal, and local government challenges around critical social issues relevant in the world today.
                        DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how information
                        generated within every community can be leveraged to improve quality of life and inform public policy. ", style = "color:#232D4B"),
                      h5("DSPG21STW Summer Project"),
                      p("Employment in the Skilled Technical Workforce (STW) requires a high level of knowledge in a technical domain and does not require a bachelor’s degree for entry.  These jobs are important because they provide a path to the middle class, and they foster US innovation and shared prosperity through having a workforce with a diverse technical skillset.  By 2022, the US will have 2.4 million unfilled STW jobs.  This summer, our goal was to create a list of nondegree credentials for jobs in the STW."),
                      
                      p("During the 10-week DSPG program, the STW team documented data sources to create a normalized dataset for STW jobs.  We used text matching to compare this data with job ads data and analyzed credential portability through network analysis."),
                      
                      h5("Our Team"),
                      
                      
                      p("SDAD: Vicki Lancaster and Cesar Montalvo"),
                      p("DSPG: Emily Kurtz (Fellow), Haleigh Tomlin (Intern), Madeline Garrett (Intern)"),
                      p("Sponsor: Gigi Jones, National Science Foundation (NSF), National Center for Science and Engineering (NCSES)")
                      
                      
             ),
             
             #ui
             navbarMenu("Profiling",
                        
                        tabPanel("Certifications and Occupations", style = "margin:20px",
                                 h5("Visuals"),
                                 p(style = "margin-top:25px","In this graph we show the top burning glass certifications for each SOC Occupation Family. We removed drivers license as a credential. Blah blah blah, will have to fill this in." ),
                                 br(),
                                 br(),
                                 br(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     h4("Top Certifications for each Occupation Family"),
                                     selectInput("family", "Occupation Family", choices = list(
                                       'Management' = '11' ,
                                       'Business and Financial Operations' = '13' ,
                                       'Architecture and Engineering' = '17' ,
                                       'Life, Physical, and Social Science' = "19" ,
                                       'Arts, Design, Entertainment, Sports, and Media' = "27" ,
                                       'Healthcare Practitioners and Technical' = "29" ,
                                       'Protective Service' = "33" ,
                                       'Food Preparation and Serving Related' = "35" ,
                                       'Office and Administrative Support' = "43" ,
                                       'Farming, Fishing, and Forestry' = "45" ,
                                       'Construction and Extraction' = "47" ,
                                       'Installation, Maintenance, and Repair' = "49" ,
                                       'Production' = "51" ,
                                       'Transportation and Material Moving' = "53"
                                     ))),
                                   mainPanel(
                                     plotlyOutput('plot_20top_certificates' , width = "auto" ,  height = "700"))
                                 )
                                 
                        ),
                        
                        tabPanel("Profiles", style = "margin:60px",
                                 h5("Profiling", align = "center"),
                                 p(style = "margin-top:25px","Our first task was to profile the Burning Glass Technologies data in order to get a better understanding of the data we would be working with. We looked primarily at metrics like data completeness, uniqueness, and interesting metrics for columns SOC code and Certification. Completeness is simply a percentage of how complete the data was. Uniqueness can be defined as the number of distinct entries for each of the variables.In this Data Discovery we are looking at 3 data sets, BGT_Main, BGT_CERT, and STW.BGT_MAIN is a data set of 33,859,698 rows, we pulled specifically for the variables" ),
                                 br(),
                                 p("Before we evaluate completeness and uniqueness of variables, we had to do some manipulation to the data set. In order to successfully filter for STW jobs down the line we needed to remove job-ads from BGT_MAIN that did not have a soc code. This decreased the size of the original data set from 33,859,698 to 32,488,447, removing 1,371,251 rows. Next, we had to remove “-” from SOC variable and make SOC codes integers in both the BGT_MAIN and STW data sets. We were then able to merge the data by matching on the SOC variable, so that the data only contained job-ads that were in the STW. This decreased our data set down to 2,234,115 rows. This shows that 6.9% of the bgt job ads with soc codes are in the STW. Next, we needed add in certification data. We merged BGT_MAIN and BGT_CERT on the jobid variable. This decreased our data set to 1,173,772, indicating that out of the job ads within STW 52.5% have credentials listed."),
                                 sidebarLayout(
                                   sidebarPanel(
                                     width = 6,
                                     selectInput("selectTable", "Select", choices = c("Completeness, Uniqueness, Duplicates", "Validity")),
                                     h4("Definitions: ", style = "margin-top:50px"),
                                     helpText("Note: All definitions are provided by Dow Jones Developer Platform"),
                                     tags$ul(
                                       tags$li("soc code: SOC code of the job assigned using BGT occupation coding rules. SOC codes are always the first 6- digits of a job’s O*NET code. We use SOCs based off of the most recent 2010 SOC delineations."),
                                       tags$li("Latitude: Latitude for the Canonicalized Location."),
                                       tags$li("Longitude: Longitude for the Canonicalized Location."),
                                       tags$li("Fipsstate: FIPS is a 5-digit code, representing the concatenation of the state + county FIPS codes. Ex. 29019 is the FIPS code for Boone County, MO, where the first 2-digits, 29, represent MO, and the last 3-digits, 019, represent Boone County in MO."),
                                       tags$li("Jobid: A unique ID generated by Burning Glass. BGTJobId is used to link this table with both the main table, and subsequent tables. "),
                                       tags$li("Certification: Certification is a standardized version of the certifications listed in the posting to enable improved search and categorization.Note that there may be multiple certifications per job posting (or, alternatively, none at all). As such, the data in this table is presented vertically, where each certification for a particular job posting will be in its own row, with the job posting’s BGTJobId in its own column to the left of each certification, identifying which job posting each certification came from. "),
                                       tags$li("Name: Name of the occupation associated with a given SOC Code ")
                                     )
                                     
                                   ),
                                   mainPanel(width = 3, tableOutput("tables"))
                                 ))
             ),
             #end profiling tab------------------------------------------
             
             
             
             
             
             tabPanel("Methods",
                      h3("Methods", align = "center", style = "margin-bottom: 50px"),
                      style = "margin-left: 120px;",
                      style = "margin-top: 30px;",
                      style = "margin-right: 120px;",
                      
                      
                      fluidRow(style = "margin-top:100px",
                               column(3, h4("Web Scraping")),
                               column(6, wellPanel(p(style = "font-size:15px","To collect the credential and SOC data from O*NET, we used R’s rvest web scraping package. We looped through the list of 133 Skilled Technical Workforce SOC codes and found all associated credentials for each. O*NET also reports the certifying organization and the type of credential, so we collected that information as well. In the end, we created a data frame with 1137 rows and 4 columns.  ")))
                      ),
                      hr(),
                      fluidRow(style = "margin-top:100px",
                               column(3, h4("Cleaning")),
                               column(6, wellPanel(p(style = "font-size:15px","To match credentials across the O*NET, BGT, and VA Community Colleges data sources, we had to perform some preliminary data cleaning. We used regular expressions to remove acronyms within parentheses in the BGT dataset and states in the Community Colleges dataset. We also removed stop words and stemmed the words in the datasets, removing common, superfluous suffixes. Once the credentials were cleaned within all three datasets, we were ready to perform our text matching.")))
                      ),
                      hr(),
                      fluidRow(style = "margin-top:100px",
                               column(3, h4("Text Matching")),
                               column(6, wellPanel(p(style = "font-size:15px","Filling crucial jobs in the Skilled Technical Workforce requires coordination between governments, industries, and educational institutions. Unfortunately, there are inconsistencies in the language these three bodies use to describe the credentials required for these jobs. We used text matching methods to address some of these inconsistencies. To do this, we used the stringdist and lingmatch R packages. Specifically, we used the stringdist function within the stringdist package to calculate the Damerau-Levenshtein distance between each pair of credentials across our three datasets. The lingmatch package allowed us to easily create matrices where words were represented by columns and credentials were represented by rows. This aided the distance calculations across each pair of data sources.  ")))
                      ),
                      hr(),
                      fluidRow(style = "margin-top:100px",
                               column(3, h4("Network Analysis")),
                               column(6, wellPanel(p(style = "font-size:15px","We used the R package igraph in order to visualize the links between occupations and credentials within the Skilled Technical Workforce. By visualizing these links and combining supplemental information, such as the projected growth of specific occupations in the STW, we are able to analyze the paths workers can take in the STW.  ")))
                      )
                      
                      
                      
                      
             ),#),#end navbar
             
             #end Data Sources and Methods tabs-----------------
             
             
             navbarMenu("Results",
                        tabPanel("Across Data Matching",
                                 selectInput("across", "Select", choices = c("ONETxBGT", "ONETxVA", "VAxBGT")),
                                 dataTableOutput("AcrossData")
                                 
                                 
                                 
                        ),
                        
                        tabPanel("Network Analysis - Occupation Clusters", style = "margin:20px",
                                 h5("Occupation Clusters"),
                                 br(),
                                 br(),
                                 br(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     h4("Occupation"),
                                     selectInput("network", "Occupation", choices = c("Health Sciences", "Cybersecurity", "Manufacturing")),
                                     p("Explanation Here.  Perhaps depending on Choice, a different explanation comes up.")),
                                   mainPanel(
                                     imageOutput("netgraph")))),
#<<<<<<< HEAD
                        tabPanel("Network Analysis - STW by Major Occupation Group", style = "margin:20px",
                                 h5("STW by Major Occupation Group"),
                                 br(),
                                 br(),
                                 br(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     h4("Occupation Group"),
                                     selectInput("occ_group", "Occupation", choices = c("Entire Network",
                                                                                        "Architecture and Engineering Occupations",
                                                                                        "Art, Design, Entertainment, Sports, and Media Occupations",
                                                                                        "Healthcare Practitioners and Technical Occupations",
                                                                                        "Construction and Extraction Occupations",
                                                                                        "Installation, Maintenance, and Repair Occupations",
                                                                                        "Production Occupations",
                                                                                        "Transportation and Material Moving Occupations")),
                                     p("Explanation here.  Perhaps depending on Choice, a different explanation comes up.")),
                                   mainPanel(
                                     imageOutput("occ_graph")))),#end results tab 
#=======
                        
                        tabPanel("Boxplots",style="margin:20px",
                                 h5("Boxplots", align = "center"),
                                 p(style = "margin-top:25px","To visualize the relationships between the named credentials within the three main datasets, we created boxplots. One natural question to ask is whether there are more precise matches in how credentials are referenced within certain types of jobs. Within the Skilled Technical Workforce, there are 14 occupational groups. These are: 1) 11. Agriculture, Forestry, Fishing, and Hunting, 2) 13. 3) 17. 4) 19. 5) 27. 6) 29. 7) 33. 8) 35. 9) 43. 10) 45. 11) 47. 12) 49. 13) 51. Information, and 14) 53. Real Estate and Rental and Leasing. The boxplot below shows the confidences for our text matching algorithm between Burning Glass credentials and O*NET credentials for each of these occupational groups. While some groups' confidences are somewhat higher than others, all results are somewhat low. This highlights the gaps between industries, governments, and educational institutions, and shows these gaps manifest even in the language used to discuss occupations and credentials. " ),
                                 #br(),
                                 #p("Originally, the dataset contained 1,942,855 data entries. Given a restriction on memory and running power, we decided to only have unique and complete entries as it diminished the dataset by 96.2% to 73, 688 entries, while still fulfilling our main goal of understanding what companies are producing innovation. The visualization [above/below] demonstrates the total percentage of data entries that passed our validity checks.  About 78.3% of the total unique entries passed the validity check, 100% of the entries were published after 2010, and 91.7% of the entries contained valid company codes.  "),
                                 imageOutput("bgtonetbox")
                                 
                                 
                                 )#end results tab 
#>>>>>>> 3814624d0ea896e9d7d428b327547af7d7df317d
             ), #end navbarPage
             navbarMenu("Data Sources",
             tabPanel("Data Sources",
                      h3("Data Sources", align = "center", style = "margin-bottom: 50px"),
                      style = "margin-left: 120px;",
                      style = "margin-top: 30px;",
                      style = "margin-right: 120px;",
                      fluidRow(
                        column(3, tags$img(height = "100%", width = "100%",src = "dnalogo.png")),
                        column(6, wellPanel(p(style = "font-size:15px","The Burning Glass Technologies delivers job market analytics that empower employers, workers and educators to make data-driven decisions. The company’s artificial intelligence technology analyzes hundreds of millions of job postings and real-life career transitions to provide insight into workforce demand patterns. This real-time strategic intelligence offers crucial insights, such as which jobs are most in demand, the specific skills employers need and the career directions that offer the highest potential for workers. For more information, visit burning-glass.com. "))),
                      hr(),
                      fluidRow(style = "margin-top:100px",
                               column(3, tags$img(height = "100%", width = "100%", src = "fdalogo.png")),
                               column(7, wellPanel(
                                   br(),
                                   br(),
                                   tags$b("ONET"),
                                   p(style = "font-size:15px", "ONET Data was webscraped from the Occupational Information Network. O*NET Data descriptors are categories of occupational information collected and available for O*NET-SOC occupations. Each descriptor contains more specific elements with data ratings.")
                                 )))
                      )
                      
             )))) #end fluid page





server <- function(input, output) {
  
  output$plot_20top_certificates <- renderPlotly({
    
    # When input$n is 3, filename is ./images/image3.jpeg
    dat <- merged_cert_fam %>% filter(fam==input$family, 
                                      certification != "Driver's License",
                                      certification != "CDL Class A",
                                      certification != "Cdl Class B",
                                      certification != "Cdl Class C",
                                      certification != "CDL Class D",
                                      certification != "Security Clearance"
    ) %>%
      arrange(desc(number)) %>%
      slice(1:30)
    
    # Return a list containing the filename and alt text
    plot_cert <- ggplot(data = dat, mapping = aes(x = reorder( factor(name_cert), number), number)) + 
      geom_bar(stat = "identity") + coord_flip()+
      labs(x='', y='Number',
           #title = 'Top 20 Certifications for   Occupation')
           title = paste('Top 30 Certifications for', 'Occupation') )
    
    
    plot_cert
    
    
    
  })

  output$tables <- renderTable({
    if(input$selectTable == "Validity"){
      
      
      valid <- read.csv("validitytable.csv")
      names(valid)[names(valid) == "X"] <- "Column Name"
      
      valid
    }else{
      profTable <- read.csv("profilingTable.csv")
      
      names(profTable)[names(profTable) == "X"] <- "Column Name"
      
      profTable
    }
    
  })

  
  output$withinData <- renderDataTable({
    if(input$within == "FDAxFDA"){
      withinTable <- read.csv("fdaxfda.csv")
      
      withinTable$X <- NULL
      withinTable$fuzz.ratio <- NULL
      withinTable$original.row.number <- NULL
      
      names(withinTable)[names(withinTable) == "clean.company.name"] <- "Corporate Family"
      names(withinTable)[names(withinTable) == "company.matches"] <- "Matches"
      names(withinTable)[names(withinTable) == "original.company.names"] <- "Original Company Name"
      
      
      
      
      withinTable
      
    }else if(input$within == "NDCxNDC"){
      withinTable <- read.csv("ndcxndc.csv")
      
      withinTable$X <- NULL
      withinTable$fuzz.ratio <- NULL
      withinTable$original.row.number <- NULL
      
      names(withinTable)[names(withinTable) == "clean.company.name"] <- "Corporate Family"
      names(withinTable)[names(withinTable) == "company.matches"] <- "Matches"
      names(withinTable)[names(withinTable) == "original.company.names"] <- "Original Company Name"
      
      withinTable
    }
  })
  
  output$AcrossData <- renderDataTable({
    if(input$across == "ONETxBGT"){
      acrossTable <- read.csv("onetxbg.csv")
      
      acrossTable$X <- NULL
      acrossTable$fda.row <- NULL
      acrossTable$clean.fda.company.name <- NULL
      acrossTable$clean.ndc.row <- NULL
      acrossTable$fuzz.ratio <- NULL
      acrossTable$clean.ndc.company <- NULL
      
      names(acrossTable)[names(acrossTable) == "original.fda.company"] <- "Original FDA Company"
      names(acrossTable)[names(acrossTable) == "corporate.family"] <- "Corporate Family"
      names(acrossTable)[names(acrossTable) == "original.ndc.company"] <- "Original NDC Company"
      
      acrossTable
    }else if(input$across == "FDAxDNA"){
      acrossTable <- read.csv("fda_dna_matching.csv")
      
      acrossTable$fda.row <- NULL
      acrossTable$clean.fda.company.name <- NULL
      acrossTable$clean.dna.row <- NULL
      acrossTable$fuzz.ratio <- NULL
      acrossTable$clean.dna.company <- NULL
      
      acrossTable$X <- NULL
      
      names(acrossTable)[names(acrossTable) == "original.fda.company"] <- "Original FDA Company"
      names(acrossTable)[names(acrossTable) == "corporate.family"] <- "Corporate Family"
      names(acrossTable)[names(acrossTable) == "original.dna.company"] <- "Original DNA Company"
      acrossTable
    }else{
      acrossTable <- read.csv("ndc_dna_matching.csv")
      
      acrossTable$X <- NULL
      acrossTable$NDC.row <- NULL
      acrossTable$clean.NDC.company <- NULL
      acrossTable$clean.DNA.row <- NULL
      acrossTable$fuzz.ratio <- NULL
      acrossTable$clean.DNA.company <- NULL
      
      names(acrossTable)[names(acrossTable) == "original.NDC.company"] <- "Original NDC Company"
      names(acrossTable)[names(acrossTable) == "corporate.family"] <- "Corporate Family"
      names(acrossTable)[names(acrossTable) == "original.DNA.company"] <- "Original DNA Company"
      
      acrossTable
    }
  })
  
  output$netgraph <- renderImage({
    
    # When input$network is nursing, filename is ./www/nursing.png
    filename <- normalizePath(file.path('www',
                                        paste(input$network, '.png', sep='')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste(input$network, "Network"),
         width = 900,
         height = 900)
    
    
    
  }, deleteFile = FALSE)
  
  
#<<<<<<< HEAD
  #The two below may need to be in an if/else statement to work with the 2 dropdowns.
  output$occ_group <- renderImage({
    
    # When input$network is nursing, filename is ./www/nursing.png
    filename <- normalizePath(file.path('www',
                                        paste(input$occ_group, '.png', sep='')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste(input$occ_group, "Network"),
         width = 900,
         height = 900)})
#=======
  output$bgtonetbox <- renderImage({
    
    # When input$network is nursing, filename is ./www/nursing.png
    filename <- normalizePath(file.path('www',
                                        'BGT_ONET_Box.png', sep=''))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste(input$bgtonetbox, "Boxplots"),
         width = 900,
         height = 600)
#>>>>>>> 3814624d0ea896e9d7d428b327547af7d7df317d
    
    
    
  }, deleteFile = FALSE)
  
#<<<<<<< HEAD
  output$occ_graph <- renderImage({
    
    # When input$network is nursing, filename is ./www/nursing.png
    filename <- normalizePath(file.path('www',
                                        paste(input$occ_group, '.png', sep='')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste(input$occ_graph, "Network"),
         width = 900,
         height = 900)
    
    
    
  }, deleteFile = FALSE)
#=======
  
#>>>>>>> 3814624d0ea896e9d7d428b327547af7d7df317d
}

# Run the application
shinyApp(ui = ui, server = server)




