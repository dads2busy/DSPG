library(shiny)
library(dplyr)
library(ggplot2)
library(data.table)
library(rsconnect)


#to do 7/28
  #remove and rename column names in results tabb
  #get all the charts needed for this app
  #Try to add a chart and images to app
  #Begin filling out methods portion
  #Work on about section, detailing the project and the problem we are trying to solve





ui <- fluidPage(
  theme ="themes.css",

  navbarPage(title = span("Business Innovation", style = "color:#232D4B"),
             tabPanel("About",style = "margin:45px",
                      fluidRow(
                        column(3, tags$img(height = "80%", width = "80%", src = "biilogo.png")),
                        column(6, h1("Business Innovation")),
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
                      h5("DSPG20BI Summer Project"),
                      p("The DSPG20BI team is one of x number of teams within the larger DSPG program tasked with looking into detecting product innovation within non-traditional data sources.
                        Our goal is to find instances of product innovation within the pharmaceutical industry thorugh niche natural-language processessing techniques in an attempt
                        to supplement the current measure of innovation", tags$a(href = "https://www.nsf.gov/statistics/srvyindustry/about/brdis/", "the Business R&D and Innovation Survey (BRDIS)"),"conducted by The National Science Foundation."),

                      p("During the 10-week DSPG program, the Business Innovation team focused on developing functions that used NLP techniques to match strings across datasets.
                        These functions were written particularly focused on datasets mentioning innovation amongst pharmaceutical companies. However, the functions can be applied to any dataset containing strings.
                        The goal of this task it to provide future insights on the companies doing innovation as it pertains to the OLSO manual definition"),

                      h5("Our Team"),
                      p("SDAD: Devika Mahoney-Nair, Gizem Korkmaz, & Neil Alexander"),
                      p("DSPG: Susweta Ray (Fellow), Isabel Gomez (Intern), Ian MacLeod (Intern)"),
                      p("Sponsor: Gary Anderson, National Science Foundation (NSF), National Center for Science and Engineering (NCSES)")


                      ),

             #ui
             navbarMenu("Repositories",

             tabPanel("Dryad", style = "margin:20px",
                      h5("Background"),
                      br(),
                      br(),
                      br(),
                      p("Dryad is a community-driven, open-source multidisciplinary repository. Of the 41,000 datasets on the website, 
each contains the number of downloads and views per dataset, as well as the number of publications the dataset was cited in and a link to the data’s original study. 
For reference, there were an average of 41 downloads per dataset, and 107 views per dataset."),
                      br(),
                      p("Notably, dryad also provides data usage, or (re)usage notes: if a researcher wants 
to use data posted to dryad in their own study, he or she can read the usage notes provided with the data to understand what 
packages and software are needed to read the data, or what steps can be taken to best prepare the data for analysis. 
The graph below depicts the number of datasets published to dryad over time from our sample of 3400 datasets,
spanning from 2007 to 2021. We see that over time, there is an increase in data sharing, quantified by the number of datasets published.
"),
                    
                      h5("Visuals"),
                      br(),
                      br(),
                      br(),
                      img(src = "dryad_pubs_per_year.png", height = 600, width = 1000),

                      ),

             tabPanel("Profiles", style = "margin:60px",
                      h5("Profiling", align = "center"),
                      p(style = "margin-top:25px","Profiling is essential for ensuring the contents of datasets align with the projects overall goal and resources. The first goal of the Business Innovation project is to obtain a general understanding of what companies are the ones producing recent innovation. Therefore, we profiled the DNA data to include only unique, complete and valid entries.  We defined a valid entry, as an article that was published after 2010, had more than 100 but less than 10,000 characters and had a company code that was in the company codes dictionary. The year restriction will allow us to only consider recent innovations, the character restriction will allow our computing resources to fully analyze the text and the company code restriction will ensure that we have the full name of the company which will provide better insights on the companies completing innovation. " ),
                      br(),
                      p("Originally, the dataset contained 1,942,855 data entries. Given a restriction on memory and running power, we decided to only have unique and complete entries as it diminished the dataset by 96.2% to 73, 688 entries, while still fulfilling our main goal of understanding what companies are producing innovation. The visualization [above/below] demonstrates the total percentage of data entries that passed our validity checks.  About 78.3% of the total unique entries passed the validity check, 100% of the entries were published after 2010, and 91.7% of the entries contained valid company codes.  "),
                      sidebarLayout(
                        sidebarPanel(
                          width = 6,
                          selectInput("selectTable", "Select", choices = c("Completeness, Uniqueness, Duplicates", "Validity")),
                          h4("Definitions: ", style = "margin-top:50px"),
                          helpText("Note: All definitions are provided by Dow Jones Developer Platform"),
                          tags$ul(
                            tags$li("an - Accession Number (Unique id)"),
                            tags$li("art: Caption text and other descriptions of images and illustrations"),
                            tags$li("action: Action perfomed on a document (ex. add, rep, del)"),
                            tags$li("body: The content of the article"),
                            tags$li("byline: The author of an article"),
                            tags$li("copyright: Copyright text"),
                            tags$li("credit: Attribution text"),
                            tags$li("currency_codes: Currencies"),
                            tags$li("dateline: Place of origin and date"),
                            tags$li("document_type: Document type (ex. article, multimedia, summary"),
                            tags$li("ingestion_datetime: Data and time the artile was added to the Dow Jones Developer Platfrom"),
                            tags$li("language_code: Code for teh published language (ex. en)"),
                            tags$li("modification_datetime: Data and time that the article was modified"),
                            tags$li("modification_date: Date in which the article was last modified"),
                            tags$li("publication_date: Date in which the article was published"),
                            tags$li("publication_datetime: Date and time in which the article was published"),
                            tags$li("publisher_name: Publisher name"),
                            tags$li("region_of_origin: Publisher's region of origin"),
                            tags$li("snippet: What you see of an article outiside the paywall"),
                            tags$li("source_code: Publisher code"),
                            tags$li("source_name: Name of the source"),
                            tags$li("title: Title text"),
                            tags$li("word_count: Document word count"),
                            tags$li("subject_codes: News subjects"),
                            tags$li("region_codes: Region codes (ex. usa, namz, etc"),
                            tags$li("industry_codes: Industry codes"),
                            tags$li("person_codes: Persons"),
                            tags$li("market_index_codes: Market indices"),
                            tags$li("company_codes: Factiva IDs for companies and organizations"),
                            tags$li("company_codes_about: Companies that have high relevance to the document"),
                            tags$li("company_codes_association: Companies added to the document because of a relationship other than parent/child"),
                            tags$li("company_codes_lineage: Companies added to the document because of a parent/child relationship to another company"),
                            tags$li("company_codes_occur: Companies mentioned in the document but that do not necessarily have a high relevance to it"),
                            tags$li("company_codes_relevance: Companies added to the document because they have a certain degree of relevance to it")

                          )

                        ),
                        mainPanel(width = 3, tableOutput("tables"))
                      ))
),
             #end profiling tab------------------------------------------


             navbarMenu("Data Sources and Methods",
                        tabPanel(
                          "Data Sources",
                          h3("Data Sources", align = "center", style = "margin-bottom: 50px"),
                          style = "margin-left: 120px;",
                          style = "margin-top: 30px;",
                          style = "margin-right: 120px;",
                          fluidRow(
                            column(3, tags$img(height = "100%", width = "100%",src = "dnalogo.png")),
                            column(6, wellPanel(p(style = "font-size:15px","The Dow Jones DNA platform collects information from Dow Jones publication with premium and licensed third party sources. This proprietary data platform contains 1.3bn articles each labeled with unique DNA taxonomies tags including word count, source name, and company code. More information on all the included data tags can be found on the DNA website. This dataset served as the primary resource for alternative text sources and will inspire the machine learning algorithms that will predict innovation. "))),
                            ),
                          hr(),
                          fluidRow(style = "margin-top:100px",
                                   column(3, tags$img(height = "100%", width = "100%", src = "fdalogo.png")),
                                   column(7, wellPanel(
                                     tags$b("Approvals"),
                                     p(style = "font-size:15px", "FDA drug approvals dataset generated and reviewed by FDA and includes information regarding. ",
                                     br(),
                                     br(),
                                     tags$b("National Drug Code"),
                                     p(style = "font-size:15px", "The National Drug Code (NDC) Directory is a publicly available source provided by the FDA that contains a list of all current drugs manufactured, prepared, propagated, compounded, or processed for commercial distribution. The data content is manually inputted by the companies producing the drugs as required per the Drug Listing Act of 1972. The FDA assigns the NDC – a unique three-digit number, to the drug products. The administration then updates the NDC directory daily with the NDC along with the rest of the information provided. We gathered content from this dataset on [enter date here]. This data was used to cross-validate the companies that we had previously identified as producing an innovation. ")
                                    )))
                          ),

                        ),


                        tabPanel("Methods",
                                 h3("Methods", align = "center", style = "margin-bottom: 50px"),
                                 style = "margin-left: 120px;",
                                 style = "margin-top: 30px;",
                                 style = "margin-right: 120px;",

                                   fluidRow(
                                     column(3, h4("Cleaning")),
                                     column(6, wellPanel(p(style = "font-size:15px","In order to match company names across all three dataset we had to make all the strings similar to each other to facilitate fuzzy matching. To accomplish this we used regular expressions, the string package and pandas' package. The first step in the process was to lowercase all the strings. Then, remove punctuations except for underscores, dashes, ampersands, percent and dollar symbols. Afterwards, we removed any parenthesis along with the content within the parentheses. We then removed single characters from the beginning and removed numbers, as numbers complicate the matching process. Additionally, we removed extra spaces between words, the prefix b, and any legal entities from the company name. This provided all three data sets to have similar words in the entries that would make matching companies that may have ")))
                                   ),
                                   hr(),
                                   fluidRow(style = "margin-top:100px",
                                            column(3, h4("Fuzzy Matching")),
                                            column(6, wellPanel(p(style = "font-size:15px","To complete the fuzzy matching, we used a package by SeatGeeks called FuzzyWuzzy. FuzzyWuzzy uses the Levenshtein distance to calculate the minimum number of single character edits (insertions, deletions or substitutions) needed to change one word to another. The package contains several functions that produces a similarity ratio out of 100. The fuzz.ratio function calculates the ratio by using the basic Levenshtein distance and the equation from diff.libratio: 2*M / T, where T is the total number of characters in both strings and M is the number of matches. The fuzz.partial_ratio compares the shortest string (n) against all the n-length substrings of the larger string and returns the highest fuzz partial ratio. Therefore, if the shortest string is found within the larger string then the partial ratio will return a ratio of 100. For our purposes, we focused on a fuzz.ratio that would only produce 100 or perfect matches.  ")))
                                   ),
                                   hr(),
                                   fluidRow(style = "margin-top:100px",
                                            column(3, h4("Network Analysis")),
                                            column(7, h4("")))




                        )),#end navbar

             #end Data Sources and Methods tabs-----------------


             navbarMenu("Results",
                        tabPanel("Within Data Matching",
                                 selectInput("within", "Select", choice = c("NDCxNDC", "FDAxFDA", "DNAxDNA")),
                                 dataTableOutput("withinData")
                        ),


                        tabPanel("Across Data Matching",
                                 selectInput("across", "Select", choices = c("FDAxNDC", "FDAxDNA", "DNAxNDC")),
                                 dataTableOutput("AcrossData")



                        ),
                        tabPanel("Network Analysis")

                        )#end results tab


      ) #end navbarPage
  )#end fluid page





server <- function(input, output) {

  output$pub <- renderImage({

    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path('www',
                                        paste(input$year, 'Publisherplot.png', sep='')))

  }, deleteFile = FALSE)






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

  #output$fda <- renderDataTable({
    #x <- read.csv("fdaxfda.csv")
    #x
  #})

  #output$ndc <- renderDataTable({
    #ndc <- read.csv("ndcxndc.csv")
    #ndc
  #})

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
    if(input$across == "FDAxNDC"){
      acrossTable <- read.csv("fdaxndc.csv")

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

}

# Run the application
shinyApp(ui = ui, server = server)




