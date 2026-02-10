library(shiny)
library(dplyr)
library(ggplot2)
library(data.table)
library(rsconnect)




ui <- fluidPage(
  theme ="themes.css",

  navbarPage(title = HTML("<img src='./DSPG_black-01.png' width='120px' style='margin-top:-10px;'/>"),
             tabPanel("About",style = "margin:45px",
                      fluidRow(
                        column(3, tags$img(height = "80%", width = "80%", src = "biilogo.png")),
                        column(6, h1("Data Reusability")),
                        column(3, tags$img(height = "80%", width = "80%", src = "Inkedpartnerlogo.jpg", align = "right"))
                      ),

                      h5("SDAD/DSPG"),
                      p("The Social and Decision Analytics Division (SDAD) is one of three research divisions within the Biocomplexity Institute and Initiative at the University of Virginia. SDAD combines expertise in statistics and social and behavioral sciences to develop evidence-based research and quantitative methods to inform policy decision-making and evaluation. The researchers at SDAD span many disciplines including statistics, economics, sociology, psychology, political science, policy, health IT, public health, program evaluation, and data science. The SDAD office is located near our nation's capital in Arlington, VA. You can learn more about us",
                        tags$a(href="https://biocomplexity.virginia.edu/social-decision-analytics.", "here."), style = "color:#232D4B"),
                      p(" "),
                      
                      p("The Data Science for the Public Good (DSPG) Young Scholars program is a summer immersive program held at SDAD. Entering its eighth year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how information generated within every community can be leveraged to improve quality of life and inform public policy. ", style = "color:#232D4B"),
                      
                      h5("Data Reusability"),
                      p("Data reusability refers to the ease with which data collected by some research communities (known as producer communities) for one purpose can be used by other research communities (known as consumer communities) to study a new problem. As data reusability becomes a distinct characteristic of modern scientific practice, organizations begin to mandate public accessibility to research data and push towards transparent research practices.  However, disciplines vary widely in their readiness to adopt these new practices, and research institutions face the daunting prospect of determining how to encourage better research practices for researchers from all disciplines. While best practices in sharing data are now centered on the FAIR principles (findable, accessible, interoperable, reusable), limited attention has been paid to what actually makes a data source reusable by another researcher. We spent our summer studying this question."),

                      strong(p("In this project, we:")),
                      em(p("1. Develop and pilot test a framework that articulates the concept of the “reusability” of a data source from the perspective of a user and is extensible across scholarly disciplines;")),
                      em(p("2. Identify practices for planning and conducting a research study that will increase the reusability of the data shared from the investigation, as well as reduce the burden in creating and appropriately using the data source; and")),
                      em(p("3. Propose a path forward for accelerating community readiness and the success of researchers in effectively producing a publicly accessible data product that readily enables a new user to evaluate and appropriately analyze the data source.")),
                      p("By better understanding the reusability of a shared research data source, researchers from a range of disciplines, especially those without a data sharing tradition, will be able to improve their planning and execution in producing research data intended for public access, thereby increasing the rigor of research studies and shared data products. By providing a pathway forward for increasing our capacity to produce more reusable data sources, this work provides information desperately needed by research institutions and other organizations to accelerate community readiness for developing and appropriately sharing high quality reusable research data sources."),

                      h5("Our Team"),
                      
                      p("SDAD: Alyssa Mikytuck, Gizem Korkmaz"),
                      p("DSPG: Emily Kurtz (Fellow), Akilesh Ramakrishna (Intern), Aditi Mahabal (Intern)"),
                      p("Sponsor: Martin Halbert, National Science Foundation (NSF)"),

                      h5("ISU Team"),
                      p("Collaborator: Sarah Nusser"),
                      p("Faculty: Adisak Sukul"),
                      p("DSPG: Tiancheng Zhou (Fellow), Sonyta Ung (Intern), Jack Studier (Intern), Saul Varshavsky (Intern)")
                      

                      ),

             




             #end profiling tab------------------------------------------


             #navbarMenu("Data Sources and Methods",
                        


             tabPanel("The Process",style = "margin:45px",
                      fluidRow(
                        column(3, tags$img(height = "80%", width = "80%", src = "biilogo.png")),
                        column(6, h1("The Process")),
                        column(3, tags$img(height = "80%", width = "80%", src = "Inkedpartnerlogo.jpg", align = "right"))
                      ),

                                   fluidRow(style = "margin-top:100px",
                                     column(3, h5("1. Repository Profiling and Selections")),
                                     column(6, wellPanel(p(style = "font-size:18px","The first step in our process this summer was to identify the data we wanted to collect and the repositories from which to collect the data. This goal was achieved in two main steps. First, we, along with the ISU team, profiled a total of 205 publicly accessible data repositories. When doing this, we recorded characteristics of the repositories, metrics they tracked, their accessibility, their usability for our research purposes, their size, and more. Characteristics include things like which fields the repositories focused on or whether they had integrated tools for facilitating reuse. Metrics they tracked included more obvious metrics like downloads and more unique metrics like altmetrics, which track a piece of research's presence on online platforms. A main consideration when evaluating accessibility was whether the repository required registration or an account for a user to reuse the datasets. To evaluate the repositories' usability for our purposes, we made a note of the APIs they offered and how they could help our research. Finally, we prioritized larger repositories, as measured by the number of datasets or other projects, such as articles or book chapters, they contained. After considering all of these factors, we chose and ultimately analyzed 5 repositories: NSF PAR, Figshare, Dryad, KNB, and ICPSR.")))
                                   ),
                                   hr(),
                                   fluidRow(style = "margin-top:100px",
                                            column(3, h5("2. Literature Review")),
                                            column(6, wellPanel(p(style = "font-size:18px","The second step for identifying the information we wanted to collect was to compare our repository profiling results to recommendations in the literature. We based our literature review on three main, recent articles: Fecher et. al. (2015), Koesten et. al. (2020), and Thanos (2015). We compiled a list of specific recommendations and common themes. This process highlighted the roles technology, policy, and culture can play in fostering data reuse. While the focus of our research this summer was on the technology side, this information guided the direction of our researh and supplemented our analyses and interpretations of our findings.  ")))
                                   ),
                                   hr(),
                                   fluidRow(style = "margin-top:100px",
                                            column(3, h5("3. Web Scraping")),
                                            column(6, wellPanel(p(style = "font-size:18px","Before we could do any quantitative analyses, we needed data. We used the R packages rvest and RSelenium, along with the sites' APIs, to collect our samples. The functions contained within rvest ultimately allowed us to extract most of the information we needed from our sites. However, in most cases we needed to use RSelenium in order to allow the websites to fully load before scraping them. This is because the sites initially loaded empty shells, so scraping immediately with rvest returned null results. These two R packages, rvest and RSelenium, allowed us to scrape one site at a time, so we built our code into for loops to scrape thousands of data sets. To get our desired datasets and websites, we used the repositories' APIs.   ")))
                                   ),
                                   hr(),
                                   fluidRow(style = "margin-top:100px",
                                            column(3, h5("4. Analyses")),
                                            column(6, wellPanel(p(style = "font-size:18px","Since each repository tracked some unique information, analyses by repository varied to some extent. More detailed information can be found in the Results section. However, for almost all repositories, downloads, citations, and views were tracked. We started our analyses by compiling descriptive statistics on these three important metrics of reuse. We also calculated correlations for these three metrics for each repository. After that point, analyses diverged. We used the other information that the repositories tracked, such as metadata analysis reports, file sizes, and numbers of keywords, to build models to predict downloads, citations, and views. We also did more qualitative analyses on the repositories as a whole. Finally, when possible, we graphed the dates of datasets being uploaded to see how the culture of data sharing has changed over time. These are just a few of the total analyses we performed.   "))))




                        ),#),#end navbar

             #end Data Sources and Methods tabs-----------------

             tabPanel("Profiling and Choosing Repositories",style = "margin:45px",
                      fluidRow(
                        column(3, tags$img(height = "80%", width = "80%", src = "biilogo.png")),
                        column(6, h1("Profiling and Choosing Repositories")),
                        column(3, tags$img(height = "80%", width = "80%", src = "Inkedpartnerlogo.jpg", align = "right"))
                      ),
                      
                      p(" "),
                      
                      p("One of our project’s preliminary steps in the Data Discovery process was finding repositories in which Publicly Accessible Research Data (PARD) is available. To this end, we were able to utilize the Open Access Directory, a collection of lists about open access (OA) to science and scholarship, maintained by the OA community at large. This list holds 204 repositories spanning a wide range of subject fields and can be accessed ",
                        tags$a(href="http://oad.simmons.edu/oadwiki/Data_repositories.", "here."),  style = "color:#232D4B"),
                      p("We also analyzed NSF PAR to get our total to 205 repositories profiled."),
                      p("Prior to the step of Data Screening and profiling this list of 205 repositories, our team conducted a literature review of articles on the subject of data reusability to understand the recent progress made towards open science and the challenges that exist in the domain. Our literature review suggested, among other valuable lessons, that solutions to address these goals can be pursued not only at the data level, but at the repository level with basic metrics for reuse and more sophisticated technological features. ", style = "color:#232D4B"),
                      
                      strong(p("With this knowledge of the relevant literature, and working in conjunction with the Iowa State University team, we profiled these 205 repositories on the basis of our evaluation framework, namely:")),
                      
                      em(p("1. Does the repository report the number of downloads, citations, views, or other metrics for datasets?"),
                      
                      p("2. How many datasets are in the repository/ what is the size of the repository?"),
                      
                      p("3. Is the repository widely accessible and has ease of use? Does the site require registration to access data?"),
                      
                      p("4. Does the repository have other interesting features that can be analyzed for their impact and/or association with data reuse?")),
                      
                      p("After completing this profiling process, our team selected 6 repositories to study, with each team member picking two of their own repositories to scrape. These 6 repositories were chosen, as previously mentioned, for their tracking of metrics of reuse, their size, their ease of access, and additionally, interesting features they displayed that could be used in our analysis of reuse. For example, the Multidisciplinary Repository Dryad contained a useful tool relating to the scope of our project, “Data (re)Usage Instructions.” Unfortunately, due to its difficulty in being scraped, we were ultimately unable to collect data from Dryad. Another example of such a feature was the “Metadata Assessment Report” from the Ecology and Biology repository the Knowledge Network for Biocomplexity (KNB). This site ran its own analysis on metadata associated with each research study, performing metadata “checks” on aspects of the metadata such as  if a methodology section is present, and if a unique identifier exists for the study."),
                      
                      strong(p("This table below provides an overview of 5 of the 6 repositories (all of our original choices but Dryad) our team from UVA chose to analyze.")),
                      
                      p(" "),
                      
                      img(src = "tablerepos.png", height = 270, width = 700),
                      
                      p(" "),
                      
                      em(p("These were just the repositories UVA analyzed. ISU also analyzed other repositories. Their site, along with a brief overview of their work, can be found in the ISU tab."))
                      
                      
             ),
             
                          
             
             
             navbarMenu("Results",
                        tabPanel("NSF PAR", style = "margin:20px",
                                          h5("Background"),
                                          br(),
                                          br(),
                                          br(),
                                          p("NSF PAR is a repository with a science focus. It contains around 130,000 total published pieces of research. NSF PAR is in many ways unlike the other repositories we chose to analyze. It does not track downloads, citations, or views. Furthermore, the majority of results it stores are not datasets, but are instead peer reviewed publications. Finally, it does not store its results directly, but instead include its results’ metadata and a link to an external repository where a user can access the entire document. However, the metadata still offered a lot for us to analyze. "),
                                         
                                         
                                          br(),
                                          p("NSF PAR allows users to sort results by date and by relevance. In order to collect our sample, we sorted by relevance and collected metadata information on the 5,000 most relevant pieces of research. Relevance is a proxy for views, as it is related to clicks. We did not filter at all on search terms in order to include all possible topics and instead searched over all possible results by simply filtering on date, including all possible dates."),
                                          br(),
                                          h5("Visuals"),
                                          p("The metadata we collected offered us the ability to analyze the publication dates of the samples and the topics they covered. Below, we can see the years that these 5,000 samples were published along with the top 100 concepts that appeared in their titles."),
                                          img(src="nsfWords.png", height = 600, width = 800),
                                          em(p("We, unsurprisingly, see many scientific concepts appear in the titles of our sample.",style = "font-size:120%")),
                                          
                                          br(),
                                          
                                          p("We can also analyze the data based on the publication dates of the pieces of research. We see a large increase in published research in our sample in 2020. The vast majority of research was posted in the past three years. Notably, all 5,000 samples were published in 2014 or later. For a bit of context, the Holdren memo, which directed federal programs to make publicly funded research available to the public within a year of publication, was signed in February of 2013."),
                                 
                                          br(),
                                          img(src = "nsfyears.png", height = 600, width = 800),
                                          br(),
                                 
                                          #img(src = "icpsr_example2.png", height = 600, width = 1000),
                                          
                                 ),


                        tabPanel("Figshare", style = "margin:20px",
                                 h5("Background"),
                                 br(),
                                 br(),
                                 br(),
                                 p("Figshare is a multidisciplinary repository with almost 6,000,000 uploaded items, including articles, datasets, reports, book chapters, etc. Figshare tracks major metrics of reuse, like downloads, citations, and views, and also collects information on download size, number of files, license, total edits, and number of keywords. Our sample size was 7,816, which we came to after accessing the 5,000 most cited and 5,000 most downloaded, merging the datasets, and removing the overlap.
                                   The median number of citations in our sample was only 1, while the mean was 1.41. The most cited dataset in our sample, and thus on the site overall, had 106 citations. In comparison, the most downloaded dataset had over 6 million downloads, and the median was 454 downloads. The median number of views was 409."),
                                # img(src="figshareKeywords.png", height = 600, width = 1000),
                                 br(),
                                 h5("Visuals"),
                                 br(),
                                
                                 p("Keywords were of particular interest for this repository, as they serve as a way for a dataset or other piece of research to be findable. This is, theoretically, a crucial first step to being downloaded or cited. While we did not analyze the actual keywords on our sample, we did collect the number of keywords on each dataset. We see below that fewer keywords tend to lead to more downloads, perhaps showing that fewer, more deliberately chosen keywords increases a dataset’s searchability."),
                                 img(src="figshareKeywords.png", height = 600, width = 800),
                                 
                                 br(),
                                 
                                 p("Licenses were also of interest. The literature argues that potential data donors are sometimes hesitant to share their data for fear that they will lose control over it or that other researchers will use it incorrectly. Licenses could potentially allay some of those fears by giving some control back to the donor. Licenses could, in these ways, affect both the donor and user. Do donors tend to choose more restrictive licenses? Do users download datasets with less restrictive licenses more frequently?"),
                                 #img(src = "fig_license_downloads.png", height = 600, width = 1000),
                                 img(src = "figlicenseschosen.png", height = 600, width = 800),
                                em(p("We can see above that the vast majority of data donors choose the CC BY Creative Commons license, with a large minority choosing the CC0 license. Very few choose any other type of license. The CC0 license, otherwise known as the No Rights Reserved license, is the least restrictive license option. In Creative Commons' words, 'CC0 empowers yet another choice altogether - the choice to opt out of copyright and database protection.' CC BY, for its part, is also a very unrestrictive license. This shows that, at least for the most downloaded and cited data sets, donors are choosing open, not restrictive, licenses.", style = "font-size:120%"),
                                 img(src = "licenseDownloadsCollapsed.png", height = 600, width = 800),
                                 p("Above we see that license doesn't seem to have a big effect on the number of downlaods. CC BY is more restrictive than CC0, and indeed we see fewer downloads of the datasets using CC BY. However, all other licenses are more restrictive than these two, and yet the Other category has more downloads that either other group.", style = "font-size:120%"))
                                 
                                
                                 
                        ),
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
                                 img(src = "dryad_pubs_per_year.png", height = 600, width = 800),
                                 br(),
                                 em(p("The graph above depicts the number of datasets published to dryad over time from our sample of 3400 datasets, spanning from 2007 to 2021. We see that over time, there is an increase in data sharing, quantified by the number of datasets published.", style = "font-size:120%"), 
                                  
                                  
                                p("Therefore, an area of analysis can be whether or not this increase correlates with an increase in data reuse, quantified by the metrics of downloads, views, and citations. If we can provide that data sharing correlates with more data reuse, we can encourage researchers to take steps to not only share their data, but make their data reusable to a higher standard, in order to facilitate a greater spread of their data through the scientific community.", style = "font-size:120%"))
                                 
                        ),
                        
                        
                        tabPanel("KNB", style = "margin:20px",
                                 h5("Background"),
                                 br(),
                                 br(),
                                 br(),
                                 p("KNB, or the Knowledge Network for Biocomplexity, is an Ecology and Biology Repository with about 30,000 uploads. Our team took particular interest in KNB because of its Metadata Assessment Report feature,  a tool that performs an analysis on the metadata associated with each research study.
                        This Metadata Assessment Report performs on average, 23 checks of metadata, checking information such as, if a methodology section is present, and if a unique identifier exists for the study. An example of such a report looks like this:"),
                                 br(),
                                 img(src="knb_example.png", height = 600, width = 800),
                                 br(),
                                 br(),
                                 p("To perform this analysis, we scraped a random 5,000 set of studies from the site, and collected Downloads, Views, and Citations, along with information on these metadata checks. In summary, this random sample average about 16 downloads per each dataset and each dataset had about 3 failures.
                        In understanding the importance of providing quality metadata in order to make data reusable, we would expect to see that failed checks were negatively associated with downloads, and this is, in fact, what we observed after performing a simple regression of using the number of failures to predict downloads. We got a coefficient of -7.5 that is highly significant at less than a .01 percent level. Below is a scatterplot showing the relationship between Failed Metadata Checks and Downloads:"), 
                        
                                 br(),
                                 img(src="md_fail.png", height = 600, width = 1000),
                                 br(),
                                 
                                 
                                 br(),
                                 p("This metadata assessment report feature holds much value for the topic of data reuse. It is of worth for both data publishers and data re-users,  as publishers are able to upload their research to KNB, and then observe, from the assessment report, which areas of their metadata is lacking. Conversely, data re-users can screen data before attempting to use them by first determining if sufficient information about the data is present. 
                        As a whole, this analysis on a sample set of studies shows the potential of such a tool for repositories with publicly accessible research data.  These methods can be replicated across repositories that have similar assessment tools, and provide insight into uncovering the specific and most glaring gaps in the reusability framework, and which policies and standards can be prioritized moving forward. "),
                                 br(),
                                 br(),
                                 br(),
                                 img(src="knb_graph.png", height = 600, width = 1000),
                                 em(p("The graph above displays the total failure count, divided by the type of failure, and we can see the most common failures being the absence of a license, a publishing date not present, and the abstract length not being sufficiently long.",style = "font-size:120%"))
                                 
                        ),
                        
                        tabPanel("ICPSR", style = "margin:20px",
                                 h5("Background"),
                                 br(),
                                 br(),
                                 br(),
                                 p("ICPSR, the International Consortium for Political and Social Research, was another repository our team profiled. We were introduced to ICPSR during our reusability workshops, that we held in June, by a researcher from the University of Michigan, who attested to the standards of reuse upheld by ICPSR. After examining study-level Metadata in ICPSR, this was clearly evident — each study’s metadata are available in any of three formats, DDI 2.5, Dublin Core, and DATS 2.2, corresponding to the Data Documentation Initiative, the Dublin Core Metadata Initiative, and the Data Tags Suite, respectively.
                        
                        ICPSR’s search capabilities allow for sorting by the most cited datasets, and we collected data on the 3,000 most cited datasets on the site, collecting downloads, and publications, along with information on the Versioning, Principal Investigator, and Funding Agency. 
                        
                        For context, these datasets were highly reused, averaging 1860 downloads and 54 publications per dataset.
                        
                        The graph below shows a visual representation of the average publications per dataset by the top funding agencies in the 3000 study sample set. As you can see, the visualization highlights the relationship between the type of funding agency, like the social science agency of the NSF and a repository in which its datasets can be found. "),
                                 br(),
                                 img(src="icpsr_graph.png", height = 600, width = 1000),
                                 br(),
                                 br(),
                                 br(),
                                 em(p("Similar analyses on other repositories can reveal valuable information about the reuse and impact of the studies that agencies fund.", style = "font-size:120%")),

                                 br(),
                                 h5("Visuals"),
                                 br(),
                                 
                                 br(),
                                 p("Additionally, another interesting and relevant feature of ICPSR is its “Usage Report,” parts of which, are displayed below. Users can create an account on ICPSR and log their affiliations with the member institution they belong to and the type of user they are, and this information is then displayed on the usage report of studies. A gap that is often unable to be filled with most publicly accessible research data and a deterrent to data sharing, this can provide valuable insight to data publishers as they are able to understand who exactly is viewing and reusing their data."),
                                 br(),
                                 img(src = "icpsr_example1.png", height = 600, width = 1000),
                                 br(),
                                 img(src = "icpsr_example2.png", height = 600, width = 1000),
                                 br()
                                 
                        )
                        

                        ),#end results tab


             tabPanel("ISU Results",style = "margin:45px",
                      fluidRow(
                        column(3, tags$img(height = "80%", width = "80%", src = "biilogo.png")),
                        column(6, h1("ISU Results")),
                        column(3, tags$img(height = "80%", width = "80%", src = "Inkedpartnerlogo.jpg", align = "right"))
                      ),
                      strong(p("Throughout the summer, we collaborated with the DSPG team at Iowa State University. Our two teams followed many of the same processes, but there were some differences. To learn more about their summer, visit their site", tags$a(href="https://faculty.sites.iastate.edu/adisak/datareuse", "here."), style = "color:#232D4B")),
                      p("We would like to highlight the results of one of their repositories here, though. Specifically, we will share what they found when analyzing the Kaggle repository, in their own words:"),
                    
                    
                    
                      p("Kaggle is an online community of data scientists and practitioners of machine learning.  Kaggle allows users to publish and explore data sets and their reusability metrics, such as size, view count, vote count, update date, download count, and their proprietary usability score.  I explored the datasets by collecting 6,900 records through the API and using web scraping tools to retrieve metadata. "),
                      img(src = "kaggle1.png", height = 400, width = 600),
                      p("Through our research, we had some interesting findings.  Many of these revolved around Kaggle’s usability rating.  According to Kaggle’s website, “It’s a single number we calculate for each dataset that rates how easy-to-use a dataset is based on a number of factors, including level of documentation, availability of related public content like kernels as references, file types and coverage of key metadata” (Goldbloom and Hamner, 2010).  This metric gave us some interesting findings, as seen in graphs like the figure above.  There is a strong correlation between the usability score and the download count in a given dataset, which could indicate that users seek out data with a higher usability rating. "),
                      img(src = "kaggle2.png", height = 400, width = 600),
                      p("We also found that the median usability rating of a dataset has increased overtime for the last four years.  The Usability Rating was introduced in May of 2019. The datasets created prior have been retrofitted with the Usability Rating calculation. Since then, the median usability rating of datasets has gradually climbed. This could indicate that the existence of such a metric has encouraged data producers to reach a higher Usability Rating by completing the metadata requirements and propel a culture of reusability on Kaggle."),
                      br(),
                      
                     em(p("ISU studied four other repositories, for a total of 5. The other repositories are NeuroMorpho, Astrophysics Data System, Global Biodiversity Information Facility, and MorphoBank. Please visit their site to learn more about their findings!"))
             )


      )
  
  #end navbarPage
  
  
  )#end fluid page





server <- function(input, output) {

  output$pub <- renderImage({

    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path('www',
                                        paste(input$year, 'Publisherplot.png', sep='')))

    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Image number", input$year))



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




