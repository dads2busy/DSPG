if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(rsconnect)) install.packages("rsconnect", repos = "http://cran.us.r-project.org")
if(!require(shinycssloaders)) install.packages("shinycssloaders", repos = "http://cran.us.r-project.org")
if(!require(shinyjs)) install.packages("shinyjs", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")

if(!require(sjmisc)) install.packages("sjmisc", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(gt)) install.packages("gt", repos = "http://cran.us.r-project.org")


colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

#setwd("/Users/czang/Documents/UVA/2021DSPG/CrystalCode/dspg21RnD/shiny")
# data -----------------------------------------------------------
abstracts_gather  = readRDS("data_shiny/abstracts_gather.rds")


# user -------------------------------------------------------------
ui <- navbarPage(title = HTML("<img src='./DSPG_black-01.png' width='120px' style='margin-top:-10px;'/>"),
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

                 #Tab1. Overview -----------------------------------------------------------
                 tabPanel(id= "overview","Overview", value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   # br("", style = "padding-top:2px;"),
                                   # img(src = "uva-dspg-logo.jpg", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                   br(""),
                                   h1(strong("R&D Text Corpora Filtering and Data Mining"),
                                      br(""),
                                      h4("Data Science for the Public Good Program"),
                                      h4("University of Virginia"),
                                      h4("Biocomplexity Insititute"),
                                      br()
                                   )
                          ),
                          fluidRow(style = "margin: 6px;",
                                   column(6,
                                          h2(strong("Project Background")),
                                          p( strong("The project"), 
                                             "The research builds upon an ongoing collaboration between the UVA Biocomplexity Institute Social and Decision Analytics Division (SDAD) and the National Center for Science and Engineering
                                             Statistics (NCSES) examining the use of administrative records to supplement or enhance data collected in NCSES surveys."),
                                          p(strong("Past study"), "This project is a continuation of last year's project:", a(href = "https://dspgtools.shinyapps.io/RnD-Emerging-Topics/", "R&D Abstracts: Emerging Topic Identification", target = "_blank"),
                                            ". Abstracts were cleaned and pre-processed through tokenization and lemmatization, stop word removal, and n-grams creation.")

                                          ),
                                   column(6,
                                          h2(strong("Research Question")),
                                          p("How can we identify emerging topics within a particular concept in a corpus? For example, what are the emerging topics in the area of Artificial Intelligence (AI) within Federal RePORTER?"),
                                          p("What is the trade-off between automatic processes and those that require human intervention to accomplish the first task? "),
                                          h2(strong("Our Work")),
                                          p("AI is a complex and hard to define theme, so this filtering problem is challenging.
                                            This project implemented and explored three  information retrieval/corpus filtering method to
                                            identify Federal RePORTER project abstracts that are related to AI:"),
                                          tags$li(strong("Term Matching Filtering Method"), ", proposed by the Organization for Economic Co-operation and Development (OECD)."),
                                          tags$li(strong("Term Matching + Latent Dirichlet Allocation (LDA)"), ", proposed by Eads and others."),
                                          tags$li(strong("Sentence BERT Embeddings"), "that compares the similarity between the AI Wikipedia page and grant abstract."),

                                          p("After retrieving abstracts that are related to AI, we performed topic modeling and identified some merging topics within the field of AI."),
                                          p("This dashboard compiles our findings and allows extension professionals, stakeholders, and other users to explore the information interactively.")
                                          )
                                   ),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: August 2021')))
                                   )
                            ),

                # Tab2. Data and Methods -----------------------------------------------------------
                 tabPanel(id= "data_methods","Data & Methods", value = "socio",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Data and Methodology"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(5,
                                          h4(strong("Data")),
                                          p("We examined research projects abstracts within the", a(href = "https://federalreporter.nih.gov/", "Federal RePORTER.", target = "_blank")),
                                          p("The Federal RePORTER is a database of federally funded science research projects. The database provides information on projects, including supporting
                                            agencies, starting dates, project terms, and abstracts. Our dataset consisted all 690814 project abstracts that began within the years of 2008-2019."),
                                          p(""),
                                          img(src = "all_data_agency.png", style = "display:  center; margin-left: auto; margin-right: auto; border: 1px solid #C0C0C0;"),
                                          p(""),
                                          img(src = "all_data_start_year.png", style = "display:  center; margin-left: auto; margin-right: auto; border: 1px solid #C0C0C0;")
                                          
                                          ),
                                   column(5,
                                          h4(strong("Methodology")),
                                          fluidRow( style = "margin: 6px;",
                                                    p("", style = "padding-top:10px;"),
                                                    h4(strong("Information Retrieval/Filtering Methods")),
                                                    p("1. Term Matching"),
                                                    p("2. Term Matching + Latent Dirichlet Allocation (LDA)"),
                                                    p("3. Sentence BERT Embeddings"),
                                                    h4(strong("Topic Modeling")),
                                                    h5(strong("Non-Negative Matrix Factorization")),
                                                    p("Non-Negative Matrix Factorization (NMF) is a unsupervised topic model. NMF is an approximate matrix decomposition that finds the
                                                      document-topic matrix (W) and topic-term matrix (H). The goal of topic modeling is to obtain the product of W and H, which produces
                                                       the document-term matrix (A). This matrix A identifies clusters of words into topics."),
                                                    img(src = "nmf_image.png", width = "400px", style = "display:  center; margin-left: auto; margin-right: auto; border: 1px solid #C0C0C0;"),

                                                    p("Latent Dirichlet Allocation (LDA) model is also commonly used for topic modeling. We chose NMF topic model because it achieved higher topic coherence than (LDA), based on last year's study."),
                                                    h4(strong("Topic Model Evaluation")),
                                                    p("We used topic coherence to evalute the strength of our topic models. Topic coherence scores measures the semantic similarity between
                                                            high scoring words in each topic and provides a way to compare models. To determine the optimal number of topics for each model, we chose the
                                                            number of topics that correlated with the highest coherence score.")
                                          ),

                                          fluidRow( style = "margin: 6px;",
                                                   p("", style = "padding-top:10px;"),
                                                   h4(strong("Identifying Emerging Topics")),
                                                          p("In order to identify which topics in AI are emerging, we use linear regression on the abstracts about a topic over the years in the data.
                                            A positive correlation between the topic prevalence in our corpus over time indicates that it is an emerging topic in AI, or a 'hot' topic.
                                            A negative correlation tells us that this topic is decreasing in popularity, or is a 'cold' topic.")
                                                          )

                                   )
                                   )
                          ),
                 # Tab3. Method 1. OECD, Cierra -----------------------------------------------------------
                 tabPanel("Filtering Method 1. Term Matching", value = "socio",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("OECD Term Matching Approach"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(4, #style = "border-right: 2px solid navy; padding: 10px 5px 5px 5px",
                                          h3(strong("Method")),
                                          p("To filter the corpus for AI, we used the method in [1] by the", a(href = "https://www.oecd.org/unitedstates/", "Organization for Economic Co-operation and Development", target = "_blank"), "(OECD).
                                            OECD created a list of key terms which are broken into two groups: core terms and non-core terms."), 
                                          br(),
                                          h4(strong(" A document is considered AI-related if its abstract contains at least one core term or two non-core terms."), style = "border: 2px solid navy; padding: 5px 5px 5px 5px"),
                                          br(),
                                          h4(strong("Core Terms: unambiguously related to AI"), style = "border: 2px solid navy; padding: 5px 5px 5px 5px"),
                                          wellPanel(id="tPanel",style="overflow-y:scroll; max-height: 200px", # ; background: lightgray",
                                                    tags$ul(tags$li("adaboost"),
                                                            tags$li("artificial intelligence"),
                                                            tags$li("artificial neural network"),
                                                            tags$li("back propagation"),
                                                            tags$li("back propagation neural network"),
                                                            tags$li("computational intelligence"),
                                                            tags$li("computer vision"),
                                                            tags$li("convolutional neural network"),
                                                            tags$li("deep belief network"),
                                                            tags$li("deep convolutional neural network"),
                                                            tags$li("deep learn"),
                                                            tags$li("deep neural network"),
                                                            tags$li("elman neural network"),
                                                            tags$li("expert system"),
                                                            tags$li("fee forward neural network"),
                                                            tags$li("inference engine"),
                                                            tags$li("machine intelligence"),
                                                            tags$li("machine learn"),
                                                            tags$li("machine translation"),
                                                            tags$li("machine vision"),
                                                            tags$li("multilayer neural network"),
                                                            tags$li("natural language process"),
                                                            tags$li("perceptron"),
                                                            tags$li("random forest"),
                                                            tags$li("rbf neural network"),
                                                            tags$li("recurrent neural network"),
                                                            tags$li("self organize map"),
                                                            tags$li("spike neural network"),
                                                            tags$li("supervise learn"),
                                                            tags$li("support vector machine"),
                                                            tags$li("svm classifier"),
                                                            tags$li("unsupervised learn"))),
                                          br(),
                                          h4(strong("Non-core Terms: could be related to AI or another discipline"), style = "border: 2px solid navy; padding: 5px 5px 5px 5px"),
                                          wellPanel(id="tPanel",style="overflow-y:scroll; max-height: 200px", #; background: lightgray",
                                                    tags$ul(tags$li("actor critic"),
                                                            tags$li("analog vlsi"),
                                                            tags$li("associative memory"),
                                                            tags$li("autonomous vehicle"),
                                                            tags$li("bayes classifer"),
                                                            tags$li("bayesian belief network"),
                                                            tags$li("bioinformatics"),
                                                            tags$li("camera calibration"),
                                                            tags$li("collaborative system"),
                                                            tags$li("commonsense reason"),
                                                            tags$li("computational biology"),
                                                            tags$li("datum mine"),
                                                            tags$li("decision tree"),
                                                            tags$li("description logic"),
                                                            tags$li("dimensionality reduction"),
                                                            tags$li("discriminant analysis"),
                                                            tags$li("fuzzy logic"),
                                                            tags$li("gene ontology"),
                                                            tags$li("hide markov model"),
                                                            tags$li("humanoid"),
                                                            tags$li("image alignment"),
                                                            tags$li("image match"),
                                                            tags$li("information retrieval"),
                                                            tags$li("kegg pathway"),
                                                            tags$li("knowledge base"),
                                                            tags$li("knowledge representation and reason"),
                                                            tags$li("linear discriminant"),
                                                            tags$li("markov decision process"),
                                                            tags$li("mulitclass classification"),
                                                            tags$li("naive bayes"),
                                                            tags$li("name entity recognition"),
                                                            tags$li("near neighbor classifier"),
                                                            tags$li("neural network"),
                                                            tags$li("neuro fuzzy"),
                                                            tags$li("neuromorphic compute"),
                                                            tags$li("neuromorphic hardware"),
                                                            tags$li("non rigid registration"),
                                                            tags$li("nonmonotonic reason"),
                                                            tags$li("object recognition"),
                                                            tags$li("opinion mine"),
                                                            tags$li("optimal search"),
                                                            tags$li("pattern analysis"),
                                                            tags$li("pattern recognition"),
                                                            tags$li("person re identification"),
                                                            tags$li("principal component analysis"),
                                                            tags$li("question answer"),
                                                            tags$li("radial basis function"),
                                                            tags$li("rbf kernel"),
                                                            tags$li("reinforcement learn"),
                                                            tags$li("rigid registration"),
                                                            tags$li("robot"),
                                                            tags$li("sarsa"),
                                                            tags$li("sensor datum fusion"),
                                                            tags$li("sensor network"),
                                                            tags$li("speech recognition"),
                                                            tags$li("stereo match"),
                                                            tags$li("symbolic reason"),
                                                            tags$li("system and control theory"),
                                                            tags$li("template match"),
                                                            tags$li("text categorization"),
                                                            tags$li("text mine"),
                                                            tags$li("text summarization"),
                                                            tags$li("word sense disambiguation"))),
                                          p(tags$small("[1] OECD. (2019). Identifying government funding of AI-related R&D projects - An initial exploration based on US NIH and NSF project funding data.  Directorate for Science, Technology, and Innovation and Committee for Scientific and Technological Policy. Organization for Economic Co-operation and Development (OECD). Paris, France."))),
              
                                   
                                   column(8,
                                          h3(strong("Results")),
                                          h4(strong("In total, 7933 projects are classified as AI related with this method.")),
                                          tabsetPanel(id= "m1tabs",
                                            tabPanel(id= "m1.1","Funding Sources",
                                                    p(""),
                                                  img(src = "oecd_agency.png",  style = "display:  block; margin-left: auto; margin-right: auto; border: 1px solid #C0C0C0;", width = "450px"), #style = "display:  center; margin-left: auto; margin-right: auto; border: 1px solid #C0C0C0;"), 
                                                  p(""),
                                                  img(src = "oecd_start_year.png",  style = "display:  block; margin-left: auto; margin-right: auto; border: 1px solid #C0C0C0;", width = "450px") #style = "display:  center; margin-left: auto; margin-right: auto; border: 1px solid #C0C0C0;") 

                                            ),
                                            tabPanel(id= "m1.2","NMF Model Fitting",
                                                     p(""),
                                                     p("The highest Coherence Score was achieved with ", strong("Number of Topics = 20. "), "Based on the coherence model, 20 was the optimal number of topics."),
                                                     img(src = "oecd_coherence.png", style = "display:  block; margin-left: auto; margin-right: auto; border: 1px solid #C0C0C0;", width = "600px")
                                            ),
                                            tabPanel(id= "m1.3","Emerging Topics",
                                                     p(""),
                                                     p("Our interactive plots visualize the topic trends among identified AI-related abstracts."),
                                                     
                                                     tags$ul(
                                                       tags$li(strong("Mean Topic Weight"), " measures how much a topic is represented in our set of documents"), 
                                                       tags$li("The ", strong("n"), " value for each topic describes the number of documents with a mean topic weight above 0. In other words, n is the number of documents containing the specific topic.")
                                                     ),
                                                     
                                                     selectInput(
                                                       inputId = "k",
                                                       label = "Select Number of Topics:",
                                                       width = "50%",
                                                       choices = c('10 Topics', '20 Topics', '30 Topics'),
                                                       selected = '20 Topics'
                                                     ),
                                                     conditionalPanel("input.k == '10 Topics'",
                                                                      img(src = "oecd_10_topic_trends.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "800px")
                                                     ),
                                                     conditionalPanel("input.k == '20 Topics'",
                                                                      img(src = "oecd_20_topic_trends.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "800px")
                                                     ),
                                                     conditionalPanel("input.k == '30 Topics'",
                                                                      img(src = "oecd_30_topic_trends.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "800px")
                                                     ),
                                                     br(),
                                                     fluidRow(style = "border: 2px solid navy; padding: 5px 5px 5px 5px",
                                                       h4(strong("Takeaways:")),
                                                       tags$ul(
                                                        tags$li("Machine Learning (topic 2) and Robotics (topic 3) were among the topics with the most ",strong("increasing"), " mean topic weight."), 
                                                        tags$li("Machine Learning (topic 2) has the ", strong("highest n value")," indicating that a number of abstracts may mention or describe applications of machine learning."), 
                                                        tags$li("Some topics, like Document Retrieval (topic 13) were decreasing in mean topic weight, despite having a high n value."), 
                                                        tags$li("Computational Biology (topic 19) and Network Communication (topic 20) were among the topics with the most " ,strong("decreasing"), " mean topic weight.")
                                                        )
                                                     )
                                                     
                                            )
                                          )
                                   )
                                          )
                          ),


                # Method 2. Term Matching + LDA, Haleigh -----------------------------------------------------------
                tabPanel("Filtering Method 2. Term Matching + LDA", value = "socio",
                         fluidRow(style = "margin: 6px;",
                                  h1(strong("LDA and Term Matching Approach"), align = "center"),
                                  p("", style = "padding-top:10px;"),
                                  column(4,
                                         h3(strong("Method")),
                                         p("This method uses a combination of term matching and LDA in order to filter to AI-related grant abstracts."),
                                         p("Eads and others use a more supervised method for filtering their corpus.  We adapt this to our abstract data.
                                            The method uses LDA topic modeling in combination with a keyword list in order to identify topics about AI in
                                            a corpus whether they directly mention the keywords or not. Before topic modeling, we filter our data to only
                                            include projects produced by the NSF, and within the subject area of Computer and Information Science and Engineering.
                                            We do this to have a more narrow corpus in hopes to increase the AI presence within the data.  After filtering, we have
                                            about 16,000 grant abstracts.  We found that an LDA topic model with 100 topics gave us several topics about AI. As an intital
                                            keyword list, we use the AI core terms identified by the OECD paper (ref?), and extend this with the most common words used in
                                            the AI-related topics identified by our topic model."),
                                         p("After obtaining the AI-related topics and keyword list, we used the /wheat_filtration/ package made available by Eads et al. to
                                            filter the overall corpus to those related to AI."),
                                         p(tags$small("[2] Eads, A., Schofield, A., Mahootian, F., Mimno, D., & Wilderom, R. (2021). Separating the wheat from the chaff: A topic and keyword-based procedure for identifying research-relevant text. Poetics, 86 (Article 101527)."))
                                         ),
                                  column(8,
                                         h3(strong("Results")),
                                         h4(strong("In total, 12694 projects are classified as AI related with this method.")),
                                         tabsetPanel(
                                           tabPanel("Funding Sources",
                                                    br(),
                                                    img(src = "eads_agency.png", style = "display:  block; margin-left: auto; margin-right: auto; border: 1px solid #C0C0C0;", width = "450px"),  #style = "display:  center; margin-left: auto; margin-right: auto; border: 1px solid #C0C0C0;"),
                                                    br(),
                                                    img(src = "eads_start_year.png",  style = "display:  block; margin-left: auto; margin-right: auto; border: 1px solid #C0C0C0;", width = "450px") #style = "display:  center; margin-left: auto; margin-right: auto; border: 1px solid #C0C0C0;")

                                           ),
                                           tabPanel("NMF Model Fitting",
                                                    p(""),
                                                    p(strong("Number of topics = 15 "), "achieved the highest coherence with topics. Based on the coherence model, 15 is the optimal number of topics."),
                                                    img(src = "nmf_fitting_eads.png",  style = "display:  block; margin-left: auto; margin-right: auto; border: 1px solid #C0C0C0;", width = "600px") #style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "800px")
                                           ),
                                           tabPanel("Emerging Topics",
                                                    p(""),
                                                    p("Our interactive plots visualize the topic trends among identified AI-related abstracts."),

                                                    tags$ul(
                                                      tags$li(strong("Mean Topic Weight"), " measures how much a topic is represented in our set of documents"), 
                                                      tags$li("The ", strong("n"), " value for each topic describes the number of documents with a mean topic weight above 0. In other words, n is the number of documents containing the specific topic.")
                                                    ),
                                                    selectInput(
                                                      inputId = "k2",
                                                      label = "Select Number of Topics:",
                                                      width = "50%",
                                                      choices = c('10 Topics', '15 Topics', '20 Topics', '30 Topics'),
                                                      selected = '15 Topics'
                                                    ),
                                                    conditionalPanel("input.k2 == '10 Topics'",
                                                                     img(src = "eads_10_topic_trends.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "800px")
                                                    ),
                                                    conditionalPanel("input.k2 == '15 Topics'",
                                                                     img(src = "eads_15_topic_trends.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "800px")
                                                    ),
                                                    conditionalPanel("input.k2 == '20 Topics'",
                                                                     img(src = "eads_20_topic_trends.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "800px")
                                                    ),
                                                    conditionalPanel("input.k2 == '30 Topics'",
                                                                     img(src = "eads_30_topic_trends.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "800px")
                                                    ),
                                                    br(),
                                                    fluidRow(style = "border: 2px solid navy; padding: 5px 5px 5px 5px",
                                                      h4(strong("Takeaways:")),
                                                      tags$ul(
                                                        tags$li("Network Analysis (Topic 2)  and Robotics (Topic 4) are among the topics with a positive relationship between mean topic weight and the years in our data, meaning that they are emerging topics in our corpus."), 
                                                        tags$li("Speech Recognition (Topic 15) is decreasing the most over time."), 
                                                        tags$li("Despite decreasing mean topic weights, some topics still have a large presence in our AI corpus.  For example, see Topic 9 which has a decreasing trend over time, yet contains abstracts than Topic 1, which has the steepest positive slope."), 
                                                        tags$li("Topic 7 has the most abstracts and lists the words 'statistical', 'algorithm', and 'theory', and has a fairly flat trend over time.  It is possible that many documents reference these terms in their methods.")
                                                      )
                                                    )

                                           )
                                         )
                                  )
                         )
                ),
                 #Tab5.  Method 3. Bert, Crystal -----------------------------------------------------------
                 tabPanel(id= "m3","Filtering Method 3. Embedding", value = "socio",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Sentence Bert Embedding"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(4,
                                          h3(strong("Method")),
                                          h4(strong("Web Scraping on AI Wiki")),
                                          p("We scraped" , a(href = "https://en.wikipedia.org/wiki/Artificial_intelligence", "Artificial Intelligence Wikipedia page", target = "_blank") , "and extracted text.
                                            We believe that Wikipedia of AI provides an accurate description of what AI is and a thorough list of subfields of AI. "),
                                          h4(strong("Sentence Embedding")),
                                          p("Sentence embedding is a type of representation that allows sentences with similar meaning to have a similar representation. Bidirectional Encoder Representations from Transformers (BERT),
                                          is a transformer-based machine learning technique for natural language processing (NLP) pre-training developed by Google. Sentence-BERT (SBERT) [3], is a modification
                                          of the pretrained BERT use network structures to derive semantically meaningful sentence embeddings."),

                                          p("We computed sentence embeddings using", a(href = "https://www.sbert.net/", "SBERT", target = "_blank"),  "We used a pre-trained model",
                                            a(href = "https://www.sbert.net/docs/pretrained_models.html", "paraphrase-MiniLM-L6-v2", target = "_blank"),
                                            "to embed AI Wiki and Federal RePORTER abstracts. This is a quick model with high quality. There are more accurate pre-trained model, but requires more computating power."),
                                          column(5, h4(strong("AI Wiki Embedding"), style = "border: 4px solid navy;")),
                                          column(5, h4(strong("Abstract Embedding"), style = "border: 4px solid navy; ")), br(),
                                          br(),
                                          br(),
                                          h4(strong("Cosine Similarity Score")),
                                          p("Cosine similarity measures the similarity between two non-zero vectors (sentences). Two same vectors would have a cosine-similarity score of 1, while two independent/perpendicular vectors
                                            would have a cosine-similarity score of 0. Higher the score, more similar two sentences are."),
                                          

                                          
                                          h5(strong("Example 1.")),
                                          p("The game of Go is an ancient board game which is considered by-far the most complex
                                            board game for computer software or artificial intelligence (AI) to solve.", style = "border: 2px solid navy; padding: 5px 5px 5px 5px"),
                                          
                                          p("Cosine-similarity score: 0.65"),
                                          
                                          h5(strong("Example 2.")),
                                          p("The multiprotein complex y-secretase proteolytically cleaves the intramembrance of amyloid precursoprotein (APP), which
                                            in turn forms the plaques found in Alzheimer's disease (AD) patients", style = "border: 2px solid navy; padding: 5px 5px 5px 5px"),

                                          p("Cosine-similarity score: 0.31"),
                                          
                                          p("We compared the embeddings of AI Wiki to Federal RePORTER abstracts using cosine-similarity. For each sentence in an abstract, we identified the top ten most similar sentences
                                            from our AI corpus and obtained their cosine-similarity scores. We then took the average of these ten scores and call it", strong("abstract similarity score"), ", which reflects how similar the abstract is to AI." ),
                                           
                                         
                                          
                                          h4(strong("Choosing a Cutoff Score")),
                                          p("We classify an abstract with a ", strong("abstract similarity score"), " that is 2.5 standard deviation above the mean as AI related."),
         
                                          p(tags$small("[3] Reimers, N., &amp; Gurevych, I. (2019). Sentence-BERT: Sentence EMBEDDINGS using Siamese Bert-Networks.")),
                                          br(),
                                          br(),
                                          br(),
                                          br()
                                          ),




                                   column(8,
                                          h3(strong("Results")),

                                          h4(strong("In total, 7658 projects are classified as AI related with this method.")),

                                          tabsetPanel(
                                            tabPanel("Funding Sources",
                                                      br(),
                                                      img(src = "bert_agency.png",  style = "display:  block; margin-left: auto; margin-right: auto; border: 1px solid #C0C0C0;", width = "450px"), #style = "display:  center; margin-left: auto; margin-right: auto; border: 1px solid #C0C0C0;"),
                                                      br(),
                                                      img(src = "bert_start_year.png",  style = "display:  block; margin-left: auto; margin-right: auto; border: 1px solid #C0C0C0;", width = "450px") #style = "display:  center; margin-left: auto; margin-right: auto; border: 1px solid #C0C0C0;")


                                            ),
                                            tabPanel("NMF Model Fitting",
                                                     p(""),
                                                     p(strong("Number of topics = 10")," achieved the highest coherence with topics. Based on the coherence model, 10 is the optimal number of topics."),
                                                     img(src = "nmf_fitting_bert.png",  style = "display:  block; margin-left: auto; margin-right: auto; border: 1px solid #C0C0C0;", width = "600px") #style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "800px")
                                            ),
                                            tabPanel("Emerging Topics",
                                                     p(""),
                                                     p("Our interactive plots visualize the topic trends among identified AI-related abstracts."),

                                                     tags$ul(
                                                       tags$li(strong("Mean Topic Weight"), " measures how much a topic is represented in our set of documents"), 
                                                       tags$li("The ", strong("n"), " value for each topic describes the number of documents with a mean topic weight above 0. In other words, n is the number of documents containing the specific topic.")
                                                     ),
                                                     selectInput(
                                                       inputId = "k3",
                                                       label = "Select Number of Topics:",
                                                       width = "50%",
                                                       choices = c('10 Topics', '20 Topics', '30 Topics'),
                                                       selected = '20 Topics'
                                                       ),
                                                     conditionalPanel("input.k3 == '10 Topics'",
                                                       img(src = "bert_trends_10topics.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "800px")
                                                     ),
                                                     conditionalPanel("input.k3 == '20 Topics'",
                                                       img(src = "bert_trends_20topics.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "800px")
                                                     ),
                                                     conditionalPanel("input.k3 == '30 Topics'",
                                                                      img(src = "bert_trends_30topics.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "800px")
                                                     ),
                                                     br(),
                                                     fluidRow(style = "border: 2px solid navy; padding: 5px 5px 5px 5px",
                                                      h4(strong("Takeaways:")),
                                                      tags$ul(
                                                       tags$li("Predictive medicine (topic 2) and image recognition (topic 5) were among the topics with the most increasing mean topic weight."), 
                                                       tags$li("Innovative social science (topic 8), user interaction (topic 16) are the topics had most occurance in the documents, indicating that many ai related 
                                                               abstracts may be the applications of ai."), 
                                                       tags$li("Neural network(topic 20) is the the most decreasing mean topic weight, but it occured in many documents, indicating it's widely studied.")
                                                      )
                                                     )
                                                     

                                            )
                                          )
                                    )
                                   )
                          ),

               tabPanel("Discussion", value = "socio",
                        fluidRow(style = "margin: 6px;",
                                 h1(strong("Discussion"), align = "center"),
                                 p("", style = "padding-top:10px;"),
                            
                                 column(8,
                                        h3(strong("Filtering Method Comparison")),
                                        tabsetPanel(
                                                    tabPanel("Funding Sources",
                                                             br(),
                                                             br(),
                                                             abstracts_gather%>%
                                                               group_by(method)%>%
                                                               rename("Filtering Method" ="method")%>%
                                                               summarize(N=n(), "Percent (%)" = round(N/690814*100, digits = 2))%>%
                                                               gt()%>%
                                                               tab_header(
                                                                 title = "Comparison of The Number of Projects that are Classified as AI",
                                                                 subtitle = "Project Start Year From 2008 to 2018"),

                                                             br(),
                                                             br(),
                                                             br(),

                                                             img(src = "all_agency.png", width = "650px", style = "display:  center; margin-left: 50px ; margin-right: auto; border: 1px solid #C0C0C0;"),
                                                             br(),

                                                             img(src = "all_start_year.png", width = "650px", style = "display:  center; margin-left: 50px; margin-right: auto; border: 1px solid #C0C0C0;")


                                                    ),
                                                    tabPanel("NMF Model Fitting",
                                                             p("First filtering method: Term Matching proposed by OECD achieved the highest topic coherence."),
                                                             img(src = "nmf_coherence_all.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "800px")
                                                    )
                                                 )
                                        ),
                                 column(4,
                                        h3(strong("Future Work")),
                                        h5(strong("Precision and Recall")),
                                        p("To evaluate the quality of these three filtering methods,
                                                  we plan to measure their precision and recall. Precision is
                                                  also known as positive predictive value, and recall is known
                                                  as sensitivity."),
                                        p("For each filtering method, we would randomly
                                                  select a certain number abstracts that are classified as AI related and are
                                                  classified as NOT AI related. And manually label them into: true positive,
                                                  true negative, false positive, and false negative. And then we would compare
                                                  the precision and recall for each filtering method.")
                                        
                                 )
                                )
                      ),

                 # contact -----------------------------------------------------------
                 tabPanel(id= "contact","Contact", value = "contact",
                          fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                                   h1(strong("Contact"), align = "center"),
                                   br(),
                                   h4(strong("UVA Data Science for the Public Good")),
                                   p("The", a(href = 'https://biocomplexity.virginia.edu/social-decision-analytics/dspg-program', 'Data Science for the Public Good (DSPG) Young Scholars program', target = "_blank"),
                                     "is a summer immersive program held at the", a(href = 'https://biocomplexity.virginia.edu/social-decision-analytics', 'University of Virginia Biocomplexity Institute’s Social and Decision Analytics division (SDAD).'),
                                     "In its seventh year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around
                                     critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences
                                     to determine how information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program
                                     highlights, how to apply, and our annual symposium, please visit", a(href = 'https://biocomplexity.virginia.edu/social-decision-analytics/dspg-program', 'the official Biocomplexity DSPG website.', target = "_blank")),
                                   p("", style = "padding-top:10px;")
                                   ),
                          fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                                   column(6, align = "center",
                                          h4(strong("DSPG Team Members")),
                                          img(src = "team-Crystal.jpeg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-Cierra.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-Haleigh.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = '', 'Crystal Zang', target = '_blank'), "(University of Pittsburgh Graduate School of Public Health, Biostatistics);",
                                            a(href = '', 'Cierra Oliveira', target = '_blank'), "(Clemson University);",
                                            a(href = '', 'Haleigh Tomlin', target = '_blank'), "(Washington and Lee University)."),
                                          p("", style = "padding-top:10px;")
                                   ),
                                   column(6, align = "center",
                                          h4(strong("UVA SDAD Team Members")),
                                          img(src = "team-Kathryn.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-Eric.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-Joel.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-Stephanie.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = "", 'Kathryn Kinehan', target = '_blank'), "(Project Lead, Research Scientist);",
                                            a(href = "", 'Eric Oh', target = '_blank'), "(Research Assistant Professor);",
                                            a(href = '', 'Joel Thurston', target = '_blank'), "(Senior Scientist).",
                                          a(href = "", 'Stephanie Shipp', target = '_blank'), "(Deputy Division Director, Research Professor);"),
                                          p("", style = "padding-top:10px;")
                                    )
                                    ),
                          fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                                   h4(strong("Project Stakeholders")),
                                   p(a(href = '', 'John Jankowski', target = '_blank'), "(NCSES, Director of R&D Statistics Program);",
                                     a(href = '', 'Audrey Kindlon', target = '_blank'), "(NCSES, Survey Statistician)."),
                                   p("", style = "padding-top:10px;"),
                                   h4(strong("Acknowledgments")),
                                   p("We would like to thank  XXXXX  for their input to this project.")
                          )
                        )
)



# server -----------------------------------------------------------
server <- function(input, output, session) {

}

shinyApp(ui = ui, server = server)
