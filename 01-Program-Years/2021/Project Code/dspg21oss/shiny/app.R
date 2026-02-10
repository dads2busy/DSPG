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
if(!require(collapsibleTree)) install.packages("collapsibleTree", repos = "http://cran.us.r-project.org")

if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")


#setwd("/home/zz3hs/git/dspg21oss/shiny")

#exclude white color
uva_colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200")
uva_col_distinct<-c("#232d4b", "#0e879c", "#d9e12b", "#e6ce3a", "#e57200")

# data -----------------------------------------------------------

# collapsible tree 
df = read_csv("data_shiny/oss_software_types - dictionary.csv")
df_no_na <- df %>% 
  filter(!is.na(sourceforge_count)) %>% 
  filter(!is.na(fleming_primary)) %>%
  filter(!is.na(fleming_secondary)) %>%
  filter(main_type!="Undeveloped") %>% 
  mutate(main_type = replace(main_type, main_type=="" & summary_type!="Programming" & summary_type!="Other/Nonlisted Topic", "Other")) %>% 
  mutate(sub_type = replace(sub_type, sub_type=="" & summary_type!="Programming" & summary_type!="Other/Nonlisted Topic", "Other"))

# bert embedding 
embeddings <- read_rds("data_shiny/classified_repos_embeddings.rds") #slug with x, y, z
label_true <- read_rds("data_shiny/classified_repos_labelled.rds") #repos with labelled for each catrgory and only kept the ones is true for that category 
repo_scores <- read_rds("data_shiny/repo_score.rds") #slug with similarity score
repo_scores <- repo_scores%>%
  select(-bert)
bert_embeddings_label = label_true%>%
  left_join(embeddings, by = "slug")%>%
  left_join(repo_scores, by = c("slug", "software_type"))
bert_embeddings_label$software_type <- factor(bert_embeddings_label$software_type, levels =  c("app_blockchain", "prog_clang", "prog_java", "prog_javascript", "app_database", "topics_ai", "prog_python", "app_php", "topics_dataviz"))
levels(bert_embeddings_label$software_type)
names(uva_colors) <- levels(bert_embeddings_label$software_type)

# node embeddings
networks<-read_csv("data_shiny/tsne_df.csv")
networks<-networks %>% filter(language=="Python"|language == "Java"| language=="Javascript"| 
                                language=="PHP"| language=="C"|language=="Ruby")
names(uva_col_distinct)<-levels(networks$language)


# user -------------------------------------------------------------
ui <- navbarPage(title = "OSS",
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
                                   h1(strong("Defining and Measuring the Universe of Open Source Software Innovation"),
                                      br(""),
                                      h4("Data Science for the Public Good Program"),
                                      h4("University of Virginia"),
                                      h4("Biocomplexity Insititute"),
                                      br()
                                   )
                          ),
                          fluidRow(style = "margin: 6px;",
                                   column(4,
                                          h2(strong("Big Picture")),
                                          p("Since the advent of the internet, software has become integral part of our 
                                            lived social realities. From the rise of mobile phones to social media apps, software 
                                            shapes how we interact with those around us as well as driving much of the econonic growth seen around the world. 
                                            Federal economic indicators developed by the National Center for Science & Engineering Statistics (NCSES) 
                                            do not currently do well in measuring the value of goods and services that do not have market transactions 
                                            (i.e., they are not captured in surveys nor are they in economic measures such as the Gross Domestic Product or GDP). 
                                            Although the NCSES does track some types of software development, 
                                            it is challenging to account for software that is developed outside of traditional business contexts. Moreover, while 
                                            current measures of innovation tend to rely on survey data, patent issues, trademarks approvals, intangible asset data, 
                                            or estimates of total factor productivity growth, these measures are either incomplete or 
                                            fail to capture innovation that is freely available to the public."),
                                          
                                          img(src='oss.png', style = "display: ; margin-left: 60px; border: 1px solid #C0C0C0;", width = "300px")
                                          
                                    
                                          ),
                                   column(4,
                                          h2(strong("Project Overview")),
                                          p("To address this gap, the NCSES is interested in evaluating the economic and social impact of 
                                            Open Source Software (OSS) through the use of public administrative data. OSS refers to computer software with
                                            its source code shared with a license in which the copyright holder provides the rights to study,
                                            change, or distribute the software to anyone and for any purpose. Over the past few years, our team has aimed 
                                            to measure how much OSS is in use (stock), how much is created (flow), who is developing these tools (based on sectors, institutions, 
                                            and organizations) as well as how OSS is shared across these various institutions. In past Data Science for the Public Good projects, we developed procedures to 
                                            classify users into sectors, but very little work to date examines how different types of software are used within and across these sectors. 
                                            In this year’s OSS DSPG project, we will be collecting data from GitHub OSS repositories, classifying these repos into different OSS types, 
                                            and evaluating how these different types of software are used within and across economic sectors. 
                                            Developing a procedure to classify repositories into categories will allow the NCSES to better determine the effect that variations in software 
                                            may have on OSS contribution activity, collaboration tendencies in networked ecosystems, or on the overall cost of OSS projects.")
                                    
                                         
                                          ),
                                   column(4,
                                          h2(strong("Our Approach")),
                                          p("In this project, we study GitHub - the world's largest code hosting repository platform. The platform has roughly 40 million 
                                            users and 190 million repositories - many of which we have scraped using our own open source tools. Our dataset includes descriptive statistics on 
                                            10.2 million repositories as well as more detailed text data (READMEs) on around 157,000 projects (see Data)."), 
                                          tags$li("To classify GitHub projects into software types, we developed term-matching algorithms to allocate repos into various categories. This typology was borrowed from the Bureau of Economic Analysis and 
                                            Martin Fleming's (2021) proposed software categories that are generally useful for economists as well as more nuanced software categories. 
                                                  that we extracted from ", a(href = "https://sourceforge.net/", "SourceForge", target = "_blank"),", which are designed by and more arguably more useful for computer programmers to organize existing projects (see Software Types). 
                                                  For the 2021 DSPG summer project, we focused on 10 prominent OSS categories "), 
                                          tags$li("Third, we worked to validate that our approach generated accurate results by comparing manually validated repos to untagged repos through the use of Bidirectional Encodings 
                                                  through Representational Transformers (or ", a(href = "https://huggingface.co/transformers/model_doc/bert.html", "BERT", target = "_blank"),"). Generally, this approach allows us to embed sentences' meanings within a vector space to compare how similar 
                                            the content is using a cosine similarity metric."), 
                                          tags$li("Finally, we used node embeddings (or ", a(href = "https://snap.stanford.edu/node2vec/", "node2vec", target = "_blank"),") to compare the similarity of repository collaboration 
                                            networks based on common contributors. Like word embeddings, node embeddings place repos as vectors within a vector space with those that are 
                                            most similar closest to one another. By combing these strategies, we hope to infer software types based on their collaboration networks and improve our abilities to 
                                            classify GitHub repositories through a combination of these computational methods.")
                                          )
                                          ),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: August 2021'))))
                                   ),
                 
                 # data -----------------------------------------------------------
                 tabPanel("Data",
                          value = "socio",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Data Source and Collection"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(4,
                                          h3(strong("Data Source"),
                                             img(src='github_logo.png', style = "display: ; margin-left: 50px; border: 1px solid #C0C0C0;", width = "70px")
                                          ),

                                          #h5(strong("GitHub")),
                                          p("Our data comes from", a(href = "https://www.github.com/", "GitHub", target = "_blank"),  
                                            "- the world's largest code hosting repository platform in the world. People develop, store, 
                                            and share programming projects as repositories on GitHub. As of January 2020, GitHub had more 
                                            than 40 million users and 190 million repositories - 28 million of which are in the public domain. 
                                            Other prominent code hosting platforms include GitLab (32M users), BitBucket (5M users), LaunchPad (4M users),
                                            and SourceForge (3.7M users). While other research projects, such as GHTorrent, GitHub Archive, and Software Heritage 
                                            have aimed to collect code development activity more broadly, we are interested in examining a subset of GitHub repositories 
                                            with licenses that explicitly designate them as open source. OSS licenses allow software to be freely used, modified, and shared, 
                                            providing value for that software to be replicated and repurposed for a wide variety of purposes."),
                                          br(),
                                          br(),
                                          br(),
                                           img(src='cats.png', style = "display: ; margin-left: 5px; border: 1px solid #C0C0C0;", width = "400px",align = "center")
                                          
                                          ),
                                   column(8,
                                          
                                          h3(strong("Overaching Methodology")),
                                          p("To classify and examine GitHub repositories, we aimed to first collect a survey of all OSS repositories on GitHub in addition to 
                                            summaries of their commit activities and descriptive information such as repo descriptions and READMEs (see below). We then 
                                            developed algorithms that classified the repos into software types. This typology was borrowed from the Bureau of Economic Analysis and 
                                            Martin Fleming's (2021) proposed software categories that are generally useful for economists as well as more nuanced software categories 
                                            that we extracted from ", a(href = "https://sourceforge.net/", "SourceForge", target = "_blank"),", which are designed by and more arguably more useful for computer programmers to organize existing projects. Our initial classification effort was 
                                            predicated on a term-matching approach where we curated a nested dictionary with terms like 'tensorflow' or 'pytorch' mapping onto broader 
                                            cateogores like 'artificial intelligence/machine learning' and then aggregating up to 'software developmental tools' in our summary categories (see Software Types page 
                                            for more detils). For the 2021 DSPG summer project, we limited our curation of terms to 10 categories and then sought to validate 
                                            that our approach generated accurate results by comparing manually validated repos to untagged repos through the use of Bidirectional Encodings 
                                            through Representational Transformers (or ", a(href = "https://huggingface.co/transformers/model_doc/bert.html", "BERT", target = "_blank"),"). Generally, this approach allows us to embed sentences' meanings within a vector space to compare how similar 
                                            the content is using a cosine similarity metric. Similarly, we also decided to use node embeddings (or ", a(href = "https://snap.stanford.edu/node2vec/", "node2vec", target = "_blank"),") to compare the similarity of repository collaboration 
                                            networks based on common contributors. Like word embeddings, node embeddings place repos as vectors within a vector space with those that are 
                                            most similar closest to one another. By combing these strategies, we hope to infer software types based on their collaboration networks and improve our abilities to 
                                            classify GitHub repositories."),
                                          br(),
                                          br(),
                                     
                                          h3(strong("Repository Descriptive Data")),
                                          p("To establish our universe of OSS, we used a list of 29 Open-Source Initiative (OSI)-approved licenses 
                                            derived from the Ruby Gem Licensee on GitHub. Next, we developed an open source Julia package called", a(href = "https://uva-bi-sdad.github.io/GHOST.jl/dev/", "GHOST.jl", target = "_blank"), 
                                            "to scrape all the repos with these licenses, including the repository slug, license, description, primary language,
                                            and date created for 10,288,063 distinct repositories. Though this paper focuses on repository data, 
                                            GHOST.jl also has the capacity to collect targeted user and activity data for all OSS repos on GitHub, 
                                            which we hope to continue to collecting to monitor the impact of open source in the 
                                            years to come. For this project, our main goal was to use the repository slug (owner/repo name), descriptions and 
                                            commit histories to learn more about the types of software being developed on GitHub's platform by classifying the projects 
                                            through the use of term-matching, sentence embeddings (i.e. BERT), and node embeddings (i.e. node2vec)."),
                                          img(src='facebook_react_repo_example.png', style = "display: ; margin-left: 5px; border: 1px solid #C0C0C0;", width = "800px"),
                                          
                                          br(),
                                          br(),
                                   
                                          h3(strong("Repository Popularity and READMEs Data")),
                                          p("To supplement these repository descriptive data, we also developed two Python scripts to scrape repository ", 
                                            a(href = "https://github.com/DSPG-Young-Scholars-Program/dspg21oss/blob/main/src/01_scrape_readmes/03_scrape_repo_stats.ipynb", "popularity statistics", target = "_blank"),
                                            " and ", a(href = "https://github.com/DSPG-Young-Scholars-Program/dspg21oss/blob/main/src/01_scrape_readmes/02_scrape_readmes_requests_final.ipynb", "READMEs", target = "_blank"), 
                                            " for the top 250,000 repos ranked by the number of commits. The collection of
                                            repository popularity statistics (stars, watchers, forks, and topics) was aided by the use of the", a(href = "https://pygithub.readthedocs.io/en/latest/introduction.html", "pyGithub.", target = "_blank"),
                                            "These data tell us more about high-impact projects and allow us to compare them with those that have the most commit activity.
                                            To collect repository READMEs, we used Python's", a(href = "https://docs.python-requests.org/en/master/", "requests", target = "_blank"), 
                                            " and ", a(href = "https://www.crummy.com/software/BeautifulSoup/bs4/doc/", "Beautiful Soup", target = "_blank"), " modules to scrape the raw text of READMEs from 
                                            these repositories before joining these data sources together. Once we merged these data,
                                            our sample shrunk to 157,538 entries that had valid description and README entries. These data, in turn, were used to 
                                            classify software types using our term-matching and sentence embeddings."),
                                          img(src='facebook_react_readme_example.png', style = "display: ; margin-left: 5px; border: 1px solid #C0C0C0;", width = "700px"),
                                          br(),
                                          br(),
                                         
                                          h3(strong("Repository Network Data")),
                                          p("Finally, we decided to use a form of graph representational learning called node embeddings (specifically node2vec) to
                                            examine the similarity of projects based on their collaboration networks. Drawing from the commit activity data scraped 
                                            using the GHOST.jl package, we constructed a repository collaboration network where the nodes represent repositories and 
                                            the edges correspond to the number of common collaborators between those repos. Given the size of the full network and the 
                                            computational limitations it presents for conducting node embedding, these collaboration networks were limited to the 
                                            repositories in the 157K subset mentioned above. After isolate nodes were removed, this network ended up being comprised of 416 nodes and 5,237 edges."),
                                          br(),
                                          br(),
                                          br(),
                                          br(),
                                          br(),
                                          br()
                                   ))
                                   ),
                 
                 # software type, Cierra-----------------------------------------------------------
                 tabPanel("Software Types", value = "data",
                          h1(strong("Software Types"), align = "center"),
                          fluidRow(style = "margin: 6px;",
                                   column(4,
                                          h3(strong("Classification")),
                                          p("The main objective of our project is to classify GitHub repositories
                                            into software types so that the NCSES and other federal statisticians 
                                            can better understand the economic evaluation of labor costs and 
                                            software impact. To classify GitHub projects into software types, we 
                                            drew from two primary schemas integrating them into general “summary 
                                            types” and more nuanced “main types” and “sub types.”"),
                                          p("The summary types 
                                            include Application Software, Programming Software, System Software, 
                                            Utility Software, and General Topics (Fleming 2021), which are meant 
                                            to inform broader understandings of economic evaluation for federal 
                                            statisticians. The more nuanced main and sub types, on the other hand, 
                                            derive from categorizations provided by another prominent open source 
                                            code hosting platform named", 
                                            a(href = "https://sourceforge.net/", " SourceForge. ", target = "_blank"),
                                            "While GitHub repos do, at times, have topics listed, the site 
                                            does not organize projects by topics. SourceForge’s more 
                                            sophisticated and wide-ranging classification schema allowed us 
                                            to focus on smaller subcategories that could be aggregated into 
                                            the broader summary types later on."), 
                                          p("Using both schemas offers insight to 
                                            economists interested in evaluating the costs of broader, more general categories 
                                            as well as researchers interested in specialized, more specific categories. 
                                            Below, we used R’s collapsibleTree
                                            package to visualize how these three levels of categorization fit together."),
                                          h4(strong("Project Focus Software Categories")),
                                          p("We chose to focus in on some of the top programming language software on GitHub and a
                                            number of Application and Topic software categories. Below is a list of the specific 
                                            software categories we focused on:"),
                                          tags$ul(
                                            tags$li("Ruby"), 
                                            tags$li("Python"), 
                                            tags$li("C"),
                                            tags$li("Javascript"),
                                            tags$li("Java"),
                                            tags$li("PHP"),
                                            tags$li("Data Visualization"),
                                            tags$li("Database Management"),
                                            tags$li("Artificial Intelligence/Machine Learning"),
                                            tags$li("Blockchain")
                                          )
                                          
                                          ),
                                   column(8, 
                                          h3(strong("Collapsible Trees")),
                                          h4(strong("Fleming Classification Schema")),
                                          collapsibleTreeOutput("tree_flemming"),
                                          h4(strong("Source Forge Classification Schema")),
                                          collapsibleTreeOutput("tree_sf")
                                   )
                                          )
                          ),
                                            
                 
                 
                 # Classification Method-----------------------------------------------------------
                 tabPanel("Classification Methods", value = "data",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Classification Methods"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                      tabsetPanel(
                          # fluidRow(style = "margin: 6px;",
                          #          h1(strong("Classification Methods"), align = "center"),
                          #          br()
                          # ),
                          tabPanel("Term Matching",
                                   h3(strong(""), align = "center"),
                                   fluidRow(style='margin: 6px;',
                                            column(5,
                                                   h3(strong("Classification")),
                                                   p("To classify the repositories scraped from GitHub, a nested dictonary approach was adopted following the classificaiton of software types. 
                                                       To do so, a subset of the most popular programming languages on GitHub (Python, C, PHP, Java, Javascript) and applications (Blockchain, AI, Databases) were selected for further research. 
                                                       Each was assigned a series of keywords, ranging from the name of the programming language or topic to popular topics tagged on scraped repositories, as well as popular packages for programming languages, interfaces or applications. 
                                                       From this, term matching was used to 'flag' repository descriptions that contained these keywords, and thus, potentially belonged to the corresponding category. 
                                                       The figures below show the results of this initial classification based on keyword for the 157k repositories scraped this summer, as well as based on the 
                                                       original 10.3 million repository descriptions."),
                                                   h3(strong("Limitations")),
                                                   p("A major limitation of this method involves the prevelance of false positives and negatives that are detected. ,We observe examples of a false positive and a false negative below, noticed during the validation process."),
                                                   h5(strong("Example: False Positive")),
                                                   p("The following respository", 
                                                     a(href = "https://github.com/fsharp/fsharp", "F#"), "was incorrectly flagged as a Python repository
                                                       based on the description."),
                                                   h5(strong("Example 2: False Negative")),
                                                   p("The following repository", 
                                                     a(href = "https://github.com/apache/camel", "Apache Camel"),", an open source Java integration framework was not flagged as a Java repository.")),
                                            column(7, h3(strong("Figures")),
                                                   img(src='software_type_103.png', style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "650px"),
                                                   img(src='software_type_157.png', style = "display: inline;  5px; border: 1px solid #C0C0C0;", width = "650px"))
                                            )
                                   ),
                            tabPanel("Sentence Embeddings Estimation",  
                                     h3(strong(""), align = "center"),
                                     fluidRow(style = "margin: 6px;",
                                              column(4,
                                                     h3(strong("Methods")),
                                                     h4(strong("I. Formulate Software Type Sentence Corpus")),
                                                     p("For each software type, we manually validated repository classifications. We prioritize the repositories
                                                       with large number of stars for validation and obtained about fifty validated repositories for each software type.
                                                       And we use the descriptions of these validated repositories as our corpus. We have one sentence corpus for each 
                                                       software type."),
                                                     h4(strong("II. Calculate Sentence Embedding")),
                                                     p("We computed sentence embeddings using",
                                                       a(href = "https://www.sbert.net/", "Sentence BERT (SBERT)", target = "_blank"), 
                                                       "There are many pretrained models. We used ", 
                                                        a(href = "https://www.sbert.net/docs/pretrained_models.html", " paraphrase-mpnet-base-v2", target = "_blank"), 
                                                          "to embed repository descriptions in our corpus and repository descriptions of unlabelled repositories. 
                                                       We chose this model because it has the highest quality."),
                                                     
                                                     h4(strong("III. Compare Repo Descriptions to Sentence Corpus using Cosine-Similarity Score")),
                                                    p("For each software type, we compared the similarity between sentence embeddings of our corpus to the remaining repository descriptions using cosine-similarity. 
                                                        Cosine-similarity score ranges from 0 to 1, higher the score, more similar two sentences are to each 
                                                        other."),
                                                    
                                              
                                                    br(),
                                                    h5(strong("Example:")),
                                                       p("Transformers: State-of-the-art Natural Language Processing for Pytorch, TensorFlow, and JAX."), 
                                                    
                                                    h5(strong("Compare with Two Sentences:")),
                                                      p(strong("Sentence A"), "An Natural Language Processing library for building bots, with entity extraction, sentiment analysis, automatic language identify, and so more."), 
                                                      
                                                      p("Cosine-similarity score: 0.62. "),
                                                    
                                                      p(strong("Sentence B"),"An implementation of the Grammar of Graphics in R."),
                                                      p("Cosine-similarity score: 0.22. "),
                                                    
                                                      
                                
                                                    br(),
                                                     p("First and second sentences are describing natural language processing which obtained a higher cosine-similarity score between them. 
                                                       The last sentence is describing a data visualization tool, comparing it with sentence 1 gave us a cosine-similarity socre of 0.2."),

                                                      p("For repository (with a one-sentence repository description), we identified the top ten most similar 
                                                        sentences from our sentence corpus and obtained their cosine-similarity scores. We then took the median (due to the skewness of sentence scores) of the ten scores
                                                        and obtained an embedding score for each repository, indicating how similar the repository is to the corresponding software type. We call this" , strong("repository similarity score."),
                                                         "For each software type, we calculated the mean and standard deviation of", strong("similarity score"),	 "of all repositories. And we classify a
                                                         repository with a", strong("repository similarity score"), "that is 2 standard deviation above the mean as the corresponding software type." ),
                                                     h4(strong("IIII. Visualization")),
                                                    p("To visualize embeddings, we used the embeddings of the repositories. Embeddings are in extremely high dimension (768-D). To make sense of the embeddings 
                                                      in a lower dimension, we performed Principal Component Analysis. The interactive scatterplot shows the first two dimensions of the embeddings." ),
                                                    h4(strong("IV. Take-away")),
                                                    p("Clusters of software types is an indicator of the differences between them. However, some software types are more similar than some other software types.
                                                      For example, artificial intelligence (AI) is more similar to data visualization than to blockchain since data visualization serves as a tool for showing
                                                      AI results."),
                                                    p("We proposed 2 standard deviation above the mean as a preliminary cutoff for the similarity score. Note that higher the cutoff, clearer the decision boundry 
                                                      is between two clusters (software types).")
                                                    
                                                    
                                                     
                                                  ),
                                       column(8, 
                                              h3(strong("Embedding Visualization")),
                                              
                                                        
                                                         column(5, 
                                                                
                                                                checkboxGroupInput( "software_type", 
                                                                                       "Software Type:",
                                                                                       choices = c("App-Blockchain"= "app_blockchain",
                                                                                                   "App-Database"="app_database",
                                                                                                   "Programming Language-clang"="prog_clang",
                                                                                                   "Programming Language-Java"="prog_java",
                                                                                                   "Programming Language-Javascript"= "prog_javascript",
                                                                                                   "Programming Language-php"= "prog_php",
                                                                                                   "Programming Language-Python"= "prog_python",
                                                                                                   "Topics-Artificial Intelligence"= "topics_ai",
                                                                                                   "Topics-Data Visualization"= "topics_dataviz"),
                                                                                       selected = c("topics_ai","app_blockchain"))
                                                                ),
                                                         column(4,
                                                                sliderInput(inputId = "similarity_score", 
                                                                             label = div(style = "width:300px",
                                                                                          div(style = 'float:left;', 'lower similarity'),
                                                                                          div(style = 'float:right;', 'higher similarity')),
                                                                             min = 0, max = 1, value = 0.4, width = '300px'
                                                                            )
                                                                ),
                                                         br(),
                                                         br(),
                                                         br(),
                                                         br(),
                                                         br(),
                                                         br(),
                                                         br(),
                                                         br(),
                                                         br(),
                                                         br(),
                                                         br(),
                                                         br(),
                                                         br(),
                                                         br(),
                                                         plotlyOutput("embedding_plot", width = "800px", height = "700px"),
                                                         br(),
                                                         p("Size of the circles indicate the number of stars. Larger the circle, more stars a repository has.")
                                                         )
                                                      )

                                                  ),
                                          
                          tabPanel("Node Embeddings",  
                                   h3(strong(""), align = "center"), 
                                   fluidRow(style = "margin: 6px;",
                                            column(5, 
                                                   h3(strong("Network Descriptives")),
                                                   p(strong("Number of Nodes:"),"416",br(),
                                                     strong("Number of Edges:"), "5237",br(),
                                                     strong("Transitivity:"), ".602", br(),
                                                     strong("Density:"), ".06", br(),
                                                     strong("Average Cluster:"), ".439", br()),
                                                   h3(strong("Node2Vec")),
                                                   p("Based on a network of repositories (nodes) and collaberators (edges), the Node2Vec algorithm was used to generate embeddings. This is done through the generation
                                                of biased random walks from each node (weighted by a parameter alpha), and then using Word2Vec to generate the embeddings.
                                                To visualize the results, t-distributed stochastic neighbor embedding (tSNE) is used to reduce the dimensions, and the embeddings are plotted on a 2-D plane. 
                                                As opposed to the sentence embedding approach based on the degree of similarity in content, node embedding focuses on common collaberators, which then leads to repositories with shared collaborators clustering closer together. 
                                                As seen below, repositories with the same language (indicated by color) are clustered together. The size of the node reflects degree centrality, which indicates how \"connected\" a node is.")),
                                            column(7,h3(strong("Results")),
                                                   plotlyOutput("nodeEmbedding", width = "800px", height = "700px"),
                                                   br(),
                                                   br(),
                                                   br(),
                                                   br(),
                                                   br(),
                                                   br(),
                                                   br(),
                                                   br()
                                            )
                                          )
                                )
                    )
                    )
                    ),
                 

                 # contact -----------------------------------------------------------
                 tabPanel("Contact", value = "contact",
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
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
                          fluidRow(style = "margin-left: 20px; margin-right: 20px;",
                                   column(6, align = "center",
                                          h4(strong("DSPG Team Members")),
                                          img(src = "Crystal.jpeg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "Cierra.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "Stephanie.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = 'https://www.linkedin.com/in/crystal-zang', 'Crystal Zang', target = '_blank'), "(University of Pittsburgh Graduate School of Public Health, Biostatistics);",
                                            a(href = '', 'Cierra Oliveira', target = '_blank'), "(Clemson University, Computing and Applied Sciences);" ,
                                            a(href = '', 'Stephanie Zhang', target = '_blank'), "(University of Virginia, Mathematics (Probability/Statistics), Sociology);"),
                                          p("", style = "padding-top:10px;")
                                   ),
                                   column(6, align = "center",
                                          h4(strong("UVA SDAD Team Members")),
                                          img(src = "Brandon.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "Gizem.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = "https://biocomplexity.virginia.edu/person/brandon-kramer", 'Brandon Kramer', target = '_blank'), "(Postdoctoral Research Associate);",
                                            a(href = "https://biocomplexity.virginia.edu/person/gizem-korkmaz", 'Gizem Korkmaz', target = '_blank'), "(Research Associate Professor);"),
                                          p("", style = "padding-top:10px;")
                                   )
                          ),
                          fluidRow(#style = "margin-left: 100px; margin-right: 100px;",
                                        #style = "center",
                                       column(12, align = "center",
                                       h4(strong("Project Stakeholders")),
                                       img(src = "Carol.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                       img(src = "Ledia.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                       p(a(href = '', 'Carol Robbins' , target = '_blank'), "(NCSES, Senior Analyst);"),
                                       p(a(href = '', 'Ledia Guci', target = '_blank'), "(NCSES, Science Resource Analyst);"),
                                       p("", style = "padding-top:10px;")
                                        )
                                   ),
                          fluidRow(style = "margin-left: 20px; margin-right: 20px;",
                                   style = "center",
                                   h4(strong("Acknowledgments")),
                                   p(" In addition to our appreciation to Carol Robbins, Ledia Guci, and Bayoán Santiago Calderón for their continued support of work on OSS,
                                     we also want to thank Martin Fleming for joining us to talk about software classification.")
                                  )
                          )
                 
)


# server -----------------------------------------------------------
server <- function(input, output, session) {
  # Run JavaScript Code
  #runjs(jscode)
  
  # Tab: Software Type, collapsible tree -----------------------------------------------------
  output$tree_flemming <- renderCollapsibleTree({
    collapsibleTreeSummary(df,
                           hierarchy = c("fleming_primary", "fleming_secondary"),
                           width=800, height = 800, 
                           root = "Software Types", 
                           fontSize = 12,
                           zoomable = FALSE,
                           fillFun = colorspace::heat_hcl)
  })
  
  output$tree_sf <- renderCollapsibleTree({
    collapsibleTreeSummary(df_no_na,
                           hierarchy = c("summary_type", "main_type", "sub_type"),
                           width=800, height = 1000,  
                           root = "Software Types", 
                           fontSize = 12,
                           zoomable = FALSE,
                           attribute = "sourceforge_count",
                           fillFun = colorspace::heat_hcl)
  })
  
  
  # Tab: Classification Method, BERT------------------------------------------------
   output$embedding_plot <- renderPlotly({
     colScale <- scale_colour_manual(name = "software_type", values = uva_colors)
     
     p1 <- ggplot(
       filter(bert_embeddings_label, software_type %in% input$software_type, similarity_score > input$similarity_score), 
       aes(x=x, 
           y=y,
           slug = slug,
           description = description,
           software_type = software_type,
           stars= stars)
     ) + 
       geom_point(aes(size = stars), alpha = 0.6) +
       scale_size_continuous(range = c(0.5, 10))+ 
       xlim(-1.5, 2)+
       ylim(-1.5,2.2)+
       aes(colour = software_type)  + 
       labs(title = "Embeddings", 
            subtitle = "1st + 2nd principal components",
            x = "First Principal Component",
            y = "Second Principal Component")+
       theme_minimal()+
       colScale +
       theme(
         #legend.key.size = unit(10, 'cm'), 
         legend.text = element_text(size=10))
     
     ply1 <- ggplotly(p1, tooltip = c("slug", "software_type", "description", "stars"))%>%
       layout(legend = list(
         orientation = "h",
         y=0.99
       )
       )
     ply1
   })
  
  
  # Tab: Classification Method, Node Embedding------------------------------------------------
  
  output$nodeEmbedding <- renderPlotly({
    colScale2 <- scale_colour_manual(name = "language", values = uva_col_distinct)
    
    dim1<-list(title="t-SNE Dimension 1", showgrid=FALSE)
    
    dim2<-list(title="t-SNE Dimension 2", showgrid=FALSE)
    
    p2 <- ggplot(
      networks,
      aes(x=x, 
          y=y,
          slug = slug,
          description = description,
          language = language)
    ) + 
      geom_point(aes(size = commits), alpha = 0.6) +
      scale_size_continuous(range = c(0.5, 10))+ 
      xlim(-30, 30)+
      ylim(-30, 30)+
      aes(colour = language)  + 
      labs(title = "Node Embeddings")+
      theme_minimal()+colScale2
    ply2 <- ggplotly(p2, tooltip = c("slug", "language", "description", "page_rank"))%>%
      layout(legend = list(orientation = "h")) %>% layout(xaxis=dim1, yaxis=dim2)
    
    ply2
  })
  
  
}

shinyApp(ui = ui, server = server)