
# get_dictionary_terms pulls in the oss_software_types dictionary terms 

get_dictionary_terms <- function(summary_type, main_type, sub_type){
  
  library("dplyr")
  library("readr")
  library("tidyr")
  
  if( missing(main_type) & missing(sub_type) ){ 
    
    # this is to pull all of the terms for a summary_type 
    
    summary_type <- enquo(summary_type)
    setwd("~/git/dspg21oss/docs/")
    software_types <- readr::read_csv("oss_software_types - dictionary - co.csv") %>% 
      unnest_legacy(terms = strsplit(terms, "\\|")) 
    software_terms <- software_types %>% 
      filter(summary_type == {{ summary_type }})
    software_terms <- na.omit(software_terms$terms)
    software_terms
    
  } else if (missing(summary_type) & missing(sub_type) ){
    
    # this is to pull all of the terms for a main_type  
    
    main_type <- enquo(main_type)
    setwd("~/git/dspg21oss/docs/")
    software_types <- readr::read_csv("oss_software_types - dictionary - co.csv") %>% 
      unnest_legacy(terms = strsplit(terms, "\\|")) 
    software_terms <- software_types %>% 
      filter(main_type == {{ main_type }}) 
    software_terms <- na.omit(software_terms$terms)
    software_terms
    
  } else if (missing(summary_type) & missing(main_type)) {
    
    sub_type <- enquo(sub_type)
    setwd("~/git/dspg21oss/docs/")
    software_types <- readr::read_csv("oss_software_types - dictionary - co.csv") %>% 
      unnest_legacy(terms = strsplit(terms, "\\|")) 
    software_terms <- software_types %>% 
      filter(sub_type == {{ sub_type }})
    software_terms <- na.omit(software_terms$terms)
    software_terms
    
  } else if ( missing(summary_type) & sub_type == FALSE ){
    
    # this is to pull all of the terms for main_type general category   
    
    main_type <- enquo(main_type)
    setwd("~/git/dspg21oss/docs/")
    software_types <- readr::read_csv("oss_software_types - dictionary - co.csv") %>% 
      unnest_legacy(terms = strsplit(terms, "\\|")) 
    software_terms <- software_types %>% 
      filter(main_type == {{ main_type }} & is.na(sub_type)) 
    software_terms <- na.omit(software_terms$terms)
    software_terms
    
  } else if ( main_type == FALSE & sub_type == FALSE ){ 
    
    # this is to pull all of the terms for a summary_type when all else is NA  
    
    summary_type <- enquo(summary_type)
    setwd("~/git/dspg21oss/docs/")
    software_types <- readr::read_csv("oss_software_types - dictionary - co.csv") %>% 
      unnest_legacy(terms = strsplit(terms, "\\|")) 
    software_terms <- software_types %>% 
      filter(summary_type == {{ summary_type }} & is.na(main_type))
    software_terms <- na.omit(software_terms$terms)
    software_terms
    
  } else {
    
    "ERROR: Variables were not correctly. Please try again."
    
  }
  
}

# detect_subcategory is a function embedded within the software_type detector that 
# classifies all of the subcategories by aggregating the total terms in that category 

detect_types <- function(df, id, input, terms){
  
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytext")
  library("tidytable")
  
  input <- enquo(input)
  output <- enquo(terms)
  id <- enquo(id)
  
  tmp_df <- df %>% 
    as.data.frame() %>% 
    tidytext::unnest_tokens(word, !!input) %>%
    as_tidytable() %>% 
    tidytable::filter.(word %in% terms) %>% 
    tidytable::mutate.("{{output}}" := 1) %>% 
    tidytable::select.(-word) %>% 
    tidytable::summarize.("{{output}}" := sum(!!output), .by = !!id) 
  
  df <- df %>% 
    tidytable::left_join.(tmp_df) %>% 
    tidytable::mutate.("{{output}}" := replace_na.(!!output, 0)) %>% 
    as.data.frame()
  
  df
}

detect_system_sw <- function(df, id, input, sum_only = FALSE, prob = FALSE){
  
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytext")
  library("tidytable")
  
  sys_general <- get_dictionary_terms(summary_type = "System", main_type = F, sub_type = F)
  sys_os <- get_dictionary_terms(main_type = "Operating Systems")
  sys_windows <- get_dictionary_terms(sub_type = "Windows")
  sys_linux <- get_dictionary_terms(sub_type = "Linux")
  sys_mac <- get_dictionary_terms(sub_type = "Mac")
  sys_modern <- get_dictionary_terms(sub_type = "Modern")
  sys_bsd <- get_dictionary_terms(sub_type = "BSD")
  sys_android <- get_dictionary_terms(sub_type = "Android")
  sys_solaris <- get_dictionary_terms(sub_type = "Solaris")
  sys_virtual <- get_dictionary_terms(sub_type = "Virtualization")
  sys_handheld <- get_dictionary_terms(sub_type = "Handheld/Embedded Operating Systems")
  sys_emulapi <- get_dictionary_terms(sub_type = "Emulation and API Compatability")
  sys_grouping <- get_dictionary_terms(sub_type = "Grouping and Descriptive Categories")
  sys_other <- get_dictionary_terms(sub_type = "Other Operating Systems")
  
  id <- enquo(id)
  input <- enquo(input)
  
  df <- df %>% 
    as_tidytable() %>% 
    tidytable::mutate.("{{input}}" := tolower({{ input }})) %>% 
    detect_types(!!id, !!input, sys_general) %>% 
    detect_types(!!id, !!input, sys_os) %>% 
    detect_types(!!id, !!input, sys_windows) %>% 
    detect_types(!!id, !!input, sys_linux) %>% 
    detect_types(!!id, !!input, sys_mac) %>% 
    detect_types(!!id, !!input, sys_modern) %>% 
    detect_types(!!id, !!input, sys_bsd) %>% 
    detect_types(!!id, !!input, sys_android) %>% 
    detect_types(!!id, !!input, sys_solaris) %>% 
    detect_types(!!id, !!input, sys_virtual) %>% 
    detect_types(!!id, !!input, sys_handheld) %>% 
    detect_types(!!id, !!input, sys_emulapi) %>% 
    detect_types(!!id, !!input, sys_grouping) %>%
    detect_types(!!id, !!input, sys_other) 
  
  df <- df %>% 
    as.data.frame() %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(system_all = sum(across(contains("sys_")), na.rm = TRUE))
  
  if( sum_only == TRUE ){
    
    df <- df %>% select(-starts_with("sys_"))
    
  } else { df }
  
  if( prob == TRUE ){
    
    tmp_df <- readme_raw_data %>% 
      as.data.frame() %>% 
      tidytext::unnest_tokens(word, !!input) %>%
      as_tidytable() %>% 
      tidytable::count.(!!id, name = "n_words")
    
  } else { df }
  
  df
  
}



detect_utility_sw <- function(df, id, input, sum_only = FALSE, prob = FALSE){
  
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytext")
  library("tidytable")
  
  util_general <- get_dictionary_terms(summary_type = "Utility", main_type = F, sub_type = F)
  util_security <- get_dictionary_terms(main_type = "Security")
  util_crypto <- get_dictionary_terms(sub_type = "Cryptography")
  util_pwdmgr <- get_dictionary_terms(sub_type = "Password Manager")
  util_malware <- get_dictionary_terms(sub_type = "Anti-Malware")
  util_virus <- get_dictionary_terms(sub_type = "Anti-Virus")
  util_spam <- get_dictionary_terms(sub_type = "Anti-Spam")
  
  id <- enquo(id)
  input <- enquo(input)
  
  df <- df %>% 
    as_tidytable() %>% 
    tidytable::mutate.("{{input}}" := tolower({{ input }})) %>% 
    detect_types(!!id, !!input, util_general) %>% 
    detect_types(!!id, !!input, util_security) %>% 
    detect_types(!!id, !!input, util_crypto) %>% 
    detect_types(!!id, !!input, util_pwdmgr) %>% 
    detect_types(!!id, !!input, util_malware) %>% 
    detect_types(!!id, !!input, util_virus) %>% 
    detect_types(!!id, !!input, util_spam) 
  
  df <- df %>% 
    as.data.frame() %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(utility_all = sum(across(contains("util_")), na.rm = TRUE))
  
  if( sum_only == TRUE ){
    
    df <- df %>% select(-starts_with("util_"))
    
  } else { df }
  
  if( prob == TRUE ){
    
    tmp_df <- readme_raw_data %>% 
      as.data.frame() %>% 
      tidytext::unnest_tokens(word, !!input) %>%
      as_tidytable() %>% 
      tidytable::count.(!!id, name = "n_words")
    
  } else { df }
  
  df
  
}

detect_application_sw <- function(df, id, input, sum_only = FALSE, prob = FALSE){
  
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytext")
  library("tidytable")
  
  app_general <- get_dictionary_terms(summary_type = "Application", main_type = F, sub_type = F)
  app_mobile <- get_dictionary_terms(main_type = "Mobile")
  app_database <- get_dictionary_terms(main_type = "Database")
  app_blockchain <- get_dictionary_terms(main_type = "Blockchain")
  app_comm <- get_dictionary_terms(main_type = "Communications")
  app_texted <- get_dictionary_terms(main_type = "Text Editors")
  app_games <- get_dictionary_terms(main_type = "Games/Entertainment")
  app_deskenvi <- get_dictionary_terms(main_type = "Desktop Environments")
  app_formats <- get_dictionary_terms(main_type = "Formats/Protocols")
  app_crypto <- get_dictionary_terms(sub_type = "Crytocurrency")
  app_databaseeng <- get_dictionary_terms(sub_type = "Database Engines/Servers")
  app_vim <- get_dictionary_terms(sub_type = "Vi/Vim")
  app_emacs <- get_dictionary_terms(sub_type = "Emacs")
  app_chat <- get_dictionary_terms(sub_type = "Chat")
  app_devenvi <- get_dictionary_terms(sub_type = "Integrated Development Environments")
  app_protocols <- get_dictionary_terms(sub_type = "Protocols")
  app_graphdb <- get_dictionary_terms(sub_type = "Graph Databases")
  app_gamedev <- get_dictionary_terms(sub_type = "Game Development Frameworks")
  app_frontends <- get_dictionary_terms(sub_type = "Front-Ends")
  
  
  id <- enquo(id)
  input <- enquo(input)
  
  df <- df %>% 
    as_tidytable() %>% 
    tidytable::mutate.("{{input}}" := tolower({{ input }})) %>% 
    detect_types(!!id, !!input, app_general) %>% 
    detect_types(!!id, !!input, app_mobile) %>% 
    detect_types(!!id, !!input, app_database) %>% 
    detect_types(!!id, !!input, app_blockchain) %>% 
    detect_types(!!id, !!input, app_comm) %>% 
    detect_types(!!id, !!input, app_texted) %>% 
    detect_types(!!id, !!input, app_games) %>% 
    detect_types(!!id, !!input, app_deskenvi) %>% 
    detect_types(!!id, !!input, app_formats) %>%
    detect_types(!!id, !!input, app_crypto) %>% 
    detect_types(!!id, !!input, app_databaseeng) %>% 
    detect_types(!!id, !!input, app_vim) %>% 
    detect_types(!!id, !!input, app_emacs) %>% 
    detect_types(!!id, !!input, app_chat) %>% 
    detect_types(!!id, !!input, app_devenvi) %>% 
    detect_types(!!id, !!input, app_protocols) %>% 
    detect_types(!!id, !!input, app_graphdb) %>% 
    detect_types(!!id, !!input, app_gamedev) %>% 
    detect_types(!!id, !!input, app_frontends)
  
  df <- df %>% 
    as.data.frame() %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(app_all = sum(across(contains("app_")), na.rm = TRUE))
  
  if( sum_only == TRUE ){
    
    df <- df %>% select(-starts_with("app_"))
    
  } else { df }
  
  if( prob == TRUE ){
    
    tmp_df <- readme_raw_data %>% 
      as.data.frame() %>% 
      tidytext::unnest_tokens(word, !!input) %>%
      as_tidytable() %>% 
      tidytable::count.(!!id, name = "n_words")
    
  } else { df }
  
  df
  
}

detect_database_sw <- function(df, id, input, sum_only = FALSE, prob = FALSE){
  
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytext")
  library("tidytable")
  
  db_general <- get_dictionary_terms(main_type = "Database", sub_type = F)
  db_engine <- get_dictionary_terms(main_type = "Database Engines/Servers")
  db_graph <- get_dictionary_terms(sub_type = "Graph Databases")
  db_frontends <- get_dictionary_terms(sub_type = "Front-Ends")
  
  id <- enquo(id)
  input <- enquo(input)
  
  df <- df %>% 
    as_tidytable() %>% 
    tidytable::mutate.("{{input}}" := tolower({{ input }})) %>% 
    detect_types(!!id, !!input, db_general) %>% 
    detect_types(!!id, !!input, db_engine) %>% 
    detect_types(!!id, !!input, db_graph) %>% 
    detect_types(!!id, !!input, db_frontends) 
  
  df <- df %>% 
    as.data.frame() %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(db_all = sum(across(contains("db_")), na.rm = TRUE))
  
  if( sum_only == TRUE ){
    
    df <- df %>% select(-starts_with("db_"))
    
  } else { df }
  
  if( prob == TRUE ){
    
    tmp_df <- readme_raw_data %>% 
      as.data.frame() %>% 
      tidytext::unnest_tokens(word, !!input) %>%
      as_tidytable() %>% 
      tidytable::count.(!!id, name = "n_words")
    
  } else { df }
  
  df
  
}


detect_ai_sw <- function(df, id, input, sum_only = FALSE, prob = FALSE){
  
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytext")
  library("tidytable")
  
  ai <- get_dictionary_terms(sub_type = "Artificial Intelligence")
  
  id <- enquo(id)
  input <- enquo(input)
  
  df <- df %>% 
    as_tidytable() %>% 
    tidytable::mutate.("{{input}}" := tolower({{ input }})) %>% 
    detect_types(!!id, !!input, ai)  
  
  # df <- df %>% 
  #   as.data.frame() %>% 
  #   dplyr::rowwise() %>% 
  #   dplyr::mutate(utility_all = sum(across(contains("util_")), na.rm = TRUE))
  
  # if( sum_only == TRUE ){
  #   
  #   df <- df %>% select(-starts_with("util_"))
  #   
  # } else { df }
  # 
  # if( prob == TRUE ){
  #   
  #   tmp_df <- readme_raw_data %>% 
  #     as.data.frame() %>% 
  #     tidytext::unnest_tokens(word, !!input) %>%
  #     as_tidytable() %>% 
  #     tidytable::count.(!!id, name = "n_words")
  #   
  # } else { df }
  
  df
  
}

detect_viz_sw <- function(df, id, input, sum_only = FALSE, prob = FALSE){
  
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytext")
  library("tidytable")
  
  viz <- get_dictionary_terms(sub_type = "Visualization")
  
  id <- enquo(id)
  input <- enquo(input)
  
  df <- df %>% 
    as_tidytable() %>% 
    tidytable::mutate.("{{input}}" := tolower({{ input }})) %>% 
    detect_types(!!id, !!input, viz)  
  
  # df <- df %>% 
  #   as.data.frame() %>% 
  #   dplyr::rowwise() %>% 
  #   dplyr::mutate(utility_all = sum(across(contains("util_")), na.rm = TRUE))
  
  # if( sum_only == TRUE ){
  #   
  #   df <- df %>% select(-starts_with("util_"))
  #   
  # } else { df }
  # 
  # if( prob == TRUE ){
  #   
  #   tmp_df <- readme_raw_data %>% 
  #     as.data.frame() %>% 
  #     tidytext::unnest_tokens(word, !!input) %>%
  #     as_tidytable() %>% 
  #     tidytable::count.(!!id, name = "n_words")
  #   
  # } else { df }
  
  df
  
}