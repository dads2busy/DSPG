
# get_dictionary_terms pulls in the oss_software_types dictionary terms 

get_dictionary_terms <- function(summary_type, main_type, sub_type){
  
  library("dplyr")
  library("readr")
  library("tidyr")
  
  if( missing(main_type) & missing(sub_type) ){ 
    
    # this is to pull all of the terms for a summary_type 
    
    summary_type <- enquo(summary_type)
    setwd("~/git/dspg21oss/docs/")
    software_types <- readr::read_csv("oss_software_types - dictionary - co.csv", col_types = cols()) %>% 
      unnest_legacy(terms = strsplit(terms, "\\|")) 
    software_terms <- software_types %>% 
      filter(summary_type == {{ summary_type }})
    software_terms <- na.omit(software_terms$terms)
    software_terms
    
  } else if (missing(summary_type) & missing(sub_type) ){
    
    # this is to pull all of the terms for a main_type  
    
    main_type <- enquo(main_type)
    setwd("~/git/dspg21oss/docs/")
    software_types <- readr::read_csv("oss_software_types - dictionary - co.csv", col_types = cols()) %>% 
      unnest_legacy(terms = strsplit(terms, "\\|")) 
    software_terms <- software_types %>% 
      filter(main_type == {{ main_type }}) 
    software_terms <- na.omit(software_terms$terms)
    software_terms
    
  } else if (missing(summary_type) & missing(main_type)) {
    
    sub_type <- enquo(sub_type)
    setwd("~/git/dspg21oss/docs/")
    software_types <- readr::read_csv("oss_software_types - dictionary - co.csv", col_types = cols()) %>% 
      unnest_legacy(terms = strsplit(terms, "\\|")) 
    software_terms <- software_types %>% 
      filter(sub_type == {{ sub_type }})
    software_terms <- na.omit(software_terms$terms)
    software_terms
    
  } else if ( missing(summary_type) & sub_type == FALSE ){
    
    # this is to pull all of the terms for main_type general category   
    
    main_type <- enquo(main_type)
    setwd("~/git/dspg21oss/docs/")
    software_types <- readr::read_csv("oss_software_types - dictionary - co.csv", col_types = cols()) %>% 
      unnest_legacy(terms = strsplit(terms, "\\|")) 
    software_terms <- software_types %>% 
      filter(main_type == {{ main_type }} & is.na(sub_type)) 
    software_terms <- na.omit(software_terms$terms)
    software_terms
    
  } else if ( main_type == FALSE & sub_type == FALSE ){ 
    
    # this is to pull all of the terms for a summary_type when all else is NA  
    
    summary_type <- enquo(summary_type)
    setwd("~/git/dspg21oss/docs/")
    software_types <- readr::read_csv("oss_software_types - dictionary - co.csv", col_types = cols()) %>% 
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



# get_dictionary_terms pulls in the oss_software_types dictionary terms 

get_dictionary_terms <- function(summary_type, main_type, sub_type){
  
  library("dplyr")
  library("readr")
  library("tidyr")
  
  if( missing(main_type) & missing(sub_type) ){ 
    
    # this is to pull all of the terms for a summary_type 
    
    summary_type <- enquo(summary_type)
    setwd("~/git/dspg21oss/docs/")
    software_types <- readr::read_csv("oss_software_types - dictionary - sz.csv", col_types = cols()) %>% 
      unnest_legacy(terms = strsplit(terms, "\\|")) 
    software_terms <- software_types %>% 
      filter(summary_type == {{ summary_type }})
    software_terms <- na.omit(software_terms$terms)
    software_terms
    
  } else if (missing(summary_type) & missing(sub_type) ){
    
    # this is to pull all of the terms for a main_type  
    
    main_type <- enquo(main_type)
    setwd("~/git/dspg21oss/docs/")
    software_types <- readr::read_csv("oss_software_types - dictionary - sz.csv", col_types = cols()) %>% 
      unnest_legacy(terms = strsplit(terms, "\\|")) 
    software_terms <- software_types %>% 
      filter(main_type == {{ main_type }}) 
    software_terms <- na.omit(software_terms$terms)
    software_terms
    
  } else if (missing(summary_type) & missing(main_type)) {
    
    sub_type <- enquo(sub_type)
    setwd("~/git/dspg21oss/docs/")
    software_types <- readr::read_csv("oss_software_types - dictionary - sz.csv", col_types = cols()) %>% 
      unnest_legacy(terms = strsplit(terms, "\\|")) 
    software_terms <- software_types %>% 
      filter(sub_type == {{ sub_type }})
    software_terms <- na.omit(software_terms$terms)
    software_terms
    
  } else if ( missing(summary_type) & sub_type == FALSE ){
    
    # this is to pull all of the terms for main_type general category   
    
    main_type <- enquo(main_type)
    setwd("~/git/dspg21oss/docs/")
    software_types <- readr::read_csv("oss_software_types - dictionary - sz.csv", col_types = cols()) %>% 
      unnest_legacy(terms = strsplit(terms, "\\|")) 
    software_terms <- software_types %>% 
      filter(main_type == {{ main_type }} & is.na(sub_type)) 
    software_terms <- na.omit(software_terms$terms)
    software_terms
    
  } else if ( main_type == FALSE & sub_type == FALSE ){ 
    
    # this is to pull all of the terms for a summary_type when all else is NA  
    
    summary_type <- enquo(summary_type)
    setwd("~/git/dspg21oss/docs/")
    software_types <- readr::read_csv("oss_software_types - dictionary.csv", col_types = cols()) %>% 
      unnest_legacy(terms = strsplit(terms, "\\|")) 
    software_terms <- software_types %>% 
      filter(is.na(main_type) & is.na(sub_type))
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

# detect_statprog_sw classifies statistical programming languages  

detect_prog_stat_sw <- function(df, id, input, sum_only = FALSE, prob = FALSE){
  
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytext")
  library("tidytable")
  
  # load dictionaries for top-50 languages based on: 
  # https://madnight.github.io/githut/#/pull_requests/2021
  
  # top statistical languages 
  prog_python <- get_dictionary_terms(sub_type = "Python")
  prog_scala <- get_dictionary_terms(sub_type = "Scala")
  prog_rlang <- get_dictionary_terms(sub_type = "R")
  prog_matlab <- get_dictionary_terms(sub_type = "MATLAB")
  prog_stata <- get_dictionary_terms(sub_type = "Stata")
  prog_sas <- get_dictionary_terms(sub_type = "SAS")
  prog_julia <- get_dictionary_terms(sub_type = "Julia")
  
  id <- enquo(id)
  input <- enquo(input)
  
  df <- df %>% 
    as_tidytable() %>% 
    tidytable::mutate.("{{input}}" := tolower({{ input }})) %>% 
    detect_types(!!id, !!input, prog_python) %>% 
    detect_types(!!id, !!input, prog_scala) %>% 
    detect_types(!!id, !!input, prog_rlang) %>% 
    detect_types(!!id, !!input, prog_matlab) %>% 
    detect_types(!!id, !!input, prog_stata) %>% 
    detect_types(!!id, !!input, prog_sas) %>% 
    detect_types(!!id, !!input, prog_julia) 
  
  df <- df %>% 
    as.data.frame() %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(prog_stat_all = sum(across(contains("prog_")), na.rm = TRUE))
  
  if( sum_only == TRUE ){
    
    df <- df %>% select(-starts_with("prog_"))
    
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

# detect_prog_web_sw classifies web, application and gaming programming languages  

detect_prog_web_sw <- function(df, id, input, sum_only = FALSE, prob = FALSE){
  
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytext")
  library("tidytable")
  
  # load dictionaries for top-50 languages based on: 
  # https://madnight.github.io/githut/#/pull_requests/2021
  
  # top statistical languages 
  prog_javascript <- get_dictionary_terms(sub_type = "JavaScript")
  prog_java <- get_dictionary_terms(sub_type = "Java")
  prog_typescript <- get_dictionary_terms(sub_type = "TypeScript")
  prog_php <- get_dictionary_terms(sub_type = "PHP")
  prog_html <- get_dictionary_terms(sub_type = "HTML")
  prog_css <- get_dictionary_terms(sub_type = "CSS")
  prog_dart <- get_dictionary_terms(sub_type = "Dart") # from google 
  prog_kotlin <- get_dictionary_terms(sub_type = "Kotlin")
  prog_lua <- get_dictionary_terms(sub_type = "Lua")
  prog_coffeescript <- get_dictionary_terms(sub_type = "CoffeeScript")
  prog_dm <- get_dictionary_terms(sub_type = "DM") # used for games 
  prog_erlang <- get_dictionary_terms(sub_type = "Erlang") # ecommerce, banking, IMing
  prog_elixer <- get_dictionary_terms(sub_type = "Elixir") # used with erlang 
  prog_jsonnet <- get_dictionary_terms(sub_type = "Jsonnet") # structures json data 
  prog_elm <- get_dictionary_terms(sub_type = "Elm")
  prog_webassembly <- get_dictionary_terms(sub_type = "WebAssembly")
  prog_purescript <- get_dictionary_terms(sub_type = "PureScript")
  
  id <- enquo(id)
  input <- enquo(input)
  
  df <- df %>% 
    as_tidytable() %>% 
    tidytable::mutate.("{{input}}" := tolower({{ input }})) %>% 
    detect_types(!!id, !!input, prog_javascript) %>% 
    detect_types(!!id, !!input, prog_java) %>% 
    detect_types(!!id, !!input, prog_typescript) %>% 
    detect_types(!!id, !!input, prog_php) %>% 
    detect_types(!!id, !!input, prog_html) %>% 
    detect_types(!!id, !!input, prog_css) %>% 
    detect_types(!!id, !!input, prog_dart) %>% 
    detect_types(!!id, !!input, prog_kotlin) %>% 
    detect_types(!!id, !!input, prog_lua) %>% 
    detect_types(!!id, !!input, prog_coffeescript) %>% 
    detect_types(!!id, !!input, prog_dm) %>% 
    detect_types(!!id, !!input, prog_erlang) %>% 
    detect_types(!!id, !!input, prog_elixer) %>% 
    detect_types(!!id, !!input, prog_jsonnet) %>% 
    detect_types(!!id, !!input, prog_elm) %>% 
    detect_types(!!id, !!input, prog_webassembly) %>% 
    detect_types(!!id, !!input, prog_purescript) 
  
  df <- df %>% 
    as.data.frame() %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(prog_web_all = sum(across(contains("prog_")), na.rm = TRUE))
  
  if( sum_only == TRUE ){
    
    df <- df %>% select(-starts_with("prog_"))
    
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

# detect_prog_gen_sw classifies general and multipurpuse use programming languages

detect_prog_gen_sw <- function(df, id, input, sum_only = FALSE, prob = FALSE){
  
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytext")
  library("tidytable")
  
  # load dictionaries for top-50 languages based on: 
  # https://madnight.github.io/githut/#/pull_requests/2021
  
  # top statistical languages 
  #prog_general <- get_dictionary_terms(summary_type = "Programming")
  prog_go <- get_dictionary_terms(sub_type = "Go")
  prog_ruby <- get_dictionary_terms(sub_type = "Ruby")
  prog_cpp <- get_dictionary_terms(sub_type = "C++")
  prog_csharp <- get_dictionary_terms(sub_type = "C#")
  prog_clang <- get_dictionary_terms(sub_type = "C")
  prog_objc <- get_dictionary_terms(sub_type = "Objective-C")
  prog_objcpp <- get_dictionary_terms(sub_type = "Objective-C++")
  prog_rust <- get_dictionary_terms(sub_type = "Rust")
  prog_perl <- get_dictionary_terms(sub_type = "Perl")
  prog_swift <- get_dictionary_terms(sub_type = "Swift") # from apple 
  prog_haskell <- get_dictionary_terms(sub_type = "Haskell")
  prog_groovy <- get_dictionary_terms(sub_type = "Groovy") 
  prog_clojure <- get_dictionary_terms(sub_type = "Clojure") 
  prog_ocaml <- get_dictionary_terms(sub_type = "OCaml")
  prog_dotnet <- get_dictionary_terms(sub_type = ".NET")
  prog_vbdotnet <- get_dictionary_terms(sub_type = "Virtual Basic .NET")
  prog_fsharp <- get_dictionary_terms(sub_type = "F#")
  prog_fortran <- get_dictionary_terms(sub_type = "Fortran") # from ibm 
  prog_commonlisp <- get_dictionary_terms(sub_type = "Common Lisp")
  prog_coq <- get_dictionary_terms(sub_type = "Coq") # formal proof mgmt system
  
  id <- enquo(id)
  input <- enquo(input)
  
  df <- df %>% 
    as_tidytable() %>% 
    tidytable::mutate.("{{input}}" := tolower({{ input }})) %>% 
    #detect_types(!!id, !!input, prog_general) %>% 
    detect_types(!!id, !!input, prog_go) %>% 
    detect_types(!!id, !!input, prog_ruby) %>% 
    detect_types(!!id, !!input, prog_cpp) %>% 
    detect_types(!!id, !!input, prog_csharp) %>% 
    detect_types(!!id, !!input, prog_clang) %>% 
    detect_types(!!id, !!input, prog_objc) %>% 
    detect_types(!!id, !!input, prog_objcpp) %>% 
    detect_types(!!id, !!input, prog_rust) %>% 
    detect_types(!!id, !!input, prog_perl) %>% 
    detect_types(!!id, !!input, prog_swift) %>% 
    detect_types(!!id, !!input, prog_haskell) %>% 
    detect_types(!!id, !!input, prog_groovy) %>% 
    detect_types(!!id, !!input, prog_clojure) %>% 
    detect_types(!!id, !!input, prog_ocaml) %>% 
    detect_types(!!id, !!input, prog_dotnet) %>% 
    detect_types(!!id, !!input, prog_vbdotnet) %>% 
    detect_types(!!id, !!input, prog_fsharp) %>% 
    detect_types(!!id, !!input, prog_fortran) %>% 
    detect_types(!!id, !!input, prog_commonlisp) %>% 
    detect_types(!!id, !!input, prog_coq) 
  
  df <- df %>% 
    as.data.frame() %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(prog_gen_all = sum(across(contains("prog_")), na.rm = TRUE))
  
  if( sum_only == TRUE ){
    
    df <- df %>% select(-starts_with("prog_"))
    
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