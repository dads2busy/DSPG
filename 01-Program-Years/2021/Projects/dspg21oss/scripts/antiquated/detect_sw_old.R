
# TODO = finish detect_application_sw

# this function classifies (or will eventually) all application software 

detect_application_sw <- function(df, input){
  
  # load required packages 
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytable")
  
  # load dictionary of all software terms 
  # https://github.com/DSPG-Young-Scholars-Program/dspg21oss/blob/main/docs/oss_software_types%20-%20dictionary.csv 
  # there are three levels of software categories: summary_types, main_types and sub_types 
  # we only need to match strings for the main_types and sub_types categories 
  # all of the summary_types will be aggregated in the last step so its more computationally efficient 
  
  setwd("~/git/dspg21oss/docs/")
  software_types <- readr::read_csv("oss_software_types - dictionary.csv") 
  
  # blockchain    
  blockchain_gen <- software_types %>% filter(main_type == "Blockchain" & is.na(sub_type))
  blockchain_gen <- paste(c("\\b(?i)(zcx", na.omit(blockchain_gen$terms), "zcx)\\b"), collapse = "|")
  blockchain_crypto <- software_types %>% filter(sub_type == "Crytocurrency")
  blockchain_crypto <- paste(c("\\b(?i)(zcx", na.omit(blockchain_crypto$terms), "zcx)\\b"), collapse = "|")
  
  #communications 
  communications <- software_types %>% filter(main_type == "Communications")
  communications <- paste(c("\\b(?i)(zcx", communications$terms, "zcx)\\b"), collapse = "|")
  
  #comm_
  
  # database management 
  database_gen <- software_types %>% filter(main_type == "Database" & is.na(sub_type))
  database_gen <- paste(c("\\b(?i)(zcx", na.omit(database_gen$terms), "zcx)\\b"), collapse = "|")
  database_frontends <- software_types %>% filter(sub_type == "Front-Ends")
  database_frontends <- paste(c("\\b(?i)(zcx", na.omit(database_frontends$terms), "zcx)\\b"), collapse = "|")
  database_servers <- software_types %>% filter(sub_type == "Database Engines/Servers")
  database_servers <- paste(c("\\b(?i)(zcx", na.omit(database_servers$terms), "zcx)\\b"), collapse = "|")
  
  # desktop environments 
  #desktop_
  
  # ...
  
  # text editors 
  #texted_
  textedit_vivim <- software_types %>% filter(sub_type == "Vi/Vim")
  textedit_vivim <- paste(c("\\b(?i)(zcx", na.omit(textedit_vivim$terms), "zcx)\\b"), collapse = "|")
  
  
  # classifier 
  df <- df %>% 
    as_tidytable() %>% 
    tidytable::mutate.("{{input}}" := tolower({{ input }})) %>% 
    
    # blockchain 
    tidytable::mutate.(blockchain_gen = ifelse(stringr::str_detect(string = {{ input }}, blockchain_gen), 1, 0)) %>% 
    tidytable::mutate.(blockchain_crypto = ifelse(stringr::str_detect(string = {{ input }}, blockchain_crypto), 1, 0)) %>% 
    
    # database 
    tidytable::mutate.(database_gen = ifelse(stringr::str_detect(string = {{ input }}, database_gen), 1, 0)) %>%
    tidytable::mutate.(database_frontends = ifelse(stringr::str_detect(string = {{ input }}, database_frontends), 1, 0)) %>%
    tidytable::mutate.(database_servers = ifelse(stringr::str_detect(string = {{ input }}, database_servers), 1, 0)) %>% 
    
    # text editors
    tidytable::mutate.(textedit_vivim = ifelse(stringr::str_detect(string = {{ input }}, textedit_vivim), 1, 0)) 
  
  # aggregate across all sub_types to count the summary_types 
  df <- df %>% 
    as.data.frame() %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(blockchain_all = sum(across(contains("blockchain_")), na.rm = TRUE)) %>%
    dplyr::mutate(database_all = sum(across(contains("database_")), na.rm = TRUE)) %>%
    # ... 
    dplyr::mutate(textedit_all = sum(across(contains("textedit_")), na.rm = TRUE))  
  
  df
  
}

# this function detects system software like bitcoin, etherium and vechain 

detect_blockchain_sw <- function(df, input){
  
  # load required packages 
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytable")
  
  # load dictionary of all software terms 
  # https://github.com/DSPG-Young-Scholars-Program/dspg21oss/blob/main/docs/oss_software_types%20-%20dictionary.csv 
  # there are three levels of software categories: summary_types, main_types and sub_types 
  # we only need to match strings for the main_types and sub_types categories 
  # all of the summary_types will be aggregated in the last step so its more computationally efficient 
  
  setwd("~/git/dspg21oss/docs/")
  software_types <- readr::read_csv("oss_software_types - dictionary.csv") 
  
  # blockchain    
  blockchain_gen <- software_types %>% filter(main_type == "Blockchain" & is.na(sub_type))
  blockchain_gen <- paste(c("\\b(?i)(zcx", na.omit(blockchain_gen$terms), "zcx)\\b"), collapse = "|")
  blockchain_crypto <- software_types %>% filter(sub_type == "Crytocurrency")
  blockchain_crypto <- paste(c("\\b(?i)(zcx", na.omit(blockchain_crypto$terms), "zcx)\\b"), collapse = "|")
  
  # classifier 
  df <- df %>% 
    as_tidytable() %>% 
    tidytable::mutate.("{{input}}" := tolower({{ input }})) %>% 
    tidytable::mutate.(blockchain_gen = ifelse(stringr::str_detect(string = {{ input }}, blockchain_gen), 1, 0)) %>% 
    tidytable::mutate.(blockchain_crypto = ifelse(stringr::str_detect(string = {{ input }}, blockchain_crypto), 1, 0)) 
  
  # aggregate across all sub_types to count the summary_types 
  df <- df %>% 
    as.data.frame() %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(blockchain_all = sum(across(contains("blockchain_")), na.rm = TRUE)) 
  
  df
  
}


# this function detects system software such as postgresql, nosql, mysql, etc.

detect_database_sw <- function(df, input){
  
  # load required packages 
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytable")
  
  # load dictionary of all software terms 
  # https://github.com/DSPG-Young-Scholars-Program/dspg21oss/blob/main/docs/oss_software_types%20-%20dictionary.csv 
  # there are three levels of software categories: summary_types, main_types and sub_types 
  # we only need to match strings for the main_types and sub_types categories 
  # all of the summary_types will be aggregated in the last step so its more computationally efficient 
  
  setwd("~/git/dspg21oss/docs/")
  software_types <- readr::read_csv("oss_software_types - dictionary.csv") 
  
  # database management 
  database_gen <- software_types %>% filter(main_type == "Database" & is.na(sub_type))
  database_gen <- paste(c("\\b(?i)(zcx", na.omit(database_gen$terms), "zcx)\\b"), collapse = "|")
  database_frontends <- software_types %>% filter(sub_type == "Front-Ends")
  database_frontends <- paste(c("\\b(?i)(zcx", na.omit(database_frontends$terms), "zcx)\\b"), collapse = "|")
  database_servers <- software_types %>% filter(sub_type == "Database Engines/Servers")
  database_servers <- paste(c("\\b(?i)(zcx", na.omit(database_servers$terms), "zcx)\\b"), collapse = "|")
  
  # classifier 
  df <- df %>% 
    as_tidytable() %>% 
    tidytable::mutate.("{{input}}" := tolower({{ input }})) %>% 
    tidytable::mutate.(database_gen = ifelse(stringr::str_detect(string = {{ input }}, database_gen), 1, 0)) %>%
    tidytable::mutate.(database_frontends = ifelse(stringr::str_detect(string = {{ input }}, database_frontends), 1, 0)) %>%
    tidytable::mutate.(database_servers = ifelse(stringr::str_detect(string = {{ input }}, database_servers), 1, 0)) 
  
  # aggregate across all sub_types to count the summary_types 
  df <- df %>% 
    as.data.frame() %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(database_all = sum(across(contains("database_")), na.rm = TRUE)) 
  
  df
  
}


# this function detects the top-10 programming languages (rankings based on sourceforge projects) 

detect_programming_sw <- function(df, input){
  
  # load required packages 
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytable")
  
  # load dictionary of all software terms 
  # https://github.com/DSPG-Young-Scholars-Program/dspg21oss/blob/main/docs/oss_software_types%20-%20dictionary.csv 
  # there are three levels of software categories: summary_types, main_types and sub_types 
  # we only need to match strings for the main_types and sub_types categories 
  # all of the summary_types will be aggregated in the last step so its more computationally efficient 
  
  setwd("~/git/dspg21oss/docs/")
  software_types <- readr::read_csv("oss_software_types - dictionary.csv") 
  
  # utility software     
  program_gen <- software_types %>% filter(summary_type == "Programming" & is.na(main_type))
  program_gen <- paste(c("\\b(?i)(zcx", na.omit(program_gen$terms), "zcx)\\b"), collapse = "|") 
  
  java <- software_types %>% filter(sub_type == "Java")
  java <- paste(c("\\b(?i)(zcx", na.omit(java$terms), "zcx)\\b"), collapse = "|")
  c_plus_plus <- software_types %>% filter(sub_type == "C++")
  c_plus_plus <- paste(c("\\b(?i)(zcx", na.omit(c_lang$terms), "zcx)\\b"), collapse = "|")
  php <- software_types %>% filter(sub_type == "PHP")
  php <- paste(c("\\b(?i)(zcx", na.omit(php$terms), "zcx)\\b"), collapse = "|")
  c_lang <- software_types %>% filter(sub_type == "C")
  c_lang <- paste(c("\\b(?i)(zcx", na.omit(c_lang$terms), "zcx)\\b"), collapse = "|")
  c_sharp <- software_types %>% filter(sub_type == "C#")
  c_sharp <- paste(c("\\b(?i)(zcx", na.omit(c_sharp$terms), "zcx)\\b"), collapse = "|")
  python <- software_types %>% filter(sub_type == "Python")
  python <- paste(c("\\b(?i)(zcx", na.omit(python$terms), "zcx)\\b"), collapse = "|")
  javascript <- software_types %>% filter(sub_type == "JavaScript")
  javascript <- paste(c("\\b(?i)(zcx", na.omit(javascript$terms), "zcx)\\b"), collapse = "|")
  perl <- software_types %>% filter(sub_type == "PHP")
  perl <- paste(c("\\b(?i)(zcx", na.omit(perl$terms), "zcx)\\b"), collapse = "|")
  unix <- software_types %>% filter(sub_type == "Unix Shell")
  unix <- paste(c("\\b(?i)(zcx", na.omit(unix$terms), "zcx)\\b"), collapse = "|")
  virtual_basic <- software_types %>% filter(sub_type == "Virtual Basic .NET")
  virtual_basic <- paste(c("\\b(?i)(zcx", na.omit(virtual_basic$terms), "zcx)\\b"), collapse = "|")
  
  # classifier 
  df <- df %>% 
    as_tidytable() %>% 
    tidytable::mutate.("{{input}}" := tolower({{ input }})) %>% 
    tidytable::mutate.(java = ifelse(stringr::str_detect(string = {{ input }}, java), 1, 0)) %>% 
    tidytable::mutate.(c_plus_plus = ifelse(stringr::str_detect(string = {{ input }}, c_plus_plus), 1, 0)) %>% 
    tidytable::mutate.(php = ifelse(stringr::str_detect(string = {{ input }}, php), 1, 0)) %>% 
    tidytable::mutate.(c_lang = ifelse(stringr::str_detect(string = {{ input }}, c_lang), 1, 0)) %>% 
    tidytable::mutate.(c_sharp = ifelse(stringr::str_detect(string = {{ input }}, c_sharp), 1, 0)) %>% 
    tidytable::mutate.(python = ifelse(stringr::str_detect(string = {{ input }}, python), 1, 0)) %>% 
    tidytable::mutate.(javascript = ifelse(stringr::str_detect(string = {{ input }}, javascript), 1, 0)) %>% 
    tidytable::mutate.(perl = ifelse(stringr::str_detect(string = {{ input }}, perl), 1, 0)) %>% 
    tidytable::mutate.(unix = ifelse(stringr::str_detect(string = {{ input }}, unix), 1, 0)) %>% 
    tidytable::mutate.(virtual_basic = ifelse(stringr::str_detect(string = {{ input }}, virtual_basic), 1, 0)) 
  
  # aggregate across all sub_types to count the summary_types 
  df <- df %>% 
    as.data.frame() %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(utility_all = sum(across(contains("util_")), na.rm = TRUE)) 
  
  #if(sum_only == TRUE){
  #  df <- df %>% select(-starts_with("sys_"))
  #} else {
  #  df 
  #}
  
  df
  
}

# this function detects system software such as terminals and operating systems 

detect_system_sw <- function(df, input, sum_only = FALSE){
  
  # load required packages 
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytable")
  
  # load dictionary of all software terms 
  # https://github.com/DSPG-Young-Scholars-Program/dspg21oss/blob/main/docs/oss_software_types%20-%20dictionary.csv 
  # there are three levels of software categories: summary_types, main_types and sub_types 
  # we only need to match strings for the main_types and sub_types categories 
  # all of the summary_types will be aggregated in the last step so its more computationally efficient 
  
  setwd("~/git/dspg21oss/docs/")
  software_types <- readr::read_csv("oss_software_types - dictionary.csv") 
  
  # system software     
  sys_gen <- software_types %>% filter(summary_type == "System" & is.na(main_type))
  sys_gen <- paste(c("\\b(?i)(zcx", na.omit(sys_gen$terms), "zcx)\\b"), collapse = "|")
  
  # terminals 
  sys_terminals <- software_types %>% filter(main_type == "Terminals" & is.na(sub_type))
  sys_terminals <- paste(c("\\b(?i)(zcx", na.omit(sys_terminals$terms), "zcx)\\b"), collapse = "|")
  sys_xterminals <- software_types %>% filter(sub_type == "Terminal Emulators/X Terminals")
  sys_xterminals <- paste(c("\\b(?i)(zcx", na.omit(sys_xterminals$terms), "zcx)\\b"), collapse = "|")
  sys_serial <- software_types %>% filter(sub_type == "Serial")
  sys_serial <- paste(c("\\b(?i)(zcx", na.omit(sys_serial$terms), "zcx)\\b"), collapse = "|")
  sys_telnet <- software_types %>% filter(sub_type == "Telnet")
  sys_telnet <- paste(c("\\b(?i)(zcx", na.omit(sys_telnet$terms), "zcx)\\b"), collapse = "|")
  
  # operating systems 
  sys_os <- software_types %>% filter(main_type == "Operating Systems" & is.na(sub_type))
  sys_os <- paste(c("\\b(?i)(zcx", na.omit(sys_os$terms), "zcx)\\b"), collapse = "|")
  sys_windows <- software_types %>% filter(sub_type == "Windows")
  sys_windows <- paste(c("\\b(?i)(zcx", na.omit(sys_windows$terms), "zcx)\\b"), collapse = "|")
  sys_mac <- software_types %>% filter(sub_type == "Mac")
  sys_mac <- paste(c("\\b(?i)(zcx", na.omit(sys_mac$terms), "zcx)\\b"), collapse = "|")
  sys_linux <- software_types %>% filter(sub_type == "Linux")
  sys_linux <- paste(c("\\b(?i)(zcx", na.omit(sys_linux$terms), "zcx)\\b"), collapse = "|")
  sys_android <- software_types %>% filter(sub_type == "Android")
  sys_android <- paste(c("\\b(?i)(zcx", na.omit(sys_android$terms), "zcx)\\b"), collapse = "|")
  sys_solaris <- software_types %>% filter(sub_type == "Solaris")
  sys_solaris <- paste(c("\\b(?i)(zcx", na.omit(sys_solaris$terms), "zcx)\\b"), collapse = "|")
  sys_modern <- software_types %>% filter(sub_type == "Modern")
  sys_modern <- paste(c("\\b(?i)(zcx", na.omit(sys_modern$terms), "zcx)\\b"), collapse = "|")
  sys_bsd <- software_types %>% filter(sub_type == "BSD")
  sys_bsd <- paste(c("\\b(?i)(zcx", na.omit(sys_bsd$terms), "zcx)\\b"), collapse = "|")
  sys_handheld <- software_types %>% filter(sub_type == "Handheld/Embedded Operating Systems")
  sys_handheld <- paste(c("\\b(?i)(zcx", na.omit(sys_handheld$terms), "zcx)\\b"), collapse = "|")
  sys_emul_api <- software_types %>% filter(sub_type == "Emulation and API Compatability")
  sys_emul_api <- paste(c("\\b(?i)(zcx", na.omit(sys_emul_api$terms), "zcx)\\b"), collapse = "|")
  sys_virtualization <- software_types %>% filter(sub_type == "Virtualization")
  sys_virtualization <- paste(c("\\b(?i)(zcx", na.omit(sys_virtualization$terms), "zcx)\\b"), collapse = "|")
  sys_grouping <- software_types %>% filter(sub_type == "Grouping and Descriptive Categories")
  sys_grouping <- paste(c("\\b(?i)(zcx", na.omit(sys_grouping$terms), "zcx)\\b"), collapse = "|")
  sys_other_os <- software_types %>% filter(sub_type == "Other Operating Systems")
  sys_other_os <- paste(c("\\b(?i)(zcx", na.omit(sys_other_os$terms), "zcx)\\b"), collapse = "|")
  
  # classifier 
  df <- df %>% 
    as_tidytable() %>% 
    tidytable::mutate.("{{input}}" := tolower({{ input }})) %>% 
    # general 
    tidytable::mutate.(sys_gen = ifelse(stringr::str_detect(string = {{ input }}, sys_gen), 1, 0)) %>% 
    # terminals 
    tidytable::mutate.(sys_terminals = ifelse(stringr::str_detect(string = {{ input }}, sys_terminals), 1, 0)) %>% 
    tidytable::mutate.(sys_xterminals = ifelse(stringr::str_detect(string = {{ input }}, sys_xterminals), 1, 0)) %>% 
    tidytable::mutate.(sys_serial = ifelse(stringr::str_detect(string = {{ input }}, sys_serial), 1, 0)) %>% 
    tidytable::mutate.(sys_telnet = ifelse(stringr::str_detect(string = {{ input }}, sys_telnet), 1, 0)) %>% 
    # operating systems 
    tidytable::mutate.(sys_windows = ifelse(stringr::str_detect(string = {{ input }}, sys_windows), 1, 0)) %>% 
    tidytable::mutate.(sys_mac = ifelse(stringr::str_detect(string = {{ input }}, sys_mac), 1, 0)) %>% 
    tidytable::mutate.(sys_linux = ifelse(stringr::str_detect(string = {{ input }}, sys_linux), 1, 0)) %>% 
    tidytable::mutate.(sys_android = ifelse(stringr::str_detect(string = {{ input }}, sys_android), 1, 0)) %>% 
    tidytable::mutate.(sys_solaris = ifelse(stringr::str_detect(string = {{ input }}, sys_solaris), 1, 0)) %>% 
    tidytable::mutate.(sys_modern = ifelse(stringr::str_detect(string = {{ input }}, sys_modern), 1, 0)) %>% 
    tidytable::mutate.(sys_bsd = ifelse(stringr::str_detect(string = {{ input }}, sys_bsd), 1, 0)) %>% 
    tidytable::mutate.(sys_handheld = ifelse(stringr::str_detect(string = {{ input }}, sys_handheld), 1, 0)) %>% 
    tidytable::mutate.(sys_emul_api = ifelse(stringr::str_detect(string = {{ input }}, sys_emul_api), 1, 0)) %>% 
    tidytable::mutate.(sys_virtualization = ifelse(stringr::str_detect(string = {{ input }}, sys_virtualization), 1, 0)) %>% 
    tidytable::mutate.(sys_grouping = ifelse(stringr::str_detect(string = {{ input }}, sys_grouping), 1, 0)) %>% 
    tidytable::mutate.(sys_other_os = ifelse(stringr::str_detect(string = {{ input }}, sys_other_os), 1, 0)) 

  # aggregate across all sub_types to count the summary_types 
  df <- df %>% 
    as.data.frame() %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(system_all = sum(across(contains("sys_")), na.rm = TRUE)) 
  
  if(sum_only == TRUE){
    df <- df %>% select(-starts_with("sys_"))
  } else {
    df 
  }
  
  df
  
}

# this function detects utility software such as password managers, anti-virus, etc. 

detect_utility_sw <- function(df, input, sum_only = FALSE){
  
  # load required packages 
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytable")
  
  # load dictionary of all software terms 
  # https://github.com/DSPG-Young-Scholars-Program/dspg21oss/blob/main/docs/oss_software_types%20-%20dictionary.csv 
  # there are three levels of software categories: summary_types, main_types and sub_types 
  # we only need to match strings for the main_types and sub_types categories 
  # all of the summary_types will be aggregated in the last step so its more computationally efficient 
  
  setwd("~/git/dspg21oss/docs/")
  software_types <- readr::read_csv("oss_software_types - dictionary.csv") 
  
  # utility software     
  util_gen <- software_types %>% filter(summary_type == "Utility" & is.na(main_type))
  util_gen <- paste(c("\\b(?i)(zcx", na.omit(util_gen$terms), "zcx)\\b"), collapse = "|")
  util_security <- software_types %>% filter(main_type == "Security" & is.na(sub_type))
  util_security <- paste(c("\\b(?i)(zcx", na.omit(util_security$terms), "zcx)\\b"), collapse = "|")
  util_cryptography <- software_types %>% filter(sub_type == "Cryptography")
  util_cryptography <- paste(c("\\b(?i)(zcx", na.omit(util_cryptography$terms), "zcx)\\b"), collapse = "|")
  util_pwdmngr <- software_types %>% filter(sub_type == "	Password Manager")
  util_pwdmngr <- paste(c("\\b(?i)(zcx", na.omit(util_pwdmngr$terms), "zcx)\\b"), collapse = "|")
  util_antimalware <- software_types %>% filter(sub_type == "Anti-Malware")
  util_antimalware <- paste(c("\\b(?i)(zcx", na.omit(util_antimalware$terms), "zcx)\\b"), collapse = "|")
  util_antivirus <- software_types %>% filter(sub_type == "Anti-Virus")
  util_antivirus <- paste(c("\\b(?i)(zcx", na.omit(util_antivirus$terms), "zcx)\\b"), collapse = "|")
  util_antispam <- software_types %>% filter(sub_type == "Anti-Spam")
  util_antispam <- paste(c("\\b(?i)(zcx", na.omit(util_antispam$terms), "zcx)\\b"), collapse = "|")
  
  # classifier 
  df <- df %>% 
    as_tidytable() %>% 
    tidytable::mutate.("{{input}}" := tolower({{ input }})) %>% 
    tidytable::mutate.(util_gen = ifelse(stringr::str_detect(string = {{ input }}, util_gen), 1, 0)) %>% 
    tidytable::mutate.(util_security = ifelse(stringr::str_detect(string = {{ input }}, util_security), 1, 0)) %>% 
    tidytable::mutate.(util_cryptography = ifelse(stringr::str_detect(string = {{ input }}, util_cryptography), 1, 0)) %>% 
    tidytable::mutate.(util_pwdmngr = ifelse(stringr::str_detect(string = {{ input }}, util_pwdmngr), 1, 0)) %>% 
    tidytable::mutate.(util_antimalware = ifelse(stringr::str_detect(string = {{ input }}, util_antimalware), 1, 0)) %>% 
    tidytable::mutate.(util_antivirus = ifelse(stringr::str_detect(string = {{ input }}, util_antivirus), 1, 0)) %>% 
    tidytable::mutate.(util_antispam = ifelse(stringr::str_detect(string = {{ input }}, util_antispam), 1, 0)) 
  
  # aggregate across all sub_types to count the summary_types 
  df <- df %>% 
    as.data.frame() %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(utility_all = sum(across(contains("util_")), na.rm = TRUE))
  
  if(sum_only == TRUE){
    df <- df %>% select(-starts_with("util_"))
  } else {
    df 
  }
  
  df
  
}


































