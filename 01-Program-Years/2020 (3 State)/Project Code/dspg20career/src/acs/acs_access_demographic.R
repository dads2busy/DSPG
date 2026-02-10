library(tidycensus)
library(maps)
library(data.table)
library(extrafont)

vet_age_m <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
                                    variables = c(total = "B21001_001",
                                                  vet_total = "B21001_002",
                                                  nonvet_total = "B21001_003",
                                                  male_total = "B21001_004",
                                                  male_vet_total = "B21001_005",
                                                  male_nonvet_total = "B21001_006",
                                                  male_18_34_total = "B21001_007",
                                                  male_18_34_vet = "B21001_008",
                                                  male_18_34_nonvet = "B21001_009",
                                                  male_35_54_total = "B21001_010",
                                                  male_35_54_vet = "B21001_011",
                                                  male_35_54_nonvet = "B21001_012",
                                                  male_55_64_total = "B21001_013",
                                                  male_55_64_vet = "B21001_014",
                                                  male_55_64_nonvet = "B21001_015",
                                                  male_65_74_total = "B21001_016",
                                                  male_65_74_vet = "B21001_017",
                                                  male_65_74_nonvet = "B21001_018",
                                                  male_75plus_total = "B21001_019",
                                                  male_75plus_vet = "B21001_020",
                                                  male_75plus_nonvet = "B21001_021"),
                       year = 2018)

#filter to DC
vet_age_m <- vet_age_m %>%
  filter(NAME == "Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area") %>%
  select(-c('GEOID', 'NAME'))

vet_age_m <- mutate(vet_age_m, isnonvet = ifelse(grepl('nonvet', variable), 'Non-Veteran', 'Veteran'))

write_csv(vet_age_tbl, 'vet_age_tbl.csv')

vet_age_m <- read_csv('vet_age_tbl.csv')

vet_age_m %>%
  filter(variable == 'male_1834' | variable == 'male_3554' | variable == 'male_5564' | variable == 'male_6574' | variable == 'male_75plus') %>%
  ggplot(aes(isnonvet, estimate, fill = variable)) + 
  geom_bar(stat = 'identity', position = 'fill')

vet_age_m %>%
  filter(variable == 'male_1834' | variable == 'male_3554' | variable == 'male_5564' | variable == 'male_6574' | variable == 'male_75plus') %>%
  ggplot(aes(variable, pct, fill = isnonvet)) + 
  geom_bar(stat = 'identity', position = 'dodge')

vet_age_m %>%
  filter(variable == 'male_1834' | variable == 'male_3554' | variable == 'male_5564' | variable == 'male_6574' | variable == 'male_75plus') %>%
  ggplot(aes(variable, estimate, fill = isnonvet)) + 
  geom_bar(stat = 'identity', position = 'dodge')

#get female 
vet_age_f <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
                     variables = c(female_total = "B21001_022",
                                   female_vet_total = "B21001_023",
                                   female_nonvet_total = "B21001_024",
                                   female_18_34_total = "B21001_025",
                                   female_18_34_vet = "B21001_026",
                                   female_18_34_nonvet = "B21001_027",
                                   female_35_54_total = "B21001_028",
                                   female_35_54_vet = "B21001_029",
                                   female_35_54_nonvet = "B21001_030",
                                   female_55_64_total = "B21001_031",
                                   female_55_64_vet = "B21001_032",
                                   female_55_64_nonvet = "B21001_033",
                                   female_65_74_total = "B21001_034",
                                   female_65_74_vet = "B21001_035",
                                   female_65_74_nonvet = "B21001_036",
                                   female_75plus_total = "B21001_037",
                                   female_75plus_vet = "B21001_038",
                                  female_75plus_nonvet = "B21001_039"),
                     year = 2018)

#filter to DC
vet_age_f <- vet_age_f %>%
  filter(NAME == "Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area") %>%
  select(-c('GEOID', 'NAME'))

vet_age_f <- mutate(vet_age_f, isnonvet = ifelse(grepl('nonvet', variable), 'Non-Veteran', 'Veteran'))

write_csv(vet_age_f, 'vet_age_f.csv')

vet_age_f <- read_csv('vet_age_f.csv')

#vet_age_tbl <- mutate(vet_age_tbl, isnonvet = ifelse(grepl('nonvet', variable), 'Non-Veteran', 'Veteran'))

vet_age_f %>%
  filter(variable == 'female_1834' | variable == 'female_3554' | variable == 'female_5564' | 
           variable == 'female_6574' | variable == 'female_75plus') %>%
  ggplot(aes(isnonvet, estimate, fill = variable)) + 
  geom_bar(stat = 'identity', position = 'fill')

vet_age_f %>%
  filter(variable == 'female_1834' | variable == 'female_3554' | variable == 'female_5564' | 
           variable == 'female_6574' | variable == 'female_75plus') %>%
  ggplot(aes(variable, pct, fill = isnonvet)) + 
  geom_bar(stat = 'identity', position = 'dodge')

vet_age_f %>%
  filter(variable == 'female_1834' | variable == 'female_3554' | variable == 'female_5564' | 
           variable == 'female_6574' | variable == 'female_75plus') %>%
  ggplot(aes(variable, estimate, fill = isnonvet)) + 
  geom_bar(stat = 'identity', position = 'dodge')


vet_ed <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
                     variables = c(
                       total_vet_ed = "B21003_002",
                       vet_less_hs = "B21003_003",
                       vet_hs = "B21003_004",
                       vet_some_coll = "B21003_005",
                       vet_bachelor_plus = "B21003_006",
                       total_nonvet_ed = "B21003_007",
                       nonvet_less_hs = "B21003_008",
                       nonvet_hs = "B21003_009",
                       nonvet_some_coll = "B21003_010",
                       nonvet_bachelor_plus = "B21003_011"
                     ),
                     year = 2018)

#filter to DC
vet_ed <- vet_ed %>%
  filter(NAME == "Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area") %>%
  select(-c('GEOID', 'NAME'))

vet_ed <- mutate(vet_ed, isnonvet = ifelse(grepl('nonvet', variable), 'Non-Veteran', 'Veteran'))

write_csv(vet_ed, 'vet_ed.csv')

vet_ed <- read_csv('vet_ed.csv')
                                
vet_ed %>%
  mutate(variable = fct_relevel(variable, 
                            "less_hs", "hs", "some_college", 
                            "bachelor_plus")) %>%
  ggplot(aes(variable, pct, fill = isnonvet)) + 
  scale_fill_manual(values =c('#232D4B', '#E57200')) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_classic() + 
  xlab('') +
  ylab('Percent of Group Population') +
  labs(title = 'Educational Attainment by Veteran Status',
       subtitle = 'ACS 2018 5-Year Estimates',
       fill = 'Veteran Status') +
  theme(text = element_text(family=font)) +
  scale_x_discrete(labels= c('Less than High School', 'High School', 'Some College', 'Bachelor or More'))

vet_med_income <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area",
   variables = c(
     med_income_total = "B21004_001",
     med_income_vet = "B21004_002",
     med_inc_vet_male = "B21004_003",
     med_inc_vet_female = "B21004_004",
     med_inc_nonvet = "B21004_005",
     med_inc_nonvet_male = "B21004_006",
     med_inc_nonvet_female = "B21004_007"
   ),
   year = 2018)

#filter to DC
vet_med_income <- vet_med_income %>%
  filter(NAME == "Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area") %>%
  select(-c('GEOID', 'NAME'))

vet_med_income <- mutate(vet_med_income, isnonvet = ifelse(grepl('nonvet', variable), 'Non-Veteran', 'Veteran'),
                         vet_med_income, isfemale = ifelse(grepl('female', variable), 'Female', 'Male'))

font <- 'Franklin Gothic Medium'
vet_med_income %>%
  filter(variable == 'med_income_vet' | variable == 'med_inc_nonvet') %>%
  ggplot(aes(x = variable, y = estimate)) +
  geom_bar(stat = 'identity', fill = '#232D4B') +
  #geom_text(aes(label = estimate), vjust = -0.5) +
  theme_classic() + 
  xlab('') +
  ylab('Median Income (USD)') +
  scale_x_discrete(labels= c('Non-Veteran', 'Veteran')) +
  labs(title = 'ACS 2018 5-Year Estimates for Median Income (DC Metro Area)') +
  theme(text = element_text(family=font))

vet_lf <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area",
   variables = c(
     age_vet_emp_total = "B21005_001",
     total_18_34 = "B21005_002",
     total_18_34_vet = "B21005_003",
     total_18_34_vet_in_lf_tot = "B21005_004",
     total_18_34_vet_in_lf_emp = "B21005_005",
     total_18_34_vet_in_lf_unemp = "B21005_006",
     total_18_34_vet_not_in_lf = "B21005_007",
     total_18_34_nonvet = "B21005_008",
     total_18_34_nonvet_in_lf_tot = "B21005_009",
     total_18_34_nonvet_in_lf_emp = "B21005_010",
     total_18_34_nonvet_in_lf_unemp = "B21005_011",
     total_18_34_nonvet_not_in_lf = "B21005_012",
     total_35_54 = "B21005_013",
     total_35_54_vet = "B21005_014",
     total_35_54_vet_in_lf_tot = "B21005_015",
     total_35_54_vet_in_lf_emp = "B21005_016",
     total_35_54_vet_in_lf_unemp = "B21005_017",
     total_35_54_vet_not_in_lf = "B21005_018",
     total_35_54_nonvet = "B21005_019",
     total_35_54_nonvet_in_lf_tot = "B21005_020",
     total_35_54_nonvet_in_lf_emp = "B21005_021",
     total_35_54_nonvet_in_lf_unemp = "B21005_022",
     total_35_54_nonvet_not_in_lf = "B21005_023",
     total_55_64 = "B21005_024",
     total_55_64_vet = "B21005_025",
     total_55_64_vet_in_lf_tot = "B21005_026",
     total_55_64_vet_in_lf_emp = "B21005_027",
     total_55_64_vet_in_lf_unemp = "B21005_028",
     total_55_64_vet_not_in_lf = "B21005_029",
     total_55_64_nonvet = "B21005_030",
     total_55_64_nonvet_in_lf_tot = "B21005_031",
     total_55_64_nonvet_in_lf_emp = "B21005_032",
     total_55_64_nonvet_in_lf_unemp = "B21005_033",
     total_55_64_nonvet_not_in_lf = "B21005_034"
   ),
   year = 2018)

#filter to DC
vet_lf <- vet_lf %>%
  filter(NAME == "Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area") %>%
  select(-c('GEOID', 'NAME'))

vet_lf <- mutate(vet_lf, isnonvet = ifelse(grepl('nonvet', variable), 'Non-Veteran', 'Veteran'))

write_csv(vet_lf, 'vet_lf.csv')

vet_lf <- read_csv('vet_lf_condensed.csv')

vet_lf$variable <- as.factor(vet_lf$variable)

vet_lf %>%
  filter(isnonvet == 'Veteran') %>%
  ggplot(aes(fill = status, y = estimate, x = variable)) +
  geom_bar(position = 'dodge', stat = 'identity')

vet_lf %>%
  filter(isnonvet == 'Non-Veteran') %>%
  ggplot(aes(fill = status, y = estimate, x = variable)) +
  geom_bar(position = 'dodge', stat = 'identity')

vet_lf %>%
  filter(status == 'Employed') %>%
  ggplot(aes(fill = isnonvet, y = estimate, x = variable)) +
  geom_bar(position = 'dodge', stat = 'identity')

vet_lf %>%
  filter(status == 'Unemployed') %>%
  ggplot(aes(fill = isnonvet, y = estimate, x = variable)) +
  geom_bar(position = 'dodge', stat = 'identity')

vet_lf %>%
  filter(status == 'Employed') %>%
  ggplot(aes(fill = isnonvet, y = estimate, x = variable)) +
  geom_bar(position = 'dodge', stat = 'identity')

ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity")


#variable = 'med_income_vet' | variable = 'med_income_nonvet' | 
# sample <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
#                   variables = c(total = "B21001_001",
#                                 vet_total = "B21001_002",
#                                 nonvet_total = "B21001_003",
#                                 male_total = "B21001_004",
#                                 male_vet_total = "B21001_005",
#                                 male_nonvet_total = "B21001_006",
#                                 male_18_34_total = "B21001_007",
#                                 male_18_34_vet = "B21001_008",
#                                 male_18_34_nonvet = "B21001_009",
#                                 male_35_54_total = "B21001_010",
#                                 male_35_54_vet = "B21001_011",
#                                 male_35_54_nonvet = "B21001_012",
#                                 male_55_64_total = "B21001_013",
#                                 male_55_64_vet = "B21001_014",
#                                 male_55_64_nonvet = "B21001_015",
#                                 male_65_74_total = "B21001_016",
#                                 male_65_74_vet = "B21001_017",
#                                 male_65_74_nonvet = "B21001_018",
#                                 male_75plus_total = "B21001_019",
#                                 male_75plus_vet = "B21001_020",
#                                 male_75plus_nonvet = "B21001_021",
#                                 female_total = "B21001_022",
#                                 female_vet_total = "B21001_023",
#                                 female_nonvet_total = "B21001_024",
#                                 female_18_34_total = "B21001_025",
#                                 female_18_34_vet = "B21001_026",
#                                 female_18_34_nonvet = "B21001_027",
#                                 female_35_54_total = "B21001_028",
#                                 female_35_54_vet = "B21001_029",
#                                 female_35_54_nonvet = "B21001_030",
#                                 female_55_64_total = "B21001_031",
#                                 female_55_64_vet = "B21001_032",
#                                 female_55_64_nonvet = "B21001_033",
#                                 female_65_74_total = "B21001_034",
#                                 female_65_74_vet = "B21001_035",
#                                 female_65_74_nonvet = "B21001_036",
#                                 female_75plus_total = "B21001_037",
#                                 female_75plus_vet = "B21001_038",
#                                 female_75plus_nonvet = "B21001_039",
#                                 total_ed = "B21003_001",
#                                 total_vet_ed = "B21003_002",
#                                 vet_less_hs = "B21003_003",
#                                 vet_hs = "B21003_004",
#                                 vet_some_coll = "B21003_005",
#                                 vet_bachelor_plus = "B21003_006",
#                                 total_nonvet_ed = "B21003_002",
#                                 nonvet_less_hs = "B21003_008",
#                                 nonvet_hs = "B21003_009",
#                                 nonvet_some_coll = "B21003_010",
#                                 nonvet_bachelor_plus = "B21003_011",
#                                 med_income_total = "B21004_001",
#                                 med_income_vet = "B21004_002",
#                                 med_inc_vet_male = "B21004_003",
#                                 med_inc_vet_female = "B21004_004",
#                                 med_inc_nonvet = "B21004_005",
#                                 med_inc_nonvet_male = "B21004_006",
#                                 med_inc_nonvet_female = "B21004_007",
#                                 age_vet_emp_total = "B21005_001",
#                                 total_18_34 = "B21005_002",
#                                 total_18_34_vet = "B21005_003",
#                                 total_18_34_vet_in_lf_tot = "B21005_004",
#                                 total_18_34_vet_in_lf_emp = "B21005_005",
#                                 total_18_34_vet_in_lf_unemp = "B21005_006",
#                                 total_18_34_vet_not_in_lf = "B21005_007",
#                                 total_18_34_nonvet = "B21005_008",
#                                 total_18_34_nonvet_in_lf_tot = "B21005_009",
#                                 total_18_34_nonvet_in_lf_emp = "B21005_010",
#                                 total_18_34_nonvet_in_lf_unemp = "B21005_011",
#                                 total_18_34_nonvet_not_in_lf = "B21005_012",
#                                 total_35_54 = "B21005_013",
#                                 total_35_54_vet = "B21005_014",
#                                 total_35_54_vet_in_lf_tot = "B21005_015",
#                                 total_35_54_vet_in_lf_emp = "B21005_016",
#                                 total_35_54_vet_in_lf_unemp = "B21005_017",
#                                 total_35_54_vet_not_in_lf = "B21005_018",
#                                 total_35_54_nonvet = "B21005_019",
#                                 total_35_54_nonvet_in_lf_tot = "B21005_020",
#                                 total_35_54_nonvet_in_lf_emp = "B21005_021",
#                                 total_35_54_nonvet_in_lf_unemp = "B21005_022",
#                                 total_35_54_nonvet_not_in_lf = "B21005_023",
#                                 total_55_64 = "B21005_024",
#                                 total_55_64_vet = "B21005_025",
#                                 total_55_64_vet_in_lf_tot = "B21005_026",
#                                 total_55_64_vet_in_lf_emp = "B21005_027",
#                                 total_55_64_vet_in_lf_unemp = "B21005_028",
#                                 total_55_64_vet_not_in_lf = "B21005_029",
#                                 total_55_64_nonvet = "B21005_030",
#                                 total_55_64_nonvet_in_lf_tot = "B21005_031",
#                                 total_55_64_nonvet_in_lf_emp = "B21005_032",
#                                 total_55_64_nonvet_in_lf_unemp = "B21005_033",
#                                 total_55_64_nonvet_not_in_lf = "B21005_034"                               
#                   ),
#                   year = 2018, format = 'long')

#check for unique MSAs
#unique(sample$NAME)

                                #long to wide
                                # vet_age_pct <- as.matrix(vet_age_tbl)
                                # colnames(vet_age_pct) <- NULL
                                # vet_age_pct <- t(vet_age_pct)
                                # vet_age_colnames <- vet_age_pct[1,]
                                # vet_age_pct <- vet_age_pct[-1,]
                                # vet_age_pct <- as.data.frame(vet_age_pct)
                                # colnames(vet_age_pct) <- vet_age_colnames
                                
                                
# vet_age_m <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
#    variables = c(),
#    year = 2018)

# vet_med_income %>%
#   filter(variable == 'med_inc_vet_male' |
#            variable == 'med_inc_vet_female' | variable == 'med_inc_nonvet_male' 
#          | variable == 'med_inc_nonvet_female') %>%
#   ggplot(aes(varia))
# %>%
#   ggplot(aes())
#   # mutate(isfemale = ifelse(grepl('female', variable), 'Female', 'Male'))

#                                 