library(tidyverse)

ffx_original$HOUSING_TYPE <- as.factor(ffx_original$HOUSING_TYPE)
summary(ffx_original$HOUSING_TYPE)
ffx_original$HOUSING_TYPE <- recode(ffx_original$HOUSING_TYPE, 'High Rise' = 'Multiplex/Condo/Apartment')
ffx_original$HOUSING_TYPE <- recode(ffx_original$HOUSING_TYPE, 'Mid Rise' = 'Multiplex/Condo/Apartment')
ffx_original$HOUSING_TYPE <- recode(ffx_original$HOUSING_TYPE, 'Low Rise' = 'Multiplex/Condo/Apartment')
ffx_original$HOUSING_TYPE <- recode(ffx_original$HOUSING_TYPE, 'Mobile Home' = 'Single Family Home')
ffx_original$HOUSING_TYPE <- recode(ffx_original$HOUSING_TYPE, 'Multiplex' = 'Multiplex/Condo/ApartmentCondo')
ffx_original$HOUSING_TYPE <- recode(ffx_original$HOUSING_TYPE, 'Townhouse' = 'Single Family Home')
ffx_original$HOUSING_TYPE <- recode(ffx_original$HOUSING_TYPE, 'Single Family Detached' = 'Single Family Home')

ggplot(ffx_original) +
  aes(x=forcats::fct_infreq(HOUSING_TYPE)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label=..count.., vjust = -1))

ffx_2018$property_indicator_code <- as.character(ffx_2018$property_indicator_code)
summary(ffx_2018$property_indicator_code)
ffx_2018$property_indicator_code <- recode(ffx_2018$property_indicator_code, '10' = 'Single Family Home')
ffx_2018$property_indicator_code <- recode(ffx_2018$property_indicator_code, '11' = 'Multiplex/Condo/Apartment')
ffx_2018$property_indicator_code <- recode(ffx_2018$property_indicator_code, '21' = 'Duplex')
ffx_2018$property_indicator_code <- recode(ffx_2018$property_indicator_code, '22' = 'Multiplex/Condo/Apartment')
ffx_2018$property_indicator_code <- recode(ffx_2018$property_indicator_code, '24' = 'Multiplex/Condo/Apartment')
                                          

ffx_2018 %>%
  filter(property_indicator_code == 'Single Family Home' | property_indicator_code == 'Multiplex/Condo/Apartment' | property_indicator_code == 'Multiple/Condo/Apartment' | property_indicator_code == 'Duplex') %>%
  ggplot(aes(x=property_indicator_code)) +
    geom_bar()+
    geom_text(stat = 'count', aes(label=..count.., vjust = -1))

f1 <- ffx_2018 %>%
  group_by(property_indicator_code) %>%
  summarize(n = n()) %>%
  mutate(dataset = 'CoreLogic') %>%
  filter(property_indicator_code == 'Single Family Home' | property_indicator_code == 'Multiplex/Condo/Apartment' | property_indicator_code == 'Multiple/Condo/Apartment' | property_indicator_code == 'Duplex')
  
f2 <- ffx_original %>%
  group_by(HOUSING_TYPE) %>%
  summarize(n = n()) %>%
  mutate(dataset = 'Fairfax County') %>%
  filter(HOUSING_TYPE == 'Single Family Home' | HOUSING_TYPE == 'Multiplex/Condo/Apartment' | HOUSING_TYPE == 'Multiplex/Condo/Apartment' | HOUSING_TYPE == 'Duplex') %>%
  rename('property_indicator_code' = 'HOUSING_TYPE')

f <- rbind(f1, f2)

ggplot(f) +
  aes(x = property_indicator_code, y = n, fill = dataset) +
  geom_bar(stat = 'identity', position = 'dodge') #+
  #geom_text(aes(y = n, label = n), position_dodge(width = 1))


nk_2018$property_indicator_code <- as.character(nk_2018$property_indicator_code)
summary(nk_2018$property_indicator_code)
nk_2018$property_indicator_code <- recode(nk_2018$property_indicator_code, '10' = 'Single Family Home')
nk_2018$property_indicator_code <- recode(nk_2018$property_indicator_code, '11' = 'Multiplex/Condo/Apartment')
nk_2018$property_indicator_code <- recode(nk_2018$property_indicator_code, '21' = 'Duplex')
nk_2018$property_indicator_code <- recode(nk_2018$property_indicator_code, '22' = 'Multiplex/Condo/Apartment')
nk_2018$property_indicator_code <- recode(nk_2018$property_indicator_code, '24' = 'Multiplex/Condo/Apartment')
nk_2018$property_indicator_code <- as.factor(nk_2018$property_indicator_code)

names(nk)

nk_2018 %>%
  filter(property_indicator_code == 'Single Family Home' | property_indicator_code == 'Multiplex/Condo/Apartment' | property_indicator_code == 'Multiple/Condo/Apartment' | property_indicator_code == 'Duplex') %>%
  ggplot(aes(x=reorder(property_indicator_code, desc(property_indicator_code)))) +
  geom_bar(fill = '#232D4B')+
  geom_text(stat = 'count', aes(label=..count.., vjust = -0.5), size =3) +
  xlab('Property Type') + 
  ylab('Number of Properties') +
  theme_classic() +
  labs(title='Number of Properties by Type in New Kent County CoreLogic \nData')
