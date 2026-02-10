library(tidyverse)
library(tigris)
library(sf)

va <- tracts(state = 51)
va <- st_as_sf(va)
ffu_va_long$GEOID <- as.character(ffu_va_long$GEOID)
va <- left_join(va, ffu_va_long, by = 'GEOID')
va <- va %>%
  mutate(bad = ifelse((value > 1 | value < -1), TRUE, FALSE),
         verybad = ifelse((value > 1.5 | value < -1.5), TRUE, FALSE))

ggplot() +
  geom_sf(data = va)
