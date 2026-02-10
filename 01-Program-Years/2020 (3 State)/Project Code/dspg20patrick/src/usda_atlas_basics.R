# install.packages("tidyverse")
# install.packages("viridis")
# install.packages("sf")
# install.packages("ggthemes")
# install.packages("RColorBrewer")
# install.packages("ggplot2")
library(tidyverse)
library(viridis)
library(sf)
library(ggthemes)
library(RColorBrewer)
library(ggplot2)
library(tigris)
library(data.table)

options(scipen = 999)

######## USDA Food Desert Atlas Data Geographies - 2017 #################


#
# Tigris Work ------------------------------------------------------------------------
#
usda_data <- fread("./data/working/atlas_patrick_county.csv")

usda_data <- usda_data %>%
  rename(GEOID = CensusTract)

usda_data$GEOID <- as.character(usda_data$GEOID)
usda_data <- as.data.frame(usda_data)

# load in tigris county tract shape files
tract_data <- tracts(51, 141, cb = FALSE, year = 2017)

# change from sp to sf data type
tract_data <- st_as_sf(tract_data)

# join and make sf
usda_geo_data <- usda_data %>%
  left_join(tract_data)
usda_geo_data <- st_as_sf(usda_geo_data)

# Write for web
usda <- usda_geo_data %>% select(GEOID, State, County, 
                                 STATEFP, COUNTYFP, TRACTCE, NAME, geometry,
                                 lahunv1share, lahunv10share, lakids1share, lakids10share, 
                                 lalowi1share, lalowi10share, lapop1share, lapop10share, 
                                 laseniors1share, laseniors10share)

usda <- usda %>% mutate(lahunv1share = lahunv1share * 100,
                        lahunv10share = lahunv10share * 100,
                        lakids1share = lakids1share * 100,
                        lakids10share = lakids10share * 100, 
                        lalowi1share = lalowi1share * 100,
                        lalowi10share = lalowi10share * 100,
                        lapop1share = lapop1share * 100,
                        lapop10share = lapop10share * 100,
                        laseniors1share = laseniors1share * 100,
                        laseniors10share = laseniors10share * 100)

usda <- st_as_sf(usda)
usda <- usda %>% st_transform(4269)
write_rds(usda, "./data/web/usda.Rds")


# USDA plots -----------------------------------------------------------------------
# Vars: LAhalfand10 lahunv10share	lakids10share	lalowi10share	lapop10share	laseniors10share	LILATracts_1And10


#
# Plots at 10 miles ------------------------------------------------------------
#

# lahunv10share
ggplot() +
  geom_sf(data = usda_geo_data, size = 0.2, aes(fill = lahunv10share * 100)) +
  labs(title = "Percent of housing units without or \n with low vehicle access at 10 miles",
       caption = "Source: USDA Food Access Research Atlas, 2017") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#deebf7", high = "#3182bd")
ggsave(path = "./output/usda/", device = "png", filename = "plot_lahunv10share.png", plot = last_plot())

# lakids10share
ggplot() +
  geom_sf(data = usda_geo_data, size = 0.2, aes(fill = lakids10share * 100)) +
  labs(title = "Percent of children with low food access at 10 miles",
       caption = "Source: USDA Food Access Research Atlas, 2017") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#deebf7", high = "#3182bd")
ggsave(path = "./output/usda/", device = "png", filename = "plot_lakids10share.png", plot = last_plot())

# lalowi10share
ggplot() +
  geom_sf(data = usda_geo_data, size = 0.2, aes(fill = lalowi10share * 100)) +
  labs(title = "Percent of low income persons \nwith low food access at 10 miles",
       caption = "Source: USDA Food Access Research Atlas, 2017") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#deebf7", high = "#3182bd")
ggsave(path = "./output/usda/", device = "png", filename = "plot_lalowi10share.png", plot = last_plot())

# lapop10share
ggplot() +
  geom_sf(data = usda_geo_data, size = 0.2, aes(fill = lapop10share * 100)) +
  labs(title = "Percent of population with \nlow food access at 10 miles",
       caption = "Source: USDA Food Access Research Atlas, 2017") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#deebf7", high = "#3182bd")
ggsave(path = "./output/usda/", device = "png", filename = "plot_lapop10share.png", plot = last_plot())

# laseniors10share
ggplot() +
  geom_sf(data = usda_geo_data, size = 0.2, aes(fill = laseniors10share * 100)) +
  labs(title = "Percent of seniors with \nlow food access at 10 miles",
       caption = "Source: USDA Food Access Research Atlas, 2017") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Share", low = "#deebf7", high = "#3182bd")
ggsave(path = "./output/usda/", device = "png", filename = "plot_laseniors10share.png", plot = last_plot())


#
# Plots at 1 mile ------------------------------------------------------------
#

# lahunv1share
ggplot() +
  geom_sf(data = usda_geo_data, size = 0.2, aes(fill = lahunv10share * 100)) +
  labs(title = "Percent of housing units without or \n with low vehicle access at 1 mile",
       caption = "Source: USDA Food Access Research Atlas, 2017") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#deebf7", high = "#3182bd")
ggsave(path = "./output/usda/", device = "png", filename = "plot_lahunv1share.png", plot = last_plot())

# lakids1share
ggplot() +
  geom_sf(data = usda_geo_data, size = 0.2, aes(fill = lakids1share * 100)) +
  labs(title = "Percent of children with low food access at 1 mile",
       caption = "Source: USDA Food Access Research Atlas, 2017") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#deebf7", high = "#3182bd")
ggsave(path = "./output/usda/", device = "png", filename = "plot_lakids1share.png", plot = last_plot())

# lalowi1share
ggplot() +
  geom_sf(data = usda_geo_data, size = 0.2, aes(fill = lalowi1share * 100)) +
  labs(title = "Percent of low income persons \nwith low food access at 1 mile",
       caption = "Source: USDA Food Access Research Atlas, 2017") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#deebf7", high = "#3182bd")
ggsave(path = "./output/usda/", device = "png", filename = "plot_lalowi1share.png", plot = last_plot())

# lapop1share
ggplot() +
  geom_sf(data = usda_geo_data, size = 0.2, aes(fill = lapop1share * 100)) +
  labs(title = "Percent of population with \nlow food access at 1 mile",
       caption = "Source: USDA Food Access Research Atlas, 2017") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#deebf7", high = "#3182bd")
ggsave(path = "./output/usda/", device = "png", filename = "plot_lapop1share.png", plot = last_plot())

# laseniors1share - change scale
ggplot() +
  geom_sf(data = usda_geo_data, size = 0.2, aes(fill = laseniors1share * 100)) +
  labs(title = "Percent of seniors with \nlow food access at 1 mile",
       caption = "Source: USDA Food Access Research Atlas, 2017") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#deebf7", high = "#3182bd")
ggsave(path = "./output/usda/", device = "png", filename = "plot_laseniors1share.png", plot = last_plot())


# USDA Map Function ------------------------------------------------------------------
# usda_one_plot <- function(usda_variables, plot_title, file_name, ...){
#   ggplot() +
#     geom_sf(data = usda_geo_data, size = 0.2, aes(fill = usda_variables)) +
#     labs(title = sprintf("%s at the 1 mile", plot_title),
#          caption = "Source: USDA Food Access Research Atlas, 2017.") +
#     theme_map() +
#     theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#           legend.title = element_text(size = 11, face = "bold"),
#           legend.text = element_text(size = 11),
#           legend.position = "right") +
#     scale_fill_continuous(name = "Percent", low = "#fff7ec", high = "#7F0000",
#                           limits = c(floor(min(usda_variables)), round(max(usda_variables))),
#                           breaks = seq(floor(min(usda_variables)), drounf(max(usda_variables)), length.out = 5))
#   ggsave(path = "./output/usda/", device = "png", filename = sprintf("plot_%s.png", file_name), plot = last_plot())
#   
# }
# usda_one_plot(usda_geo_data$laseniors1share, "Percent individuals 65+ without health insurance of all individuals 65+", "healthins")
#
# usda_ten_plot <- function(usda_variables, plot_title, file_name, ...){
#   ggplot() +
#     geom_sf(data = usda_geo_data, size = 0.2, aes(fill = usda_variables)) +
#     labs(title = sprintf("%s at the 10 mile", plot_title),
#          caption = "Source: USDA Food Access Research Atlas, 2017.") +
#     theme_map() +
#     theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#           legend.title = element_text(size = 11, face = "bold"),
#           legend.text = element_text(size = 11),
#           legend.position = "right") +
#     scale_fill_continuous(name = "Percent", low = "#fff7ec", high = "#7F0000",
#                           limits = c(floor(min(usda_variables)), round(max(usda_variables))),
#                           breaks = seq(floor(min(usda_variables)), drounf(max(usda_variables)), length.out = 5))
#   ggsave(path = "./output/usda/", device = "png", filename = sprintf("plot_%s.png", file_name), plot = last_plot())
#   
# }
# usda_one_plot(usda_geo_data$laseniors1share, "Percent individuals 65+ without health insurance of all individuals 65+", "healthins")


# Plots at 20 miles --------------------------------------------------

# LAhalfand20 does not exist

# lahunv20sharem- ALL ZEROES
# min_lahunv20share <- floor(min(usda_geo_data$lahunv20share))
# max_lahunv20share <- ceiling(max(usda_geo_data$lahunv20share))
# ggplot() +
#   geom_sf(data = usda_geo_data, size = 0.2, aes(fill = lahunv20share)) +
#   labs(title = "Percent of housing units without or \n with low vehicle access at 20 miles",
#        caption = "Source: USDA Food Access Research Atlas, 2017") +
#   theme_map() +
#   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#         legend.title = element_text(size = 11, face = "bold"),
#         legend.text = element_text(size = 11),
#         legend.position = "right") +
#   scale_fill_continuous(name = "Share", low = "#fff7ec", high = "#7F0000",
#                         limits = c(min_lahunv20share, max_lahunv20share), 
#                         breaks = seq(min_lahunv20share, max_lahunv20share, length.out = 5))
# ggsave(path = "./output/usda/", device = "png", filename = "plot_lahunv20share.png", plot = last_plot())
# 
# # lakids20share - ALL ZEROES
# min_lakids20share <- floor(min(usda_geo_data$lakids20share))
# max_lakids20share <- ceiling(max(usda_geo_data$lakids20share))
# ggplot() +
#   geom_sf(data = usda_geo_data, size = 0.2, aes(fill = lakids20share)) +
#   labs(title = "Percent of children with \nlow food access at 20 miles",
#        caption = "Source: USDA Food Access Research Atlas, 2017") +
#   theme_map() +
#   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#         legend.title = element_text(size = 11, face = "bold"),
#         legend.text = element_text(size = 11),
#         legend.position = "right") +
#   scale_fill_continuous(name = "Share", low = "#fff7ec", high = "#7F0000",
#                         limits = c(min_lakids20share, max_lakids20share), 
#                         breaks = seq(min_lakids20share, max_lakids20share, length.out = 5))
# ggsave(path = "./output/usda/", device = "png", filename = "plot_lakids20share.png", plot = last_plot())
# 
# # lalowi20share - ALL ZEROES
# min_lalowi20share <- floor(min(usda_geo_data$lalowi20share))
# max_lalowi20share <- ceiling(max(usda_geo_data$lalowi20share))
# ggplot() +
#   geom_sf(data = usda_geo_data, size = 0.2, aes(fill = lalowi20share)) +
#   labs(title = "Percent of children with \nlow food access at 20 miles",
#        caption = "Source: USDA Food Access Research Atlas, 2017") +
#   theme_map() +
#   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#         legend.title = element_text(size = 11, face = "bold"),
#         legend.text = element_text(size = 11),
#         legend.position = "right") +
#   scale_fill_continuous(name = "Share", low = "#fff7ec", high = "#7F0000",
#                         limits = c(min_lalowi20share, max_lalowi20share), 
#                         breaks = seq(min_lalowi20share, max_lalowi20share, length.out = 5))
# ggsave(path = "./output/usda/", device = "png", filename = "plot_lalowi20share.png", plot = last_plot())
# 
# # lapop20share - ALL ZEROES
# min_lapop20share <- floor(min(usda_geo_data$lapop20share))
# max_lapop20share <- ceiling(max(usda_geo_data$lapop20share))
# ggplot() +
#   geom_sf(data = usda_geo_data, size = 0.2, aes(fill = lapop10share)) +
#   labs(title = "Percent of population with \nlow food access at 20 miles",
#        caption = "Source: USDA Food Access Research Atlas, 2017") +
#   theme_map() +
#   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#         legend.title = element_text(size = 11, face = "bold"),
#         legend.text = element_text(size = 11),
#         legend.position = "right") +
#   scale_fill_continuous(name = "Share", low = "#fff7ec", high = "#7F0000",
#                         limits = c(min_lapop20share, max_lapop20share), 
#                         breaks = seq(min_lapop20share, max_lapop20share, length.out = 5))
# ggsave(path = "./output/usda/", device = "png", filename = "plot_lapop20share.png", plot = last_plot())
# 
# # laseniors20share - ALL ZEROES
# min_laseniors20share <- floor(min(usda_geo_data$laseniors20share))
# max_laseniors20share <- ceiling(max(usda_geo_data$laseniors20share))
# ggplot() +
#   geom_sf(data = usda_geo_data, size = 0.2, aes(fill = laseniors20share)) +
#   labs(title = "Percent of seniors with \nlow food access at 20 miles",
#        caption = "Source: USDA Food Access Research Atlas, 2017") +
#   theme_map() +
#   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#         legend.title = element_text(size = 11, face = "bold"),
#         legend.text = element_text(size = 11),
#         legend.position = "right") +
#   scale_fill_continuous(name = "Share", low = "#fff7ec", high = "#7F0000",
#                         limits = c(min_laseniors20share, max_laseniors20share), 
#                         breaks = seq(min_laseniors20share, max_laseniors20share, length.out = 5))
# ggsave(path = "./output/usda/", device = "png", filename = "plot_laseniors20share.png", plot = last_plot())
# 
# # LILATracts_1And20 - ALL ZEROES
# min_LILATracts_1And20 <- floor(min(usda_geo_data$LILATracts_1And20))
# max_LILATracts_1And20 <- ceiling(max(usda_geo_data$LILATracts_1And20))
# ggplot() +
#   geom_sf(data = usda_geo_data, size = 0.2, aes(fill = LILATracts_1And20)) +
#   labs(title = "Low income and low access at 20 miles",
#        caption = "Source: USDA Food Access Research Atlas, 2017") +
#   theme_map() +
#   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#         legend.title = element_text(size = 11, face = "bold"),
#         legend.text = element_text(size = 11),
#         legend.position = "right") +
#   scale_fill_continuous(name = "", low = "#fff7ec", high = "#7F0000",
#                         limits = c(min_LILATracts_1And20, max_LILATracts_1And20), 
#                         breaks = seq(min_LILATracts_1And20, max_LILATracts_1And20, length.out = 5))
# ggsave(path = "./output/usda/", device = "png", filename = "plot_LILATracts_1And20.png", plot = last_plot())
