library(dplyr)
library(sf)
library(readr)

# coverage tables -------------------------------------------------

residential <- readRDS("./patrickdash/data/residential.Rds")
residential <- st_as_sf(residential, coords = c("longitude", "latitude"))
st_crs(residential) <- "+proj=longlat +datum=WGS84"
residential <- st_transform(residential, '+proj=longlat +datum=WGS84')

wifi <- 1:10
for(i in 1:length(wifi)){

wifi_iso10 <- readRDS(paste0("data/working/isochrones/wifi/wifi_iso_10_",i,".RDS"))
wifi_iso15 <- readRDS(paste0("data/working/isochrones/wifi/wifi_iso_15_",i,".RDS"))

pp_10 <- st_intersection(residential, wifi_iso10)
pp_15 <- st_intersection(residential, wifi_iso15)

coverage_10 <- (nrow(pp_10)/nrow(residential)*100)
coverage_15 <- (nrow(pp_15)/nrow(residential)*100)

table <- as.data.frame(c("10 Minutes", "15 Minutes"))
table$Coverage <- c(coverage_10, coverage_15)
colnames(table) <- c("Time", "Coverage")
write.csv(table, file = paste0("wifi_iso_table_",i,".csv"), row.names = FALSE)
}

ems <- 1:9
for(i in 1:length(ems)){
  
  ems_iso8 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_8_",i,".RDS"))
  ems_iso10 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_10_",i,".RDS"))
  ems_iso12 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_12_",i,".RDS"))
  
  pp_8 <- st_intersection(residential, ems_iso8)
  pp_10 <- st_intersection(residential, ems_iso10)
  pp_12 <- st_intersection(residential, ems_iso12)
  
  coverage_8 <- (nrow(pp_8)/nrow(residential)*100)
  coverage_10 <- (nrow(pp_10)/nrow(residential)*100)
  coverage_12 <- (nrow(pp_12)/nrow(residential)*100)
  
  table <- as.data.frame(c("8 Minutes", "10 Minutes", "12 Minutes"))
  table$Coverage <- c(coverage_8, coverage_10, coverage_12)
  colnames(table) <- c("Time", "Coverage")
  write.csv(table, file = paste0("ems_iso_table_",i,".csv"), row.names = FALSE)
}

grc <- 1:7
for(i in 1:length(grc)){
  
  grc_iso10 <- readRDS(paste0("data/working/isochrones/grocery/grc_iso_10_",i,".RDS"))
  grc_iso15 <- readRDS(paste0("data/working/isochrones/grocery/grc_iso_15_",i,".RDS"))
  
  pp_10 <- st_intersection(residential, grc_iso10)
  pp_15 <- st_intersection(residential, grc_iso15)
  
  coverage_10 <- (nrow(pp_10)/nrow(residential)*100)
  coverage_15 <- (nrow(pp_15)/nrow(residential)*100)
  
  table <- as.data.frame(c("10 Minutes", "15 Minutes"))
  table$Coverage <- c(coverage_10, coverage_15)
  colnames(table) <- c("Time", "Coverage")
  write.csv(table, file = paste0("grc_iso_table_",i,".csv"), row.names = FALSE)
}

#focus on number, location combo for this list
# emsmap_1.png -> STUART VOLUNTEER FIRE DEPARTMENT
# emsmap_2.png -> MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT
# emsmap_3.png -> BLUE RIDGE VOLUNTEER RESCUE SQUAD
# emsmap_4.png -> VESTA RESCUE SQUAD
# emsmap_5.png -> ARARAT RESCUE SQUAD
# emsmap_6.png -> FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 HQ
# emsmap_7.png -> JEB STUART RESCUE SQUAD
# emsmap_8.png -> SMITH RIVER RESCUE SQUAD
# emsmap_9.png -> FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2

# 1 <- "Mountain Meadow Farm and Craft Market"
# 2 <- "Lowes Foods of Stuart"                
# 3 <- "Patrick County Local Farmers Market"
# 4 <- "Stuart Farmers Market"                
# 5 <- "W & W Produce"
# 6 <- "Walmart Supercenter"                  
# 7 <- "Poor Farmers Farm" 

# wifimap_1.png -> Meadows of Dan Elementary School
# wifimap_2.png -> Woolwine Elementary School
# wifimap_3.png -> Patrick Springs Primary School
# wifimap_4.png -> Blue Ridge Elementary School
# wifimap_5.png -> Patrick County High School
# wifimap_6.png -> Stuart Elementary School
# wifimap_7.png -> Patrick County Branch Library
# wifimap_8.png -> Hardin Reynolds Memorial School
# wifimap_9.png -> Stuart Baptist Church                      
# wifimap_10.png -> Patrick Henry Community College Stuart Campus

# st join tables -------------------------------------------------

library(sf)

ems_list <- list.files("data/working/isochrones/ems/")

ems_iso8_1 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_8_",1,".RDS"))
ems_iso10_1 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_10_",1,".RDS"))
ems_iso12_1 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_12_",1,".RDS"))

ems_iso8_2 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_8_",2,".RDS"))
ems_iso10_2 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_10_",2,".RDS"))
ems_iso12_2 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_12_",2,".RDS"))

ems_iso8_3 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_8_",3,".RDS"))
ems_iso10_3 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_10_",3,".RDS"))
ems_iso12_3 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_12_",3,".RDS"))

ems_iso8_4 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_8_",4,".RDS"))
ems_iso10_4 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_10_",4,".RDS"))
ems_iso12_4 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_12_",4,".RDS"))

ems_iso8_5 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_8_",5,".RDS"))
ems_iso10_5 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_10_",5,".RDS"))
ems_iso12_5 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_12_",5,".RDS"))

ems_iso8_6 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_8_",6,".RDS"))
ems_iso10_6 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_10_",6,".RDS"))
ems_iso12_6 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_12_",6,".RDS"))

ems_iso8_7 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_8_",7,".RDS"))
ems_iso10_7 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_10_",7,".RDS"))
ems_iso12_7 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_12_",7,".RDS"))

ems_iso8_8 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_8_",8,".RDS"))
ems_iso10_8 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_10_",8,".RDS"))
ems_iso12_8 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_12_",8,".RDS"))

ems_iso8_9 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_8_",9,".RDS"))
ems_iso10_9 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_10_",9,".RDS"))
ems_iso12_9 <- readRDS(paste0("data/working/isochrones/ems/ems_iso_12_",9,".RDS"))

ems_iso8 <- st_union(ems_iso8_1,ems_iso8_2)
ems_iso8 <- st_union(ems_iso8,ems_iso8_3)
ems_iso8 <- st_union(ems_iso8,ems_iso8_4)
ems_iso8 <- st_union(ems_iso8,ems_iso8_5)
ems_iso8 <- st_union(ems_iso8,ems_iso8_6)
ems_iso8 <- st_union(ems_iso8,ems_iso8_7)
ems_iso8 <- st_union(ems_iso8,ems_iso8_8)
ems_iso8 <- st_union(ems_iso8,ems_iso8_9)


plot(st_geometry(ems_iso8))

ems_iso10 <- st_union(ems_iso10_1,ems_iso10_2)
ems_iso10 <- st_union(ems_iso10,ems_iso10_3)
ems_iso10 <- st_union(ems_iso10,ems_iso10_4)
ems_iso10 <- st_union(ems_iso10,ems_iso10_5)
ems_iso10 <- st_union(ems_iso10,ems_iso10_6)
ems_iso10 <- st_union(ems_iso10,ems_iso10_7)
ems_iso10 <- st_union(ems_iso10,ems_iso10_8)
ems_iso10 <- st_union(ems_iso10,ems_iso10_9)

plot(st_geometry(ems_iso10))

ems_iso12 <- st_union(ems_iso12_1,ems_iso12_2)
ems_iso12 <- st_union(ems_iso12,ems_iso12_3)
ems_iso12 <- st_union(ems_iso12,ems_iso12_4)
ems_iso12 <- st_union(ems_iso12,ems_iso12_5)
ems_iso12 <- st_union(ems_iso12,ems_iso12_6)
ems_iso12 <- st_union(ems_iso12,ems_iso12_7)
ems_iso12 <- st_union(ems_iso12,ems_iso12_8)
ems_iso12 <- st_union(ems_iso12,ems_iso12_9)

plot(st_geometry(ems_iso12))

ems_intersect_8 <- st_intersection(ems_iso8, residential)
ems_intersect_10 <- st_intersection(ems_iso10, residential)
ems_intersect_12 <- st_intersection(ems_iso12, residential)

ems_coverage_8 <- (nrow(ems_intersect_8)/nrow(residential)*100)
ems_coverage_10 <- (nrow(ems_intersect_10)/nrow(residential)*100)
ems_coverage_12 <- (nrow(ems_intersect_12)/nrow(residential)*100)


table <- as.data.frame(c("8 Minutes", "10 Minutes", "12 Minutes"))
table$Coverage <- c(ems_coverage_8, ems_coverage_10, ems_coverage_12)
colnames(table) <- c("Time", "Coverage")
write.csv(table, file = "ems_iso_table.csv", row.names = FALSE)

grc_iso10_1 <- readRDS(paste0("data/working/isochrones/grocery/grc_iso_10_",1,".RDS"))
grc_iso15_1 <- readRDS(paste0("data/working/isochrones/grocery/grc_iso_15_",1,".RDS"))

grc_iso10_2 <- readRDS(paste0("data/working/isochrones/grocery/grc_iso_10_",2,".RDS"))
grc_iso15_2 <- readRDS(paste0("data/working/isochrones/grocery/grc_iso_15_",2,".RDS"))

grc_iso10_3 <- readRDS(paste0("data/working/isochrones/grocery/grc_iso_10_",3,".RDS"))
grc_iso15_3 <- readRDS(paste0("data/working/isochrones/grocery/grc_iso_15_",3,".RDS"))

grc_iso10_4 <- readRDS(paste0("data/working/isochrones/grocery/grc_iso_10_",4,".RDS"))
grc_iso15_4 <- readRDS(paste0("data/working/isochrones/grocery/grc_iso_15_",4,".RDS"))

grc_iso10_5 <- readRDS(paste0("data/working/isochrones/grocery/grc_iso_10_",5,".RDS"))
grc_iso15_5 <- readRDS(paste0("data/working/isochrones/grocery/grc_iso_15_",5,".RDS"))

grc_iso10_6 <- readRDS(paste0("data/working/isochrones/grocery/grc_iso_10_",6,".RDS"))
grc_iso15_6 <- readRDS(paste0("data/working/isochrones/grocery/grc_iso_15_",6,".RDS"))

grc_iso10_7 <- readRDS(paste0("data/working/isochrones/grocery/grc_iso_10_",7,".RDS"))
grc_iso15_7 <- readRDS(paste0("data/working/isochrones/grocery/grc_iso_15_",7,".RDS"))


grc_iso10 <- st_union(grc_iso10_1,grc_iso10_2)
grc_iso10 <- st_union(grc_iso10,grc_iso10_3)
grc_iso10 <- st_union(grc_iso10,grc_iso10_4)
grc_iso10 <- st_union(grc_iso10,grc_iso10_5)
grc_iso10 <- st_union(grc_iso10,grc_iso10_6)
grc_iso10 <- st_union(grc_iso10,grc_iso10_7)

grc_iso15 <- st_union(grc_iso15_1,grc_iso15_2)
grc_iso15 <- st_union(grc_iso15,grc_iso15_3)
grc_iso15 <- st_union(grc_iso15,grc_iso15_4)
grc_iso15 <- st_union(grc_iso15,grc_iso15_5)
grc_iso15 <- st_union(grc_iso15,grc_iso15_6)
grc_iso15 <- st_union(grc_iso15,grc_iso15_7)

grc_intersect_10 <- st_intersection(grc_iso10, residential)
grc_intersect_15 <- st_intersection(grc_iso15, residential)

grc_coverage_10 <- (nrow(grc_intersect_10)/nrow(residential)*100)
grc_coverage_15 <- (nrow(grc_intersect_15)/nrow(residential)*100)

table <- as.data.frame(c("10 Minutes", "15 Minutes"))
table$Coverage <- c(grc_coverage_10, grc_coverage_15)
colnames(table) <- c("Time", "Coverage")
write.csv(table, file = "grc_iso_table.csv", row.names = FALSE)

wifi_iso10_1 <- readRDS(paste0("data/working/isochrones/wifi/wifi_iso_10_",1,".RDS"))
wifi_iso15_1 <- readRDS(paste0("data/working/isochrones/wifi/wifi_iso_15_",1,".RDS"))

wifi_iso10_2 <- readRDS(paste0("data/working/isochrones/wifi/wifi_iso_10_",2,".RDS"))
wifi_iso15_2 <- readRDS(paste0("data/working/isochrones/wifi/wifi_iso_15_",2,".RDS"))

wifi_iso10_3 <- readRDS(paste0("data/working/isochrones/wifi/wifi_iso_10_",3,".RDS"))
wifi_iso15_3 <- readRDS(paste0("data/working/isochrones/wifi/wifi_iso_15_",3,".RDS"))

wifi_iso10_4 <- readRDS(paste0("data/working/isochrones/wifi/wifi_iso_10_",4,".RDS"))
wifi_iso15_4 <- readRDS(paste0("data/working/isochrones/wifi/wifi_iso_15_",4,".RDS"))

wifi_iso10_5 <- readRDS(paste0("data/working/isochrones/wifi/wifi_iso_10_",5,".RDS"))
wifi_iso15_5 <- readRDS(paste0("data/working/isochrones/wifi/wifi_iso_15_",5,".RDS"))

wifi_iso10_6 <- readRDS(paste0("data/working/isochrones/wifi/wifi_iso_10_",6,".RDS"))
wifi_iso15_6 <- readRDS(paste0("data/working/isochrones/wifi/wifi_iso_15_",6,".RDS"))

wifi_iso10_7 <- readRDS(paste0("data/working/isochrones/wifi/wifi_iso_10_",7,".RDS"))
wifi_iso15_7 <- readRDS(paste0("data/working/isochrones/wifi/wifi_iso_15_",7,".RDS"))

wifi_iso10_8 <- readRDS(paste0("data/working/isochrones/wifi/wifi_iso_10_",8,".RDS"))
wifi_iso15_8 <- readRDS(paste0("data/working/isochrones/wifi/wifi_iso_15_",8,".RDS"))

wifi_iso10_9 <- readRDS(paste0("data/working/isochrones/wifi/wifi_iso_10_",9,".RDS"))
wifi_iso15_9 <- readRDS(paste0("data/working/isochrones/wifi/wifi_iso_15_",9,".RDS"))

wifi_iso10_10 <- readRDS(paste0("data/working/isochrones/wifi/wifi_iso_10_",10,".RDS"))
wifi_iso15_10 <- readRDS(paste0("data/working/isochrones/wifi/wifi_iso_15_",10,".RDS"))

wifi_iso10 <- st_union(wifi_iso10_1,wifi_iso10_2)
wifi_iso10 <- st_union(wifi_iso10,wifi_iso10_3)
wifi_iso10 <- st_union(wifi_iso10,wifi_iso10_4)
wifi_iso10 <- st_union(wifi_iso10,wifi_iso10_5)
wifi_iso10 <- st_union(wifi_iso10,wifi_iso10_6)
wifi_iso10 <- st_union(wifi_iso10,wifi_iso10_7)
wifi_iso10 <- st_union(wifi_iso10,wifi_iso10_8)
wifi_iso10 <- st_union(wifi_iso10,wifi_iso10_9)
wifi_iso10 <- st_union(wifi_iso10,wifi_iso10_10)


wifi_iso15 <- st_union(wifi_iso15_1,wifi_iso15_2)
wifi_iso15 <- st_union(wifi_iso15,wifi_iso15_3)
wifi_iso15 <- st_union(wifi_iso15,wifi_iso15_4)
wifi_iso15 <- st_union(wifi_iso15,wifi_iso15_5)
wifi_iso15 <- st_union(wifi_iso15,wifi_iso15_6)
wifi_iso15 <- st_union(wifi_iso15,wifi_iso15_7)
wifi_iso15 <- st_union(wifi_iso15,wifi_iso15_8)
wifi_iso15 <- st_union(wifi_iso15,wifi_iso15_9)
wifi_iso15 <- st_union(wifi_iso15,wifi_iso15_10)


wifi_intersect_10 <- st_intersection(wifi_iso10, residential)
wifi_intersect_15 <- st_intersection(wifi_iso15, residential)

wifi_coverage_10 <- (nrow(wifi_intersect_10)/nrow(residential)*100)
wifi_coverage_15 <- (nrow(wifi_intersect_15)/nrow(residential)*100)

table <- as.data.frame(c("10 Minutes", "15 Minutes"))
table$Coverage <- c(wifi_coverage_10, wifi_coverage_15)
colnames(table) <- c("Time", "Coverage")
write.csv(table, file = "wifi_iso_table.csv", row.names = FALSE)
