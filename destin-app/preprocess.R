# preprocess destin app data

# load packages
{
  library(tidyverse)
  library(plyr)
  library(data.table)
  library(lubridate)
  
  library(spatial)
  library(sp)
  library(sf)
  library(spdep)
  library(sfdep)
  library(fields)
  
  library(rasterVis)
  library(raster)
  library(RColorBrewer)
  
  library(leaflet)
  library(kableExtra)
  library(shiny)
  library(plotly)
  library(htmlwidgets)
  library(shinyWidgets)
  library(shinythemes)
  library(shinycustomloader)
  library(leaflet.extras2)
  
  library(KernSmooth)
  library(ks)
  
  library(googledrive)
  library(googlesheets4)
  
  library(digest)
}


options(
  gargle_oauth_email=TRUE,
  gargle_oauth_cache = "destin-app/.secrets",
  gargle_oauth_path = NULL
)

# to be done once upon setup
# gs4_create(name="cfa-test-sheet",
#            sheets="main")

# google sheet id
sheet_id <- drive_get("cfa-test-sheet")$id

#Data.In=read.csv("data/DestinData.csv")
#setwd("./Destin_App2")
#Data.In <- fread("destin-app/data/NOAA-Observer-data.csv")
Data.In <- fread("data/NOAA-Observer-data.csv")

{noaa_data=Data.In

noaa_data <- noaa_data %>% dplyr::filter(COMMON_NAME != "" | SET_TIME_START != "") #| VESSEL_ID != "")
noaa_data$SET_TIME_START_dt <- parse_date_time(noaa_data$SET_TIME_START, "d-b-y I.M.S.p")
noaa_data$SET_TIME_END_dt <- parse_date_time(noaa_data$SET_TIME_END, "d-b-y I.M.S.p")
noaa_data$DEPART_DATE_dt <- mdy(noaa_data$DEPART_DATE)
noaa_data$LAND_DATE_dt <- mdy(noaa_data$LAND_DATE)
noaa_data$SET_YEAR <- year(noaa_data$SET_TIME_START_dt)
noaa_data$SEA_DAYS <- as.numeric(difftime(noaa_data$LAND_DATE_dt, noaa_data$DEPART_DATE_dt, units = "days"))
}

# Function to generate random alphanumeric string of the same length
random_string <- function(n) {
  paste(sample(c(0:9, letters, LETTERS), n, replace = TRUE), collapse = "")
}

# Create a function to map unique values to random strings
random_mask <- function(column) {
  # Get unique values in the column
  unique_values <- unique(column)
  
  # Generate random strings for each unique value, with the same length as the original value
  masked_values <- sapply(nchar(unique_values), random_string)
  
  # Create a mapping table (dictionary)
  value_map <- setNames(masked_values, unique_values)
  
  # Replace the original values with their masked versions
  return(value_map[column])
}

# data masking
{noaa_data$VESSEL_ID <- random_mask(noaa_data$VESSEL_ID)
noaa_data$TRIPNUMBER <- random_mask(noaa_data$TRIPNUMBER)
noaa_data$UNIQUE_RETRIEVAL <- paste(noaa_data$VESSEL_ID, noaa_data$TRIPNUMBER, noaa_data$SETNUMBER, sep="_")
}

noaa_vl_des <- noaa_data %>%
  filter(TRIP_GEAR == 'VL' & COMMON_NAME != "NOCATCH" & LON_BEGIN_SET > -89 & LAT_BEGIN_SET > 29 & !is.na(SET_TIME_START)) %>%
  mutate(Effort_ID = dense_rank(TRIPNUMBER)) %>%
  group_by(TRIPNUMBER) %>%
  mutate(Days_Report = dense_rank(as.Date(SET_DATE, format="%m/%d/%Y"))) %>%
  ungroup()

# noaa_vl_gal <- noaa_data %>%
#   filter(TRIP_GEAR == 'VL' & COMMON_NAME != "NOCATCH" & LON_BEGIN_SET < -96.5 & LAT_BEGIN_SET > 27 & !is.na(SET_TIME_START)) %>%
#   mutate(Effort_ID = dense_rank(TRIPNUMBER)) %>%
#   group_by(TRIPNUMBER) %>%
#   mutate(Days_Report = dense_rank(as.Date(SET_DATE, format="%m/%d/%Y"))) %>%
#   ungroup()

#My.metric = "Number_damaged"
#My.met.label = "Damaged Fish Caught"
#My.env = "Wind"  #Wind Current

#options("rgdal_show_exportToProj4_warnings"="none")

# #read using the sf package
#grid.sq1 <- st_read(dsn="destin-app/shapefiles", layer = "DESTIN_GRID_NOAA_1MI")
grid.sq1 <- st_read(dsn="shapefiles", layer = "DESTIN_GRID_NOAA_1MI")
#grid.sq10 <- st_read(dsn = "shapefiles", layer = "GOM_GRID_10MIN_fullgulf")
#grid.sq5 <- st_read(dsn = "shapefiles", layer = "GOM_GRID_5MIN_new")
# grid.sq2.5 <- st_read(dsn = "shapefiles", layer = "GOM_GRID_2_5MIN")
# #grid.hex5 <- st_read(dsn = paste0(wd,"/shapefiles"), layer = "GOM_HEX_5MIN")
# #grid.hex2.5 <- st_read(dsn = paste0(wd,"/shapefiles"), layer = "GOM_HEX_2_5MIN")
# 
# # Check CRS (Coordinate Reference System)
#st_crs(grid.sq10)
#st_crs(grid.sq5)

#Need to simplify to the same columns
grid.sq1=grid.sq1[1] 
names(grid.sq1)[names(grid.sq1) == "Id"] <- "GRID_ID"
gridshp=grid.sq1

boat_icon <- makeIcon(iconUrl = "www/boat2.svg",
                      iconWidth=35, iconHeight=30, 
                      iconAnchorX=15, iconAnchorY=15)
html_legend <- "<img src='boat2.svg' style='width:35px;height:30px;'> Current Location<br/>"

current_speed <- fread("data/Oceanic_current_speed.csv") %>%
  dplyr::select(Longitude, Latitude, Oceanic_current_speed_October) %>%
  rename("Speed_Oct" = "Oceanic_current_speed_October") %>%
  mutate(class_bin = case_when(
    Speed_Oct >= 0.5 ~ 3,
    Speed_Oct >= 0.3 ~ 2,
    Speed_Oct < 0.3 ~ 1
    )
  )

#summary(current_speed)

#save(gridshp, sheet_id, noaa_data, noaa_vl_des, file = "destin-app/data/preprocess.RData")
save(gridshp, sheet_id, noaa_data, noaa_vl_des, boat_icon, html_legend, current_speed, file = "data/preprocess.RData")
