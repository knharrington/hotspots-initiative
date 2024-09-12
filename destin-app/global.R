################################################################################
# This script is the global environment for the destin app
# It stores objects needed in the sever and/or ui

################################################################################

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
Data.In <- fread("data/NOAA-Observer-data.csv")

noaa_data=Data.In

noaa_data <- noaa_data %>% dplyr::filter(COMMON_NAME != "" | SET_TIME_START != "") #| VESSEL_ID != "")
noaa_data$SET_TIME_START_dt <- parse_date_time(noaa_data$SET_TIME_START, "d-b-y I.M.S.p")
noaa_data$SET_TIME_END_dt <- parse_date_time(noaa_data$SET_TIME_END, "d-b-y I.M.S.p")
noaa_data$DEPART_DATE_dt <- mdy(noaa_data$DEPART_DATE)
noaa_data$LAND_DATE_dt <- mdy(noaa_data$LAND_DATE)
noaa_data$SET_YEAR <- year(noaa_data$SET_TIME_START_dt)
noaa_data$UNIQUE_RETRIEVAL <- paste(noaa_data$VESSEL_ID, noaa_data$TRIPNUMBER, noaa_data$SETNUMBER, sep="_")
noaa_data$SEA_DAYS <- as.numeric(difftime(noaa_data$LAND_DATE_dt, noaa_data$DEPART_DATE_dt, units = "days"))

# noaa_data <- noaa_data %>%
#   mutate(Effort_ID = dense_rank(TRIPNUMBER)) %>%
#   group_by(TRIPNUMBER) %>%
#   mutate(Days_Report = dense_rank(as.Date(SET_DATE, format="%m/%d/%Y"))) %>%
#   ungroup()

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


# noaa_vl_prop <- noaa_vl_des %>%
#   group_by(UNIQUE_RETRIEVAL, COMMON_NAME) %>%
#   summarise(
#     NUM_ALIVE = sum(NUM_FISH[CONDITION %in% c('ALIVE', 'ALIVE BAURO - STOM/BLADDER', 'ALIVE BAURO - EYES', 'ALIVE BAURO - BOTH')]),
#     NUM_DEAD = sum(NUM_FISH[CONDITION == 'DEAD']),
#     UNIQUE_RET_LAT = mean(LAT_BEGIN_SET),
#     UNIQUE_RET_LON = mean(LON_BEGIN_SET)
#   ) %>%
#   mutate(PROP_DEAD = NUM_DEAD / NUM_ALIVE) %>%
#   ungroup()
#noaa_vl_prop$UNIQUE_RET_LAT <- format(noaa_vl_prop$UNIQUE_RET_LAT, scientific = FALSE)
#noaa_vl_prop$UNIQUE_RET_LON <- format(noaa_vl_prop$UNIQUE_RET_LON, scientific = FALSE)
# 
# # # Check if there are non-numeric values in the coordinates columns
# # non_numeric_lon <- suppressWarnings(as.numeric(noaa_vl_prop$UNIQUE_RET_LON))
# # non_numeric_lat <- suppressWarnings(as.numeric(noaa_vl_prop$UNIQUE_RET_LAT))
# # 
# # # Identify rows with non-numeric values in coordinates
# # invalid_coords <- is.na(non_numeric_lon) | is.na(non_numeric_lat)
# # 
# # # Print the number of invalid coordinates
# # print(sum(invalid_coords))
# # 
# # # Remove rows with invalid coordinates
# # noaa_vl_prop_clean <- noaa_vl_prop[!invalid_coords, ]
# # 
# # # Convert the cleaned columns to numeric (if needed)
# # noaa_vl_prop_clean$UNIQUE_RET_LON <- as.numeric(noaa_vl_prop_clean$UNIQUE_RET_LON)
# # noaa_vl_prop_clean$UNIQUE_RET_LAT <- as.numeric(noaa_vl_prop_clean$UNIQUE_RET_LAT)
# 
# # Convert the data frame to a spatial object
# noaa_vl_prop_sf <- st_as_sf(noaa_vl_prop, coords = c("UNIQUE_RET_LON", "UNIQUE_RET_LAT"), crs = st_crs(gridshp))
# 
# # Perform the spatial join
# grid_join <- st_join(noaa_vl_prop_sf, gridshp, join = st_intersects)
# grid_join_dt <- setDT(grid_join)
# 
# # Filter and aggregate the data
# filtered_data <- grid_join_dt[!is.na(PROP_DEAD), #& COMMON_NAME == 'SNAPPER, RED',
#                               .(PROP_DEAD.mean = mean(PROP_DEAD, na.rm = TRUE)),
#                               by = GRID_ID]
# filtered_data <- filtered_data[!is.na(GRID_ID)]
# filtered_data <- filtered_data %>%
#   filter(PROP_DEAD.mean != 'Inf') %>%
#   mutate(Classification = case_when(
#     PROP_DEAD.mean == 0 ~ 'None',
#     PROP_DEAD.mean <= 0.10 ~ 'Moderate',
#     PROP_DEAD.mean > 0.10 ~ 'High'
#   ))
# 
# # Merge the results back with the grid shapefile
# gridvalues <- st_as_sf(merge(x = gridshp, y = filtered_data, by = "GRID_ID", all.x = FALSE))
# print(gridvalues)
# 
# gridvalues_dt <- setDT(gridvalues)
# grid_classes <- gridvalues_dt %>%
#   group_by(Classification) %>%
#   summarise(Total = length(Classification)) %>%
#   arrange(desc(Total))
# 
# # map results
# nrcolors <- c("#7e911d", "#fdcb2f", "#ae002d")
# nrpro_levels <- c("None", "Moderate", "High")
# nrpal3 <- colorFactor(nrcolors, levels=nrpro_levels)
# nrpopper <- paste0("<strong>Proportion Dead: </strong>", round(gridvalues$PROP_DEAD.mean, digits = 2))
# nrleaflet <- leaflet(data = gridvalues) %>%
#   addProviderTiles("Esri.OceanBasemap", options = providerTileOptions(variant = "Ocean/World_Ocean_Base")) %>%
#   addProviderTiles("Esri.OceanBasemap", options = providerTileOptions(variant = "Ocean/World_Ocean_Reference")) %>%
#   addPolygons(
#     fillColor = ~nrpal3(Classification),
#     weight = 1,
#     opacity = 1,
#     color = 'transparent',
#     fillOpacity = 1,
#     popup = nrpopper,
#     highlightOptions = highlightOptions(
#       weight = 1,
#       color = 'white',
#       fillOpacity = 0.7,
#       bringToFront = FALSE)   ) %>%
#   # addMarkers(lng=-86.4958,
#   #            lat=30.3935) %>%
#   # addMarkers(lng=-94.7977,
#   #            lat=29.3013) %>%
#   leaflet::addLegend(
#     pal = nrpal3,
#     values = ~Classification,
#     opacity = 1,
#     title = "Classification",
#     position = "topright") %>%
#   addGraticule(interval=1)


# grid.sq10=grid.sq10[1] 
# names(grid.sq10)[names(grid.sq10) == "Id"] <- "GRID_ID"
# 
# grid.sq5=grid.sq5[1]
# names(grid.sq5)[names(grid.sq5) == "Id"] <- "GRID_ID"
# 
# grid.sq2.5=grid.sq2.5[1]
# names(grid.sq2.5)[names(grid.sq2.5) == "Id"] <- "GRID_ID"
# 
# #grid.hex5=grid.hex5[3]
# 
# #grid.hex2.5=grid.hex2.5[3]
# 
# 
# # Environmental rasters
# # Current <- read.csv("data/Oceanic_current_speed.csv", sep = ";")  #Monthly!!!!
# # Current$Oceanic_current_speed_Spring = rowMeans(Current[,c("Oceanic_current_speed_March","Oceanic_current_speed_April","Oceanic_current_speed_May")])
# # Current$Oceanic_current_speed_Summer = rowMeans(Current[,c("Oceanic_current_speed_June","Oceanic_current_speed_July","Oceanic_current_speed_August")])
# # Current$Oceanic_current_speed_Fall = rowMeans(Current[,c("Oceanic_current_speed_September","Oceanic_current_speed_October","Oceanic_current_speed_November")])
# # Current$Oceanic_current_speed_Winter = rowMeans(Current[,c("Oceanic_current_speed_December","Oceanic_current_speed_January","Oceanic_current_speed_February")])
# 
# Wind <- read.csv("data/Wind_speed.csv", sep = ";")                #Season
# 
# if (My.env == "Wind") {Shiny.env = Wind}
# #if (My.env == "Current") {Shiny.env = Current}
# 
# #names(My.env)[names(My.env) %like% "Spring"]
# 
# 
# coordinates(Shiny.env) <- c("Longitude", "Latitude")
# extentval <- extent(min(Shiny.env$Longitude),
#                     max(Shiny.env$Longitude),
#                     min(Shiny.env$Latitude),
#                     max(Shiny.env$Latitude))
# r <- raster(extentval, resolution = c(0.18, 0.18)) # 10 min is 0.16667
# 
# env_spring <- rasterize(Shiny.env, r, field = paste(names(Shiny.env)[names(Shiny.env) %like% "Spring"])) #paste(names(My.env)[names(My.env) %like% "Spring"]))
# crs(env_spring) <- "+proj=longlat +datum=WGS84"
# 
# env_summer <- rasterize(Shiny.env, r, field = paste(names(Shiny.env)[names(Shiny.env) %like% "Summer"]))
# crs(env_summer) <- "+proj=longlat +datum=WGS84"
# 
# env_fall <- rasterize(Shiny.env, r, field = paste(names(Shiny.env)[names(Shiny.env) %like% "Fall"]))
# crs(env_fall) <- "+proj=longlat +datum=WGS84"
# 
# env_winter <- rasterize(Shiny.env, r, field = paste(names(Shiny.env)[names(Shiny.env) %like% "Winter"]))
# crs(env_winter) <- "+proj=longlat +datum=WGS84"
# 
# pal_env_sp <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(env_spring),
#                           na.color = "transparent")
# pal_env_su <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(env_summer),
#                           na.color = "transparent")
# pal_env_fa <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(env_fall),
#                           na.color = "transparent")
# pal_env_wi <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(env_winter),
#                           na.color = "transparent")

#content <- "https://raw.githubusercontent.com/onaci/leaflet-velocity/master/demo/wind-global.json"
#content2 <- "https://raw.githubusercontent.com/onaci/leaflet-velocity/master/demo/wind-global.json"

