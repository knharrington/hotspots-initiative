# This script preprocesses data needed in the example Charter Fisherman's Association web-based application. 

# Load packages
{
  library(tidyverse)
  library(data.table)
  library(lubridate)
  library(sf)
  library(leaflet)
  library(googledrive)
  library(googlesheets4)
  library(digest)
}

{
# Set global options for handling Google Sheets token
options(
  gargle_oauth_email=TRUE,
  gargle_oauth_cache = "/.secrets",
  gargle_oauth_path = NULL
)

# To be done once upon initializing the desired Google Sheet
# gs4_create(name="cfa-test-sheet",
#            sheets="main")

# Assign Google Sheet ID
sheet_id <- drive_get("cfa-test-sheet")$id

# Read NOAA Observer data and format columns
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

# Mask columns with sensitive information
{noaa_data$VESSEL_ID <- random_mask(noaa_data$VESSEL_ID)
noaa_data$TRIPNUMBER <- random_mask(noaa_data$TRIPNUMBER)
noaa_data$UNIQUE_RETRIEVAL <- paste(noaa_data$VESSEL_ID, noaa_data$TRIPNUMBER, noaa_data$SETNUMBER, sep="_")
}

# Filter NOAA observer data for vertical line trips
noaa_vl_des <- noaa_data %>%
  filter(TRIP_GEAR == 'VL' & COMMON_NAME != "NOCATCH" & !is.na(SET_TIME_START) & LAT_BEGIN_SET > 25.8) %>% #& LON_BEGIN_SET > -89 & LAT_BEGIN_SET > 29
  mutate(Effort_ID = dense_rank(TRIPNUMBER)) %>%
  group_by(TRIPNUMBER) %>%
  mutate(Days_Report = dense_rank(as.Date(SET_DATE, format="%m/%d/%Y"))) %>%
  ungroup()

# Load shapefile for 3mi by 3mi grid
gridshp <- st_read(dsn="shapefiles", layer = "GULF_GRID_3_MILE_SMOOTH")

# Determine bounding box for shapefile limits
bbox <- st_bbox(gridshp)

# Create icon for boat and make static legend
boat_icon <- makeIcon(iconUrl = "www/boat2.svg",
                      iconWidth=35, iconHeight=30, 
                      iconAnchorX=15, iconAnchorY=15)
html_legend <- "<img src='boat2.svg' style='width:35px;height:30px;'> Current Location<br/>"

# Read historic current speed data
current_speed <- fread("data/Oceanic_current_speed.csv") %>%
  dplyr::select(Longitude, Latitude, Oceanic_current_speed_October) %>%
  rename("Speed_Oct" = "Oceanic_current_speed_October")

# Convert the current speed data frame to a spatial object
current_speed_sf <- st_as_sf(current_speed, coords = c("Longitude", "Latitude"), crs = st_crs(gridshp))

# Perform the spatial join for current speed data and grid
grid_join_c <- setDT(st_join(current_speed_sf, gridshp, join = st_intersects))

# Assign bins to current speed data
filtered_data_c <- grid_join_c %>%
    group_by(GRID_ID) %>%
    summarise(
      speed_mean = mean(Speed_Oct, na.rm = TRUE)
    ) %>%
  mutate(class_bin = case_when(
    speed_mean >= 0.5 ~ 3,
    speed_mean >= 0.3 ~ 2,
    speed_mean < 0.3 ~ 1
  ))

# Determine grid values for current speed data
gridvalues_c <- st_as_sf(merge(x = gridshp, y = filtered_data_c, by = "GRID_ID", all.x = FALSE))

# create an example for a fixed userbase and hash the passwords
user_base <- tibble::tibble(
  user = c("cfemm-admin", "test-user"),
  password = purrr::map_chr(c("temp", "hotspots"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("Admin Account", "Test User")
)

# Save variables for use in the server
save(gridshp, bbox, sheet_id, noaa_data, noaa_vl_des, boat_icon, html_legend, user_base, file = "data/preprocess.RData")
}
################################################################################
### Non-reactive world

# # filter dataset based on user inputs
# filtered_prop <- noaa_vl_des %>% filter(Days_Report <= 14)
# 
# noaa_vl_prop <- filtered_prop %>%
#     filter(!is.na(LAT_BEGIN_SET) & !is.na(LON_BEGIN_SET)) %>%
#     group_by(UNIQUE_RETRIEVAL, COMMON_NAME, VESSEL_ID) %>%
#     mutate(
#       NUM_ALIVE = sum(NUM_FISH[CONDITION %in% c('ALIVE', 'ALIVE BAURO - STOM/BLADDER', 'ALIVE BAURO - EYES', 'ALIVE BAURO - BOTH')]),
#       NUM_DEAD = sum(NUM_FISH[CONDITION == 'DEAD']),
#       UNIQUE_RET_LAT = round(mean(LAT_BEGIN_SET), 6),
#       UNIQUE_RET_LON = round(mean(LON_BEGIN_SET), 6),
#       PROP_DEAD = NUM_DEAD / NUM_ALIVE,
#       NUM_SHARKS = sum(NUM_FISH[grepl("SHARK", COMMON_NAME)])
#     )
# 
# # Convert the data frame to a spatial object
# noaa_vl_prop_sf <- st_as_sf(noaa_vl_prop, coords = c("UNIQUE_RET_LON", "UNIQUE_RET_LAT"), crs = st_crs(gridshp))
# 
# # Perform the spatial join
# grid_join <- setDT(st_join(noaa_vl_prop_sf, gridshp, join = st_intersects))
# 
# # filter and aggregate the data if the cells contain at least 3 vessels contributing
# filtered_data <- grid_join %>%
#     group_by(GRID_ID) %>%
#     summarise(
#       PROP_DEAD.mean = mean(PROP_DEAD, na.rm = TRUE),  # Calculate the mean PROP_DEAD for each grid
#       NUM_SHARKS.mean = mean(NUM_SHARKS, na.rm=TRUE),
#       num_points = n(),  # Count the number of points in each grid
#       unique_vessel_ids = n_distinct(VESSEL_ID)  # Count unique VESSEL_IDs in each grid
#     ) %>%
#     filter(num_points >= 3 & unique_vessel_ids >= 3)  # Apply the filtering condition: at least 3 points and at least 3 unique VESSEL_IDs
# 
# filtered_data2 <- filtered_data %>%
#     filter(GRID_ID != "NA",
#            PROP_DEAD.mean != 'Inf') %>%
#     mutate(
#       classification = case_when(
#         PROP_DEAD.mean == 0 ~ 'None',
#         PROP_DEAD.mean <= 0.10 ~ 'Moderate',
#         PROP_DEAD.mean > 0.10 ~ 'High'
#       ),
#       current_speed = "None",
#       sharks_dep = case_when(
#         NUM_SHARKS.mean == 0 ~ "None",
#         NUM_SHARKS.mean <= 2 ~ "Moderate",
#         NUM_SHARKS.mean > 3 ~ "High"
#       ),
#       dolphins_dep = "None"
#     )
# # Merge the results back with the grid shapefile
# gridvalues <- st_as_sf(merge(x = gridshp, y = filtered_data2, by = "GRID_ID", all.x = FALSE))
# 
# grid_centroids <- st_centroid(gridvalues)
