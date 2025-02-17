################################################################################
# This script is the global environment for the CFA app
################################################################################

# Load packages
{
  library(tidyverse)
  library(data.table)
  library(lubridate)
  
  library(spatial)
  library(sp)
  library(sf)
  library(spdep)
  library(sfdep)
  library(fields)
  library(RColorBrewer)

  library(leaflet)
  library(kableExtra)
  library(shiny)
  library(plotly)
  library(htmlwidgets)
  library(shinyWidgets)
  library(shinycustomloader)
  library(leaflet.extras)
  library(leaflet.extras2)
  library(leafem)
  library(DT)
  library(shinydashboard)
  
  library(googledrive)
  library(googlesheets4)
  
  library(digest)
  
  library(shinyauthr)
  library(sodium)
}

# Load data made in the preprocessing file
load("preprocess.RData")

# Set Google sheet information
googledrive::drive_auth(cache = ".secrets", email = "knharrington@mote.org")
googlesheets4::gs4_auth(token = drive_token())

