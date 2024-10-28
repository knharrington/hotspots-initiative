################################################################################
# This script is the global environment for the destin app
################################################################################

## FUNCTIONS TO ADD
##    -modify or delete previous entries
##    -find user's location for recording obs and displaying boat

# load packages
{
  library(tidyverse)
  #library(plyr)
  library(data.table)
  library(lubridate)
  
  library(spatial)
  library(sp)
  library(sf)
  library(spdep)
  library(sfdep)
  library(fields)
  
  #library(rasterVis)
  #library(raster)
  library(RColorBrewer)

  library(leaflet)
  library(kableExtra)
  library(shiny)
  library(plotly)
  library(htmlwidgets)
  library(shinyWidgets)
  #library(shinythemes)
  library(shinycustomloader)
  library(leaflet.extras)
  library(leaflet.extras2)
  library(leafem)
  library(DT)
  library(shinydashboard)
  #library(bs4Dash)
  #library(fresh)
  
  #library(KernSmooth)
  #library(ks)
  
  library(googledrive)
  library(googlesheets4)
  
  library(digest)
  
  library(shinyauthr)
  library(sodium)
}

load("preprocess.RData")

googledrive::drive_auth(cache = ".secrets", email = "knharrington@mote.org")
googlesheets4::gs4_auth(token = drive_token())

