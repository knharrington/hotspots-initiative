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
  
  #library(rasterVis)
  #library(raster)
  library(RColorBrewer)

  library(leaflet)
  library(kableExtra)
  library(shiny)
  library(plotly)
  library(htmlwidgets)
  library(shinyWidgets)
  library(shinythemes)
  library(shinycustomloader)
  library(leaflet.extras)
  library(leaflet.extras2)
  library(leafem)
  library(DT)
  
  #library(KernSmooth)
  #library(ks)
  
  library(googledrive)
  library(googlesheets4)
  
  library(digest)
}

load("data/preprocess.RData")

