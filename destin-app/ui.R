################################################################################
# This script builds the user interface for the destin app
# capabilities include: viewing maps and customization options
################################################################################

# fluidPage(
#   tags$script('
#     function getLocation() {
#       if (navigator.geolocation) {
#         navigator.geolocation.getCurrentPosition(onSuccess, onError);
#       } else {
#         alert("Geolocation is not supported by this browser.");
#       }
#     }
# 
#     function onSuccess(position) {
#       Shiny.onInputChange("user_lat", position.coords.latitude);
#       Shiny.onInputChange("user_long", position.coords.longitude);
#     }
# 
#     function onError(error) {
#       console.warn("ERROR(" + error.code + "): " + error.message);
#     }
# 
#     // Call the function when the page loads
#     $(document).on("shiny:connected", function() {
#       getLocation();
#     });
#   '),

  navbarPage(title=div("Bycatch Hotspot Initiative", style = "text-align: center; margin-top: 12px;"), theme = shinytheme("flatly"), 
   
             tabPanel(title=div(img(src="cfa-logo.png", style = "width:50px;height:50px"), "Charter Fisherman's Association"),
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(helpText("Use the following selections to update the data displayed on the map."),
                                                "Total Observations:",
                                                verbatimTextOutput("text_obs", placeholder=FALSE),
                                                p(" "),
                                                #selectInput("region", "Region", choices = c("Destin", "Galveston"), selected="Destin"),
                                                sliderTextInput("days", "Time Range: Days since Reporting", choices=seq(from=max(noaa_vl_des$Days_Report), to=min(noaa_vl_des$Days_Report), by=-1), selected=c(2,1), grid=TRUE),
                                                sliderInput("effort", "Vessels Reporting", max=max(noaa_vl_des$Effort_ID), min=min(noaa_vl_des$Effort_ID), value=c(max(noaa_vl_des$Effort_ID)), step=5, round=TRUE),
                                                #checkboxGroupInput("seasons", "Season", choices = c("Spring", "Summer", "Fall", "Winter"), selected = c("Spring", "Summer", "Fall", "Winter")),
                                                #radioButtons("show_maps", "Display Data", c("Catch Events", "Catch Per Unit Effort*"), selected = "Catch Events"),
                                                #checkboxGroupInput("rasters", "Environmental**", choices = c("Spring", "Summer", "Fall", "Winter"), selected = NULL),
                                                #radioButtons("grid.scale", "Catch per Unit Effort Grid Scale", choices = c("10' square" = "grid.sq10", "5' square" = "grid.sq5", "2.5' square" = "grid.sq2.5")),
                                                actionButton("update", "Update Map")
                                    ), #sidebarPanel
                                    mainPanel(
                                       tabsetPanel(
                                         tabPanel("Map",
                                           withLoader(leafletOutput("examplemap", height = "85vh"), type="html", loader="loader4")
                                           ),
                                         tabPanel("Record New Observation",
                                           wellPanel(
                                             helpText("Please enter your observations using the following inputs."),
                                             fluidRow(
                                               column(width=4, selectInput("select_current", label = "Current Intensity", choices = c("None", "Moderate", "High"), selected = "None")),
                                               column(width=4, selectInput("select_depred", label = "Depredation Intensity", choices = c("None", "Moderate", "High"), selected = "None")),
                                               column(width=4, selectInput("select_species", label = "Species Encountered", choices = c("None","Shark", "Dolphin"), selected = "None"))
                                             ),
                                             fluidRow(
                                               column(width=4, radioButtons("check_loc", label = "Use Current Location", c("Yes", "No"), selected = "Yes")),
                                               column(width=4, textInput("text_lat", label = "Latitude")),
                                               column(width=4, textInput("text_long", label = "Longitude"))
                                             ), #fluidRow
                                             fluidRow(
                                               column(width=8, textInput("text_notes", label="Notes"))
                                             ),
                                             actionButton("submit", "Submit Data")
                                           ), #wellPanel
                                           
                                           tableOutput("sheet_data")
                                           
                                           ) #tabPanel 2
                                                                     #textOutput("radio.txt"),
                                                                     #tableOutput("trouble"),
                                                                       #tableOutput("trouble2")
                                       
                                        ) #tabsetPanel
                                     ) #mainPanel
                                   ) #sidebarLayout
                      ) # fluidPage
                      
              )#, #tabPanel
             
   ) #navbarPage
  
#) #fluid page
