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
                        #shinythemes::themeSelector(),
                        sidebarLayout(
                          sidebarPanel(helpText("Use the following selections to update the data displayed on the map."),
                                                strong("Total Observations"),
                                                verbatimTextOutput("text_obs", placeholder=FALSE),
                                                p(" "),
                                                sliderTextInput("days", "Days since Reporting", choices=seq(from=1, to=14, by=1), selected=c(2), grid=TRUE),
                                                radioGroupButtons("radio_current", "Display Current Intensity", choices = c("Yes", "No"), selected="No"),
                                                fluidRow(
                                                  column(width=6, radioGroupButtons("radio_depred", "Display Depredation", choices=c("Total", "Sharks", "Dolphins"), selected="Total")),
                                                  column(width=6, radioGroupButtons("radio_layer", "Layer Style", choices=c("Intensity (grid)", "Density (heat)"), selected="Intensity (grid)"))
                                                ),
                                                actionButton("update", "Update Map", icon=icon("refresh"), class="btn btn-primary")
                                    ), #sidebarPanel
                                    mainPanel(
                                       tabsetPanel(
                                         tabPanel("Map", icon = icon("map"),
                                           withLoader(leafletOutput("examplemap", height = "85vh"), type="html", loader="loader4")
                                           ),
                                         tabPanel("Record New Observation", icon=icon("pen-to-square"),
                                           wellPanel(
                                             helpText("Please enter your observations using the following inputs."),
                                             fluidRow(
                                               column(width=4, pickerInput("select_current", label = "Current Intensity", choices = c("None", "Moderate", "High"), selected = "None",
                                                                           choicesOpt = list(
                                                                             content = sprintf("<span class='label label-%s'>%s</span>",
                                                                                               c("success", "warning", "danger"), 
                                                                                               c("None", "Moderate", "High"))
                                                                           ))),
                                               column(width=4, pickerInput("select_depred", label = "Depredation Intensity", choices = c("None", "Moderate", "High"), selected = "None",
                                                                           choicesOpt = list(
                                                                             content = sprintf("<span class='label label-%s'>%s</span>",
                                                                                               c("success", "warning", "danger"), 
                                                                                               c("None", "Moderate", "High"))
                                                                           ))),
                                               column(width=4, pickerInput("select_species", label = "Species Encountered", choices = c("None","Shark", "Dolphin"), selected = "None",
                                                                           choicesOpt = list(
                                                                             content = sprintf("<span class='label label-%s'>%s</span>",
                                                                                               c("success", "primary", "info"), 
                                                                                               c("None", "Shark", "Dolphin"))
                                                                           )))
                                             ),
                                             fluidRow(
                                               column(width=4, radioButtons("check_loc", label = "Use Current Location", c("Yes", "No"), selected = "Yes")),
                                               column(width=4, 
                                                      conditionalPanel(condition="input.check_loc == 'No'",
                                                                       textInput("text_long", label = "Longitude"))),
                                               column(width=4,
                                                      conditionalPanel(condition="input.check_loc == 'No'",
                                                      textInput("text_lat", label = "Latitude")))
                                             ), #fluidRow
                                             fluidRow(
                                               column(width=8, textInput("text_notes", label="Notes"))
                                             ),
                                             actionButton("submit", "Submit Data", class="btn btn-info")
                                           ), #wellPanel
                                           
                                           DTOutput("sheet_data")
                                           
                                           ), #tabPanel 2
                                         tabPanel("User Data", icon=icon("user"),
                                                  
                                            DTOutput("user_data") 
                                         ) #tabPanel 3
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
