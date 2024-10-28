################################################################################
# This script builds the user interface for the destin app
################################################################################

# fluidPage(
# tags$script('
#       $(document).ready(function () {
#         navigator.geolocation.getCurrentPosition(onSuccess, onError);
#               
#         function onError (err) {
#           Shiny.onInputChange("geolocation", false);
#         }
#               
#         function onSuccess (position) {
#           setTimeout(function () {
#             var coords = position.coords;
#             console.log(coords.latitude + ", " + coords.longitude);
#             Shiny.onInputChange("geolocation", true);
#             Shiny.onInputChange("lat", coords.latitude);
#             Shiny.onInputChange("long", coords.longitude);
#           }, 1100)
#         }
#       });
#               '),

# mytheme <- create_theme(
#   adminlte_color(
#     aqua = "navy"
#   )
# )

dashboardPage(skin="black", 
  dashboardHeader(
    title = div(img(src="cfa-logo.png", style = "width:50px;height:50px"), "Charter Fisherman's Association"),
    titleWidth = 400,
    tags$li(class = "dropdown", shinyauthr::logoutUI(id = "logout"), style="margin-right:15px; margin-top:10px;")
  ), # dashboard header
  dashboardSidebar(
    width = 400,
    sidebarMenu(id="sidebarid", style="white-space: normal;",
      menuItem("Map", tabName="maptab", icon=icon("map")),
      menuItem("Record New Observation", tabName="recordtab", icon=icon("pen-to-square")),
      menuItem("User Data", tabName="usertab", icon=icon("user")),
      conditionalPanel(
        'input.sidebarid == "maptab"',
        #hr(),
        #helpText(HTML("Use the following selections to update the data displayed <br>on the map.")),
        sliderTextInput("days", "Days Since Reporting", choices=seq(from=1, to=14, by=1), selected=c(3), grid=TRUE),
        radioGroupButtons("radio_current", "Display Current Intensity", choices = c("Yes", "No"), selected="No"),
        conditionalPanel("input.radio_current == 'No'",
          radioGroupButtons("radio_depred", "Display Depredation", choices=c("Total", "Sharks", "Dolphins"), selected="Total"),
          radioGroupButtons("radio_layer", "Layer Style", choices=c("Intensity (grid)", "Density (heat)"), selected="Intensity (grid)"),
        ),
        actionButton("update", "Update Map", icon=icon("refresh"), class="btn btn-primary", style = "color: white;")
      ), # conditional panel - map tab
      conditionalPanel(
        'input.sidebarid == "usertab"',
        radioGroupButtons("radio_points", "Display Layer", choices = c("Species Encountered", "Depredation Intensity", "Current Intensity"), selected="Species Encountered", direction="vertical")
      ) # conditional panel - user tab
    ) # sidebar menu
  ), # dashboard sidebar
  dashboardBody(#use_theme(mytheme), #useShinyjs(), 
    shinyauthr::loginUI("login"),
    #uiOutput("main_ui")
    div(id="main_ui",
    tabItems(
      tabItem(tabName="maptab",
        fluidRow(
          box(
            width=8, status="primary", #title="About the Data",
            HTML("ABOUT THE DATA<br>Displayed is a combination of data collected via the NOAA Observer Program and manually recorded observations.
              The total number of observations refers to the number of points informing the map. Grid cells are 1 mi by 1 mi and reflect the average
              value of points located inside each cell. Current intensity was calculated using averages of the current speed (m/s) from 2000-2017
              during the month of October.")),
          infoBoxOutput("text_obs", width=4),
          #infoBoxOutput()
        ), # fluid row
        fluidRow(
          box(
            width=12, title = "Map", status="primary", solidHeader=TRUE, collapsible=FALSE, #height=800,
            withLoader(leafletOutput("examplemap", height = 740), type="html", loader="loader4")#,
            # DTOutput("trouble_table"),
            # DTOutput("trouble_table2"),
            # DTOutput("trouble_table3")
          ) # box -map
        ) # fluid row
      ), #tab item
      tabItem(tabName="recordtab",
        fluidRow(
        box(width=12, status="primary", title="Record New Observation", solidHeader=TRUE,
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
          ), # fluid row
          fluidRow(
            column(width=4, radioButtons("check_loc", label = "Use Current Location", c("Yes", "No"), selected = "Yes")),
            column(width=4, conditionalPanel(condition="input.check_loc == 'No'",
                                              textInput("text_long", label = "Longitude"))),
            column(width=4, conditionalPanel(condition="input.check_loc == 'No'",
                                              textInput("text_lat", label = "Latitude")))
          ), #fluid row
          fluidRow(
            column(width=8, textInput("text_notes", label="Notes"))
          ), # fluid row
          actionButton("submit", "Submit Data", class="btn btn-primary", style = "color: white;")
        )#, # box
        ) # fluidrow
        # box(
        #   width=4, status="info",
        # )
      ), #tab item
      tabItem(tabName="usertab",
        fluidRow(
        box(
          width=12, title="User Data", status="primary", solidHeader=TRUE, collapsible=TRUE,
          div(DTOutput("user_data"), style="overflow-y: auto; height=300px")
        ) # box -table
        ),
        fluidRow(
        box(
          width=12, title = "Map", status="primary", solidHeader=TRUE, collapsible=TRUE, #height=600,
          withLoader(leafletOutput("user_map", height = 540), type="html", loader="loader4")
        ) # box - user mpa
        ) # fluidrow
      ) #tab item
    ) # tab items
    ) # div main ui 
  ) # dashboard body
) #dashboard page  

################ OLD UI ##################
#                 tabPanel(
#                       fluidPage(
#                         #shinythemes::themeSelector(),
#                         sidebarLayout(
#                           sidebarPanel(helpText("Use the following selections to update the data displayed on the map."),
#                                                 strong("Total Observations"),
#                                                 verbatimTextOutput("text_obs", placeholder=FALSE),
#                                                 p(" "),
#                                                 sliderTextInput("days", "Days since Reporting", choices=seq(from=1, to=14, by=1), selected=c(3), grid=TRUE),
#                                                 radioGroupButtons("radio_current", "Display Current Intensity", choices = c("Yes", "No"), selected="No"),
#                                                 fluidRow(
#                                                   column(width=6, radioGroupButtons("radio_depred", "Display Depredation", choices=c("Total", "Sharks", "Dolphins"), selected="Total")),
#                                                   column(width=6, radioGroupButtons("radio_layer", "Layer Style", choices=c("Intensity (grid)", "Density (heat)"), selected="Intensity (grid)"))
#                                                 ),
#                                                 actionButton("update", "Update Map", icon=icon("refresh"), class="btn btn-primary")
#                                     ), #sidebarPanel
#                                     mainPanel(
#                                        tabsetPanel(
#                                          tabPanel("Map", icon = icon("map"),
#                                            withLoader(leafletOutput("examplemap", height = "85vh"), type="html", loader="loader4")
#                                            ),
#                                          tabPanel("Record New Observation", icon=icon("pen-to-square"),
#                                            wellPanel(
#                                              helpText("Please enter your observations using the following inputs."),
#                                              fluidRow(
#                                                column(width=4, pickerInput("select_current", label = "Current Intensity", choices = c("None", "Moderate", "High"), selected = "None",
#                                                                            choicesOpt = list(
#                                                                              content = sprintf("<span class='label label-%s'>%s</span>",
#                                                                                                c("success", "warning", "danger"), 
#                                                                                                c("None", "Moderate", "High"))
#                                                                            ))),
#                                                column(width=4, pickerInput("select_depred", label = "Depredation Intensity", choices = c("None", "Moderate", "High"), selected = "None",
#                                                                            choicesOpt = list(
#                                                                              content = sprintf("<span class='label label-%s'>%s</span>",
#                                                                                                c("success", "warning", "danger"), 
#                                                                                                c("None", "Moderate", "High"))
#                                                                            ))),
#                                                column(width=4, pickerInput("select_species", label = "Species Encountered", choices = c("None","Shark", "Dolphin"), selected = "None",
#                                                                            choicesOpt = list(
#                                                                              content = sprintf("<span class='label label-%s'>%s</span>",
#                                                                                                c("success", "primary", "info"), 
#                                                                                                c("None", "Shark", "Dolphin"))
#                                                                            )))
#                                              ),
#                                              fluidRow(
#                                                column(width=4, radioButtons("check_loc", label = "Use Current Location", c("Yes", "No"), selected = "Yes")),
#                                                column(width=4, 
#                                                       conditionalPanel(condition="input.check_loc == 'No'",
#                                                                        textInput("text_long", label = "Longitude"))),
#                                                column(width=4,
#                                                       conditionalPanel(condition="input.check_loc == 'No'",
#                                                       textInput("text_lat", label = "Latitude")))
#                                              ), #fluidRow
#                                              fluidRow(
#                                                column(width=8, textInput("text_notes", label="Notes"))
#                                              ),
#                                              actionButton("submit", "Submit Data", class="btn btn-info")
#                                            )#, #wellPanel
#                                            
#                                            #DTOutput("sheet_data")
#                                            
#                                            ), #tabPanel 2
#                                          tabPanel("User Data", icon=icon("user"),
#                                                   
#                                             DTOutput("user_data") 
#                                          ) #tabPanel 3
#                                         ) #tabsetPanel
#                                      ) #mainPanel
#                                    ) #sidebarLayout
#                         ) # fluidPage
#                       
#                     )#, #tabPanel