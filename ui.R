################################################################################
# This script builds the user interface for the CFA app
################################################################################

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
      menuItem("Submit Feedback", icon=icon("comment"), href="https://forms.gle/MgGVi5ZKF4pCYBuF9"),
      conditionalPanel(
        'input.sidebarid == "maptab"',
        sliderTextInput("days", "Days Since Reporting", choices=seq(from=1, to=14, by=1), selected=c(3), grid=TRUE),
        radioGroupButtons("radio_current", "Display Current Intensity", choices = c("Yes", "No"), selected="No"),
        conditionalPanel("input.radio_current == 'No'",
          radioGroupButtons("radio_depred", "Display Depredation", choices=c("Total", "Sharks", "Dolphins"), selected="Total"),
          radioGroupButtons("radio_layer", "Layer Style", choices=c("Intensity (grid)", "Density (heat)"), selected="Intensity (grid)"),
        ),
        awesomeCheckbox("show_markers", label="Show Events", value=TRUE),
        actionButton("update", "Update Map", icon=icon("refresh"), class="btn btn-primary", style = "color: white;")
      ), # conditional panel - map tab
      conditionalPanel(
        'input.sidebarid == "usertab"',
        awesomeRadio("display_all", label="Display All Data", choices = c("Yes", "No"), selected = "No", inline=TRUE),
        conditionalPanel('input.display_all == "No"',
          sliderTextInput("days_u", "Days Since Reporting", choices=seq(from=1, to=14, by=1), selected=c(3), grid=TRUE)
        ),  
        radioGroupButtons("radio_points", "Display Layer", 
                          choices = c("Species Encountered", "Depredation Intensity", "Current Intensity"), 
                          selected="Species Encountered", direction="vertical"),
        awesomeCheckbox("show_markers_u", label="Show Events", value=TRUE),
        actionButton("update_u", "Update Map", icon=icon("refresh"), class="btn btn-primary", style = "color: white;")
      ) # conditional panel - user tab
    ) # sidebar menu
  ), # dashboard sidebar
  
  dashboardBody(#use_theme(mytheme), #useShinyjs(), 
    
    tags$head(
    tags$script(HTML('
      $(document).ready(function () {
        navigator.geolocation.getCurrentPosition(onSuccess, onError);

        function onError (err) {
        Shiny.onInputChange("geolocation", false);
        }

       function onSuccess (position) {
          setTimeout(function () {
              var coords = position.coords;
              console.log(coords.latitude + ", " + coords.longitude);
              Shiny.onInputChange("geolocation", true);
              Shiny.onInputChange("lat", coords.latitude);
              Shiny.onInputChange("long", coords.longitude);
          }, 1100)
      }
      });
    '))),
    
    shinyauthr::loginUI("login"),
    #uiOutput("main_ui")
    div(id="main_ui",
    tabItems(
      tabItem(tabName="maptab",
        fluidRow(
          box(
            width=8, status="primary",
            div(
              style = "height: 70px; overflow-y: auto; padding-right: 10px;",
              HTML("ABOUT THE DATA<br>Displayed is a combination of data collected via the NOAA Observer Program and manually recorded observations.
                    The total number of observations refers to the number of points informing the map. Grid cells are 3 mi by 3 mi and reflect the average
                    value of points located inside each cell. Default bottom current intensity observations were calculated using averages of the current speed 
                    (m/s) from 2000-2017 during the month of October from GRIIDC. Surface current vectors are near real-time and provided by the 
                    Gulf of America Coastal Ocean Observing System.")
            )
            ), # box
          infoBoxOutput("text_obs", width=4),
          #infoBoxOutput()
        ), # fluid row
        fluidRow(
          box(
            width=12, title = "Map", status="primary", solidHeader=TRUE, collapsible=FALSE, 
            withLoader(leafletOutput("examplemap", height = 740), type="html", loader="loader4")
          ) # box -map
        ) # fluid row
      ), #tab item
      tabItem(tabName="recordtab",
        fluidRow(
        box(width=12, status="primary", title="Record New Observation", solidHeader=TRUE,
          helpText("Please enter your observations using the following inputs."),
          fluidRow(
            column(width=6, awesomeRadio("which_obs", label="Observation Type", choices = c("Catch", "Event"), selected = "Catch", inline=TRUE)
          )), # fluid row
          conditionalPanel(condition="input.which_obs == 'Catch'",
            fluidRow(
            column(width=4, pickerInput("select_current", label = "Current Intensity", 
                                        choices = c("None", "Moderate", "High"), selected = "None",
                                        choicesOpt = list(
                                                    content = sprintf("<span class='label label-%s'>%s</span>",
                                                          c("success", "warning", "danger"),
                                                          c("None", "Moderate", "High"))
            ))),
            column(width=4, pickerInput("select_depred", label = "Depredation Intensity", 
                                        choices = c("None", "Moderate", "High"), selected = "None",
                                        choicesOpt = list(
                                                    content = sprintf("<span class='label label-%s'>%s</span>",
                                                          c("success", "warning", "danger"),
                                                          c("None", "Moderate", "High"))
            ))),
            column(width=4, pickerInput("select_species", label = "Species Encountered", 
                                        choices = c("None","Shark", "Dolphin"), selected = "None",
                                        choicesOpt = list(
                                                    content = sprintf("<span class='label label-%s'>%s</span>",
                                                          c("success", "primary", "info"),
                                                          c("None", "Shark", "Dolphin"))
            )))
          )), # fluid row
          conditionalPanel(condition="input.which_obs == 'Event'",
            fluidRow(
            column(width=6, pickerInput("select_event", label = "Event Type",
                                        choices=c("Floating Debris", "Fish Kill", "Red Tide", "Other"),
                                        options=pickerOptions(contatiner="body"), width="100%"))
          )), # fluid row
          fluidRow(
            column(width=4, awesomeRadio("check_loc", label = "Use Current Location*", c("Yes", "No"), selected = "Yes")),
            column(width=4, conditionalPanel(condition="input.check_loc == 'No'",
                                              textInput("text_long", label = "Longitude"))),
            column(width=4, conditionalPanel(condition="input.check_loc == 'No'",
                                              textInput("text_lat", label = "Latitude")))
          ), #fluid row
          fluidRow(
            column(width=8, textInput("text_notes", label="Notes"))
          ), # fluid row
          fluidRow(
            column(width=4, awesomeCheckbox("check_share", label="Share Data with Group", value = TRUE))
          ), # fluid row
          actionButton("submit", "Submit Data", class="btn btn-primary", style = "color: white;"),
          hr(),
          helpText(em("*Geolocation services in your browser may provide approximate latitude and longitude data, 
                   which can be affected by factors like device settings, network conditions, and location permissions, 
                   potentially leading to slight inaccuracies in map plotting."))
        )#, # box
        ) # fluidrow
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
          width=12, title = "Map", status="primary", solidHeader=TRUE, collapsible=TRUE, 
          withLoader(leafletOutput("user_map", height = 540), type="html", loader="loader4")
        ) # box - user mpa
        ) # fluidrow
      ) #tab item
    ) # tab items
    ) # div main ui 
  ) # dashboard body
) #dashboard page  

