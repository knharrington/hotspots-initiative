################################################################################
# This script is the server for the Destin CFA app
################################################################################
#remove(list = ls())

function(input, output, session) {

# Handle user authentication by only showing the main user interface when credentials are satisfied
  shinyjs::hide(id="main_ui")

  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )

  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  # Dynamically display the UI according to login permissions
  observeEvent(credentials()$user_auth, {
    if (credentials()$user_auth) {
      session$userData$user_id <- credentials()$info$user
      shinyjs::show(id = "main_ui")  # Show UI when logged in
    } else {
      shinyjs::hide(id = "main_ui")  # Hide UI on logout
    }
  })
  
# Create modal dialog as a welcome/landing page
  observe({
    req(credentials()$info)
    showModal(
      ui = modalDialog(
        title = "Welcome to the Charter Fishermen's Association Hotspot Mapper",
        tags$div(
          tags$p("This app allows you to filter data on a map, record new observations, and view your own recorded data over time."),
          tags$h4("How to Use the App:"),
          tags$ul(
            tags$li("Use the filters in the sidebar to view all data in the Map tab."),
            tags$li("Share your observations on the water in the Record New Observations tab."),
            tags$li("View your own observations in the User Data tab.")
          ),
        ),
        footer = tags$p(style="text-align:center;", tags$em("Data collected through this app is confidential and only accessible to approved members.")),
        easyClose = TRUE,
        fade = TRUE
      )
    )
  })
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # If not utilizing usernames to filter data, create IDs based on session info
  # if (is.null(session$userData$user_id)) {
  #   session$userData$user_id <- paste0("user_", substr(digest::digest(Sys.time()), 1, 8))
  # }
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
  # Create a reactiveValues object to store the Google Sheet data
  data_store <- reactiveValues(data = NULL)
  
  # Function to fetch and update the data from Google Sheet
  update_sheet_data <- function() {
    tryCatch({
      sheet_data <- read_sheet(ss = sheet_id, sheet = "main")
      data_store$data <- sheet_data  
      #show_alert("Data updated successfully", type="success", btn_colors = "#3b8dbc")
      #showNotification("Data updated successfully", type="message")
    }, error = function(e) {
      showNotification("Error reading Google Sheet", type="error")
      print(paste("Error reading sheet:", e))
    })
  }
  
  # Fetch initial data on app startup
  update_sheet_data()
  
  # Confirm data entries
  observeEvent(input$submit, {
    #showNotification("submit button pressed")
    #print("submit button pressed")
    
    # Prevent duplicate entries due to double-clicking
    #shinyjs::disable("submit")
    
    ask_confirmation(
      inputId = "confirm",
      title = "Confirm Observations",
      btn_colors = c("#dd4b39","#3b8dbc")
    )
    
  })
  
  # perform error checks if data is confirmed
  observeEvent(input$confirm,{
    
    if (isTRUE(input$confirm)) {
    
    timestamp <- Sys.time()
    
    user_id <- session$userData$user_id
    
    #print(input$geolocation)
    # Check if the user wants to use the current location - turned off for now
    
    if (input$check_loc == "Yes") {
      if (input$geolocation == TRUE) {
        lon <- as.numeric(input$long)
        #print(input$long)
      } else {
      #showNotification("Unable to retrieve location. Please enter manually.", type = "error")  
      show_alert("Error writing data",
                 text = "Unable to retrieve location. Please enter manually.",
                 type="error", btn_colors = "#dd4b39")
      return(NULL)
      }
      shinyjs::enable("submit")
      #return(NULL)
    } else {
      lon <- as.numeric(input$text_long)
    }
    
    print(lon)
    
    if (is.na(lon) || lon > bbox$xmax || lon < bbox$xmin || is.null(lon)) {
      #print("Error: Longitude is outside the valid range.")
      show_alert("Error writing data",
                 text = paste0("Valid longitudes are between ", round(bbox$xmin, 2), "W and ", round(bbox$xmax, 2), "W."),
                 type="error", btn_colors = "#dd4b39")
      #showNotification("Please enter a valid longitude between -89W and -83W.", type = "error")
      shinyjs::enable("submit")
      return(NULL)  
    }
    
    if (input$check_loc == "Yes") {
      if (input$geolocation == TRUE) {
        lat <- as.numeric(input$lat)
        #print(input$lat)
      } else {
      show_alert("Error writing data",
                 text = "Unable to retrieve location. Please enter manually.",
                 type="error", btn_colors = "#dd4b39")
      #showNotification("Unable to retrieve location. Please enter manually.", type = "error")
      return(NULL)
      }
      shinyjs::enable("submit")
      #return(NULL)
    } else {
      lat <- as.numeric(input$text_lat)
    }
    
    print(lat)
    
    if (is.na(lat) || lat < bbox$ymin || lat > bbox$ymax || is.null(lat)) {
      show_alert("Error writing data",
                 text = paste0("Valid latitudes are between ", round(bbox$ymin, 2), "N and ", round(bbox$ymax, 2), "N."),
                 type="error", btn_colors = "#dd4b39")
      #showNotification("Please enter a valid latitude between 28.9N and 30.5N.", type = "error")
      shinyjs::enable("submit")
      return(NULL)  
    }
    
    notes <- if (input$text_notes == "") {
      NA
    } else {
      input$text_notes
    }
    
    marker <- if (input$which_obs == "Catch") {
      "No"
    }  else {
      "Yes"
    }
    
    event <- if (input$which_obs == "Catch") {
      NA
    } else {
      input$select_event
    }
    
    current <- if (input$which_obs == "Event") {
      NA
    } else {
      input$select_current
    }
    
    depred <- if (input$which_obs == "Event") {
      NA
    } else {
      input$select_depred
    }
    
    species <- if (input$which_obs == "Event") {
      NA
    } else {
      input$select_species
    }
    
    # Compile responses into a data frame
    response_data <- data.frame(
      current = current,
      depred = depred,
      species = species,
      latitude = lat,
      longitude = lon,
      shared = input$check_share,
      marker = marker,
      event = event,
      notes = notes,
      timestamp = timestamp,
      user_id = as.character(user_id)
    ) 
    #df(dplyr::bind_rows(df(), response_data))
    #showNotification("Response data created")
    
    values <- read_sheet(ss = sheet_id, sheet="main")
    
    #showNotification(paste("Rows in sheet:", nrow(values)))
    # Check to see if sheet has any existing data. If not, write to it and set up column names. Otherwise, append to it.
    
    # Merge the new responses with the existing Google Sheet
    tryCatch({
    if (nrow(values) == 0) {
      #showNotification("Writing new data to sheet")
      sheet_write(data = response_data,
                  ss = sheet_id,
                  sheet = "main")
    } else {
      #showNotification("Appending data to sheet")
      sheet_append(data = response_data,
                   ss = sheet_id,
                   sheet = "main")
      #showNotification("Data recorded successfully", duration=5, type="message")
    }
      
    # Handle connectivity errors
    update_sheet_data()
    
    }, error = function(e) {
      showNotification("Error writing to Google Sheet", type="error")
    })
    
    show_alert("Data updated successfully", type="success", btn_colors = "#3b8dbc")
    
    shinyjs::enable("submit")
    
    } else if (isFALSE(input$confirm)) {
      return(NULL)
      shinyjs::enable("submit")
    }
  }, ignoreNULL=TRUE) # End input confirm
  
  # testvalues <- read_sheet(ss = sheet_id, sheet="main")
  # print(testvalues)
  
  # debugging
  # observe({
  #   if (!is.null(data_store$data)) {
  #     print(sapply(data_store$data, class))  # Print the class of each column
  #   }
  # })
  
  #observe({
  # output$sheet_data <- DT::renderDT({
  #   #read_sheet(ss = sheet_id, sheet = "main")
  #   req(data_store$data)
  #   data_store$data
  # })
  #})

################################################################################
  
# Filter dataset based on user inputs  
filtered_prop <- reactive({
    noaa_vl_des %>% filter(Days_Report <= input$days[1])#,
                           #Days_Report <= input$days[1])#,
                           #Effort_ID <= input$effort)
})


# Calculate fields for NOAA dataset 
noaa_vl_prop <- reactive({
  filtered_prop() %>% 
  filter(!is.na(LAT_BEGIN_SET) & !is.na(LON_BEGIN_SET)) %>%
  group_by(UNIQUE_RETRIEVAL, COMMON_NAME, VESSEL_ID) %>%
  mutate(
    NUM_ALIVE = sum(NUM_FISH[CONDITION %in% c('ALIVE', 'ALIVE BAURO - STOM/BLADDER', 'ALIVE BAURO - EYES', 'ALIVE BAURO - BOTH')]),
    NUM_DEAD = sum(NUM_FISH[CONDITION == 'DEAD']),
    UNIQUE_RET_LAT = round(mean(LAT_BEGIN_SET), 6),
    UNIQUE_RET_LON = round(mean(LON_BEGIN_SET), 6),
    PROP_DEAD = NUM_DEAD / NUM_ALIVE,
    NUM_SHARKS = sum(NUM_FISH[grepl("SHARK", COMMON_NAME)])
    )
})

# Convert the data frame to a spatial object
noaa_vl_prop_sf <- reactive({st_as_sf(noaa_vl_prop(), coords = c("UNIQUE_RET_LON", "UNIQUE_RET_LAT"), crs = st_crs(gridshp))})

# Perform the spatial join
grid_join <- reactive({setDT(st_join(noaa_vl_prop_sf(), gridshp, join = st_intersects))})
#grid_join_dt <- setDT(grid_join)

# Filter and aggregate the data if the cells contain at least 3 vessels contributing
filtered_data <- reactive({
  grid_join() %>%
    group_by(GRID_ID) %>%
    dplyr::reframe(
      PROP_DEAD.mean = mean(PROP_DEAD, na.rm = TRUE),  # Calculate the mean PROP_DEAD for each grid
      NUM_SHARKS.mean = mean(NUM_SHARKS, na.rm=TRUE),
      num_points = n(),  # Count the number of points in each grid
      unique_vessel_ids = n_distinct(VESSEL_ID)  # Count unique VESSEL_IDs in each grid
    ) %>%
    filter(num_points >= 3 & unique_vessel_ids >= 3)  # Apply the filtering condition: at least 3 points and at least 3 unique VESSEL_IDs
})

filtered_data2 <- reactive({
  filtered_data() %>%
  filter(GRID_ID != "NA",
    PROP_DEAD.mean != 'Inf') %>%
  mutate(
    depred_class = case_when(
      PROP_DEAD.mean == 0 ~ 'None',
      PROP_DEAD.mean <= 0.10 ~ 'Moderate',
      PROP_DEAD.mean > 0.10 ~ 'High'
    ),
    current_class = "None",
    sharks_class = case_when(
      NUM_SHARKS.mean == 0 ~ "None",
      NUM_SHARKS.mean <= 2 ~ "Moderate",
      NUM_SHARKS.mean > 2 ~ "High"
    ),
    dolphins_class = "None"
  )
})

# Merge the results back with the grid shapefile
gridvalues <- reactive({st_as_sf(merge(x = gridshp, y = filtered_data2(), by = "GRID_ID", all.x = FALSE))})

suppressWarnings(
  grid_centroids <- reactive({
    st_centroid(gridvalues()) %>% select(GRID_ID, depred_class, current_class, sharks_class, dolphins_class, num_points)
  })
)
# grid_centroids_sd <- reactive({
#   st_centroid(gridvalues()) %>% filter(depred_class != "None")
# })

######################################### user data

user_data <- reactive({data_store$data %>% #data_store$data %>% sheet_data %>% 
  filter(marker == "No" & shared == TRUE & !is.na(longitude) & !is.na(latitude)) %>% # only add shareable data
  mutate(
    current_bin = case_when(
      current == "None" ~ 1,
      current == "Moderate" ~ 2,
      current == "High" ~3
    ),
    depred_bin = case_when(
      depred == "None" ~ 1,
      depred == "Moderate" ~ 2,
      depred == "High" ~3
    )
  )
})

user_data_sf <- reactive({st_as_sf(user_data(), coords = c("longitude", "latitude"), crs = st_crs(gridshp))})
grid_join_u <- reactive({setDT(st_join(user_data_sf(), gridshp, join = st_intersects))})

filtered_data_u <- reactive({
  threshold_time <- Sys.time() - as.difftime(input$days, units = "days")
  
  grid_join_u() %>%
    filter(!is.na(GRID_ID),
           timestamp >= threshold_time) %>%
    group_by(GRID_ID) %>%
    dplyr::reframe(
      current.mean = mean(current_bin, na.rm = TRUE), 
      depred.mean = mean(depred_bin, na.rm=TRUE),
      num_points = n(),
      current_class = case_when(
        current.mean == 1 ~ "None",
        current.mean <= 2 ~ "Moderate",
        current.mean > 2 ~ "High"
      ),
      depred_class = case_when(
        depred.mean == 1 ~ "None",
        depred.mean <= 2 ~ "Moderate",
        depred.mean > 2 ~ "High"
      ),
      sharks_class = case_when(
        species == "None" ~ "None",
        species == "Dolphin" ~ "None",
        species == "Shark" & depred.mean == 1 ~ "None",
        species == "Shark" & depred.mean <= 2 ~ "Moderate",
        species == "Shark" & depred.mean > 2 ~ "High"
      ),
      dolphins_class = case_when(
        species == "None" ~ "None",
        species == "Shark" ~ "None",
        species == "Dolphin" & depred.mean == 1 ~ "None",
        species == "Dolphin" & depred.mean <= 2 ~ "Moderate",
        species == "Dolphin" & depred.mean > 2 ~ "High"
      ),
      all_notes = paste(notes[!is.na(notes)], collapse = ";<br> "),
      num_points = n()
    )
})

gridvalues_u <- reactive({st_as_sf(merge(x = gridshp, y = filtered_data_u(), by = "GRID_ID", all.x = FALSE))})

suppressWarnings(
  grid_centroids_u <- reactive({
    st_centroid(gridvalues_u()) %>% select(GRID_ID, depred_class, current_class, sharks_class, dolphins_class, num_points)
  })
)
  
suppressWarnings(
  all_centroids <- reactive({rbind(grid_centroids(), grid_centroids_u())})
)

user_markers <- reactive({
  threshold_time <- Sys.time() - as.difftime(input$days, units = "days")
  data_store$data %>% #data_store$data %>% sheet_data %>% 
    filter(marker == "Yes" & shared == TRUE & timestamp >= threshold_time
           & !is.na(longitude) & !is.na(latitude)) # only add shareable data
})

#########################################

# Map with proxy
  output$examplemap <- renderLeaflet({
    req(credentials()$info)
    showNotification("Update map in order to view data", duration = 30, closeButton = TRUE)
    leaflet() %>%
      addProviderTiles("Esri.NatGeoWorldMap", options = providerTileOptions(minZoom = 6, maxZoom = 12)) %>%
      setView(lng=-90, lat=27.5, zoom=7)  %>%
      addScaleBar(position = 'topleft',
                  options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = TRUE, updateWhenIdle = FALSE)) 
  })
  
colors <- c("#00a65a", "#f39c12", "#dd4b39")
pro_levels <- c("None", "Moderate", "High")
pro_pal <- colorFactor(colors, levels = pro_levels, domain = c("None", "Moderate", "High"))
marker_icon <- makeAwesomeIcon(icon = "exclamation",
                               library = "fa",
                               markerColor = "darkred",
                               iconColor = "#FFFFFF")

  observeEvent(input$update, {
    
    popper <- paste0("<strong>Notes: </strong>observer data")
    poppy <- paste0("<strong>Notes: </strong>", gridvalues_u()$all_notes)
    poppem <- paste0("<strong>Event Type: </strong>", user_markers()$event,
                     "<br><strong>Notes: </strong>", user_markers()$notes)
    
    proxy <- leafletProxy("examplemap")
      
    proxy %>% 
      clearHeatmap() %>%
      clearShapes() %>%
      clearImages() %>%
      clearControls() %>%
      leafem::addMouseCoordinates() %>%
      addSimpleGraticule(interval = 1, 
                         group = "Graticule") %>%
      addLayersControl(position = "topleft", overlayGroups = c("Graticule"), 
                       options = layersControlOptions(collapsed = FALSE))
    
    if (input$geolocation == TRUE) {
      proxy %>%
        addMarkers(lng = input$long,
                   lat = input$lat,
                   icon = boat_icon) %>%
        addControl(html = html_legend, position="topright")
    }
    
    if (input$radio_depred == "Total" & input$radio_layer == "Intensity (grid)"){
    proxy %>%
      addPolygons(data = gridvalues(),
                  fillColor = ~pro_pal(depred_class),
                  weight = 0.5,
                  color = "black",
                  fillOpacity = 1,
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                  popup = ~popper,
                  group = "Depredation Intensity") %>%
        addPolygons(data = gridvalues_u(),
                    fillColor = ~pro_pal(depred_class),
                    weight = 0.5,
                    color = "black",
                    fillOpacity = 1,
                    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                    popup = ~poppy,
                    group = "Depredation Intensity") %>%
        leaflet::addLegend(position = 'topright',
                           pal = pro_pal,
                           values = c("None", "Moderate", "High"),
                           opacity = 1,
                           title = HTML("Depredation<br>Intensity"),
                           group = "Depredation Intensity",
                           layerId = "Depredation Intensity") 
    }  
      
    if (input$radio_depred == "Total" & input$radio_layer == "Density (heat)"){
    proxy %>% 
      addHeatmap(data = all_centroids() %>% filter(depred_class != "None"),
        #gradient = "Spectral",
        intensity = ~num_points,
        blur = 28,
        #max = 0.05,
        radius = 20,
        group = "Catch Density")
    }  
    
    if (input$radio_depred == "Sharks" & input$radio_layer == "Intensity (grid)"){
      proxy %>%
        addPolygons(data = gridvalues(),
                    fillColor = ~pro_pal(sharks_class),
                    weight = 0.5,
                    color = "black",
                    fillOpacity = 1,
                    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                    popup = ~popper,
                    group = "Shark Intensity") %>%
        addPolygons(data = gridvalues_u(),
                    fillColor = ~pro_pal(sharks_class),
                    weight = 0.5,
                    color = "black",
                    fillOpacity = 1,
                    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                    popup = ~poppy,
                    group = "Shark Intensity") %>%
        leaflet::addLegend(position = 'topright',
                           pal = pro_pal,
                           values = c("None", "Moderate", "High"),
                           opacity = 1,
                           title = HTML("Shark Intensity"),
                           group = "Shark Intensity",
                           layerId = "Shark Intensity") 
    }
    
    if (input$radio_depred == "Sharks" & input$radio_layer == "Density (heat)"){
      proxy %>% 
        addHeatmap(data = all_centroids() %>% filter(sharks_class != "None"),
                   #gradient = "Spectral",
                   intensity = ~num_points,
                   blur = 28,
                   #max = 0.05,
                   radius = 20,
                   group = "Shark Density")
    } 
    
    if (input$radio_depred == "Dolphins" & input$radio_layer == "Intensity (grid)"){
      proxy %>%
        addPolygons(data = gridvalues(),
                    fillColor = ~pro_pal(dolphins_class),
                    weight = 0.5,
                    color = "black",
                    fillOpacity = 1,
                    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                    popup = ~popper,
                    group = "Dolphin Intensity") %>%
        addPolygons(data = gridvalues_u(),
                    fillColor = ~pro_pal(dolphins_class),
                    weight = 0.5,
                    color = "black",
                    fillOpacity = 1,
                    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                    popup = ~poppy,
                    group = "Dolphin Intensity") %>%
        leaflet::addLegend(position = 'topright',
                           pal = pro_pal,
                           values = c("None", "Moderate", "High"),
                           opacity = 1,
                           title = HTML("Dolphin Intensity"),
                           group = "Dolphin Intensity",
                           layerId = "Dolphin Intensity") 
    }
    
    if (input$radio_depred == "Dolphins" & input$radio_layer == "Density (heat)"){
      proxy %>% 
        addHeatmap(data = all_centroids() %>% filter(dolphins_class != "None"),
                   #gradient = "Spectral",
                   intensity = ~num_points,
                   blur = 35,
                   #max = 0.05,
                   radius = 30,
                   group = "Dolphin Density")
    }
    
    if (input$radio_current == "Yes"){
      proxy %>%
        clearHeatmap() %>%
        clearControls() %>%
        #clearShapes() %>%
        addControl(html=html_legend, position="topright") %>%
        addPolygons(
          data = gridvalues(),
          fillColor = ~pro_pal(current_class),
          weight = 0.5,
          color = "black",
          fillOpacity = 1,
          highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
          popup = ~popper,
          group = "Current Intensity") %>%
        addPolygons(data = gridvalues_u(),
                    fillColor = ~pro_pal(current_class),
                    weight = 0.5,
                    color = "black",
                    fillOpacity = 1,
                    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                    popup = ~poppy,
                    group = "Current Intensity") %>%
        leaflet::addLegend(position = 'topright',
                           pal = pro_pal,
                           values = c("None", "Moderate", "High"),
                           opacity = 1,
                           title = HTML("Current Intensity"),
                           group = "Current Intensity",
                           layerId = "Current Intensity")
    }
    
    if (input$show_markers == TRUE){
      proxy %>%
        addAwesomeMarkers(data = user_markers(),
                   lng = ~longitude,
                   lat = ~latitude,
                   icon = marker_icon,
                   popup = ~poppem,
                   group = "Markers")
    }  else {
      proxy %>%
        clearGroup("Markers")
    }
  })

############################# 
# Print number of observations in the map
  output$text_obs <- renderInfoBox({
    infoBox(
      "Total Observations",
      paste0(format(sum(nrow(filtered_data()), nrow(filtered_data_u())), big.mark=",")),
      icon=icon("binoculars"),
      color="light-blue"
    )
  })

############################
# User data tab outputs
  
  output$user_data <- DT::renderDT({
    req(credentials()$info)
    req(data_store$data)
    data_store$data %>% 
      filter(
      user_id %in% session$userData$user_id
      ) %>%
      rename("Current Intensity" = "current",
             "Depredation Intensity" = "depred",
             "Species Encountered" = "species",
             "Longitude" = "longitude",
             "Latitude" = "latitude",
             "Shared" = "shared",
             "Marker" = "marker",
             "Event Type" = "event",
             "Notes" = "notes",
             "Time Recorded (UTC)" = "timestamp",
             "User ID" = "user_id")
  })
  

    userid_data <- reactive({
      threshold_time <- Sys.time() - as.difftime(input$days_u, units = "days")
      req(credentials()$info)
      req(data_store$data)
      if (input$display_all == "No") {
        data_store$data %>% 
          filter(!is.na(latitude) & !is.na(longitude) & user_id %in% session$userData$user_id
                 & timestamp >= threshold_time)
      } else if (input$display_all == "Yes") {
        data_store$data %>% 
          filter(!is.na(latitude) & !is.na(longitude) & user_id %in% session$userData$user_id)
      }
    })  
  
  calculateOpacity <- function(day_value) {
    # Ensure there are timestamps to work with
    if (length(day_value) == 0) return(rep(1, length(day_value)))  # Default to opacity of 1 if no data
    if (length(day_value) == 1) return(rep(1, length(day_value)))  # Default to opacity of 1 if one data point
    
    # Normalize the timestamps to get a range between 0 and 1
    current_time <- Sys.time()
    days_since <- as.numeric(difftime(current_time, day_value, units = "days"))
    
    # Normalize the opacity (0 is more recent, 1 is the furthest back)
    max_days <- max(days_since, na.rm = TRUE)
    min_days <- min(days_since, na.rm = TRUE)
    
    # Avoid division by zero
    if (max_days == min_days) {
      return(rep(1, length(day_value)))  # Default opacity if all timestamps are the same
    }
    
    normalized_day <- (days_since - min_days) / (max_days - min_days)
    opacity <- 1 - normalized_day  # More recent data will be more opaque
    
    return(opacity)
  }
  
  # Calculate colors for species - aligns with the input colors
  colors_sp <- colorFactor(c("#00a65a","#3c8dbc", "#00c0ef"), levels = c("None", "Shark", "Dolphin"))
  
  # Calculate opacity for days since recording
  opacity <- reactive({
    req(userid_data())
    timestamps <- userid_data()$timestamp
    calculateOpacity(timestamps)
  })
  
  # create user map
  output$user_map <- renderLeaflet({
    req(credentials()$info)
    leaflet() %>%
      addProviderTiles("Esri.NatGeoWorldMap", options = providerTileOptions(minZoom = 6, maxZoom = 12)) %>%
      setView(lng=-90, lat=27.5, zoom=7)  %>%
      addScaleBar(position = 'topleft',
                  options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = TRUE, updateWhenIdle = FALSE)) 
    })
    
  observeEvent(input$update_u, {
    
    poppy <- paste0("<strong>Notes: </strong>", userid_data()[userid_data()$marker == "No",]$notes,
                    "<br><strong>Date Recorded: </strong>", as.Date(userid_data()[userid_data()$marker == "No",]$timestamp))
    poppem <- paste0("<strong>Event Type: </strong>", userid_data()[userid_data()$marker == "Yes",]$event,
                     "<br><strong>Notes: </strong>", userid_data()[userid_data()$marker == "Yes",]$notes,
                     "<br><strong>Date Recorded: </strong>", as.Date(userid_data()[userid_data()$marker == "Yes",]$timestamp))
    
    proxyu <- leafletProxy("user_map")
    
    proxyu %>%
      clearControls() %>%
      clearMarkers() %>%
      clearImages() %>%
      clearShapes() %>%
      leafem::addMouseCoordinates() %>%
      addSimpleGraticule(interval = 1,
                         group = "Graticule") %>%
      addLayersControl(position="topleft", overlayGroups = c("Graticule"),
                       options=layersControlOptions(collapsed=FALSE))
    
    if (input$geolocation == TRUE) {
      proxyu %>%
        addMarkers(lng=input$long,
                   lat=input$lat,
                   icon=boat_icon) %>%
        addControl(html=html_legend, position="topright")
    }
    
    if (input$show_markers_u == TRUE & nrow(userid_data()[userid_data()$marker == "Yes",]) >= 1){
      proxyu %>%
        addAwesomeMarkers(data = userid_data()[userid_data()$marker == "Yes",],
                          lng = ~longitude,
                          lat = ~latitude,
                          icon = marker_icon,
                          popup = ~poppem,
                          group = "Markers U")
    }  else if  (input$show_markers == FALSE) {
      proxyu %>%
        clearGroup("Markers U")
    }
    
    if (nrow(userid_data()[userid_data()$marker == "No",]) >= 1) {
      if (input$radio_points == "Species Encountered") {
        proxyu %>%
          clearGroup("Depredation Intensity") %>%
          clearGroup("Current Intensity") %>%
          addCircleMarkers(
            data=userid_data()[userid_data()$marker == "No",],
            lng=~as.numeric(longitude),
            lat=~as.numeric(latitude),
            weight=3,
            radius=10,
            fillOpacity = opacity(),
            color = ~colors_sp(species),
            popup=~poppy,
            group="Species Encountered"
          ) %>%
          leaflet::addLegend(position = 'topright',
                             pal = colors_sp,
                             values = factor(c("None", "Shark", "Dolphin"), levels = c("None", "Shark", "Dolphin")),
                             opacity = 1,
                             title = HTML("Species Encountered"),
                             group = "Species Encountered",
                             layerId = "Species Encountered")
      } else if (input$radio_points == "Depredation Intensity") {
        proxyu %>%
          clearGroup("Species Encountered") %>%
          clearGroup("Current Intensity") %>%
          addCircleMarkers(
            data=userid_data()[userid_data()$marker == "No",],
            lng=~as.numeric(longitude),
            lat=~as.numeric(latitude),
            weight=3,
            radius=10,
            fillOpacity = opacity(),
            color = ~pro_pal(depred),
            popup=~poppy,
            group="Depredation Intensity"
          ) %>%
          addLegend(
            position = 'topright',
            pal = pro_pal,
            values = factor(c("None", "Moderate", "High"), levels = c("None", "Moderate", "High")),
            opacity = 1,
            title = HTML("Depredation Intensity"),
            group = "Depredation Intensity",
            layerId = "Depredation Intensity")
      } else if (input$radio_points == "Current Intensity") {
        proxyu %>%
          clearGroup("Depredation Intensity") %>%
          clearGroup("Species Encountered") %>%
          addCircleMarkers(
            data=userid_data()[userid_data()$marker == "No",],
            lng=~as.numeric(longitude),
            lat=~as.numeric(latitude),
            weight=3,
            radius=10,
            fillOpacity = opacity(),
            color = ~pro_pal(current),
            popup=~poppy,
            group="Current Intensity"
          ) %>%
          addLegend(
            position = 'topright',
            pal = pro_pal,
            values = factor(c("None", "Moderate", "High"), levels = c("None", "Moderate", "High")),
            opacity = 1,
            title = HTML("Current Intensity"),
            group = "Current Intensity",
            layerId = "Current Intensity")
      }
    }
  })
    
} #end server

