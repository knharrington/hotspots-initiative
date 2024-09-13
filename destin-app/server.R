################################################################################
# This script is the server for the destin app
# capabilities include: view maps and customization options

################################################################################
#remove(list = ls())

function(input, output, session) {

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
  if (is.null(session$userData$user_id)) {
    session$userData$user_id <- paste0("user_", substr(digest::digest(Sys.time()), 1, 8))
  }
    
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
  # Create a reactiveValues object to store the Google Sheets data
  data_store <- reactiveValues(data = NULL)
  
  # Function to fetch and update the data from Google Sheets
  update_sheet_data <- function() {
    tryCatch({
      sheet_data <- read_sheet(ss = sheet_id, sheet = "main")
      data_store$data <- sheet_data  # Update the reactive value
      showNotification("Data updated successfully", type="message")
    }, error = function(e) {
      showNotification("Error reading Google Sheet", type="error")
      print(paste("Error reading sheet:", e))
    })
  }
  
  # Fetch initial data on app startup
  update_sheet_data()
  
  observeEvent(input$submit, {
    #showNotification("submit button pressed")
    #print("submit button pressed")
    
    timestamp <- Sys.time()
    
    user_id <- session$userData$user_id
    
    # Check if the user wants to use the current location
    lat <- if (input$check_loc == "Yes") {
      NA #input$user_lat
    } else {
      as.numeric(input$text_lat)
    }
    
    if (is.na(lat) || lat < 29 || lat > 32) {
      showNotification("Please enter a valid latitude between 29N and 32N.", type = "error")
      return(NULL)  # Stop further execution if latitude is invalid
    }
    
    lon <- if (input$check_loc == "Yes") {
      NA #input$user_long  
    } else {
      as.numeric(input$text_long)
    }
    
    if (is.na(lon) || lon > -83 || lon < -89) {
      showNotification("Please enter a valid longitude between -89W and -83W.", type = "error")
      return(NULL)  # Stop further execution if longitude is invalid
    }
    
    notes <- if (input$text_notes == "") {
      NA
    } else {
      input$text_notes
    }
    
    response_data <- data.frame(
      current = input$select_current,
      depred = input$select_depred,
      species = input$select_species,
      latitude = lat,
      longitude = lon,
      notes = notes,
      timestamp = timestamp,
      user_id = as.character(user_id)
    ) 
    #df(dplyr::bind_rows(df(), response_data))
    #showNotification("Response data created")
    
    values <- read_sheet(ss = sheet_id, sheet="main")
    
    #showNotification(paste("Rows in sheet:", nrow(values)))
    # Check to see if our sheet has any existing data.
    # If not, let's write to it and set up column names. 
    # Otherwise, let's append to it.
    
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
      
    update_sheet_data()
    }, error = function(e) {
      showNotification("Error writing to Google Sheet", type="error")
    })
    
  }) # end observe event
  
  # testvalues <- read_sheet(ss = sheet_id, sheet="main")
  # print(testvalues)
  
  # debugging
  # observe({
  #   if (!is.null(data_store$data)) {
  #     print(sapply(data_store$data, class))  # Print the class of each column
  #   }
  # })
  
  #observe({
  output$sheet_data <- DT::renderDT({
    #read_sheet(ss = sheet_id, sheet = "main")
    req(data_store$data)
    data_store$data
  })
  #})
#######
  
boat_icon <- makeIcon(iconUrl = "www/boat2.svg",
                      iconWidth=35, iconHeight=30, 
                      iconAnchorX=15, iconAnchorY=15)
html_legend <- "<img src='boat2.svg' style='width:35px;height:30px;'> Current Location<br/>"
 
 
filtered_prop <- reactive({
    noaa_vl_des %>% filter(Days_Report >= input$days[2],
                           Days_Report <= input$days[1])#,
                           #Effort_ID <= input$effort)
})



noaa_vl_prop <- reactive({
  filtered_prop() %>% 
  filter(!is.na(LAT_BEGIN_SET) & !is.na(LON_BEGIN_SET)) %>%
  group_by(UNIQUE_RETRIEVAL, COMMON_NAME) %>%
  mutate(
    NUM_ALIVE = sum(NUM_FISH[CONDITION %in% c('ALIVE', 'ALIVE BAURO - STOM/BLADDER', 'ALIVE BAURO - EYES', 'ALIVE BAURO - BOTH')]),
    NUM_DEAD = sum(NUM_FISH[CONDITION == 'DEAD']),
    UNIQUE_RET_LAT = round(mean(LAT_BEGIN_SET), 6),
    UNIQUE_RET_LON = round(mean(LON_BEGIN_SET), 6),
    PROP_DEAD = NUM_DEAD / NUM_ALIVE
    )
})


# Convert the data frame to a spatial object
noaa_vl_prop_sf <- reactive({st_as_sf(noaa_vl_prop(), coords = c("UNIQUE_RET_LON", "UNIQUE_RET_LAT"), crs = st_crs(gridshp))})

# Perform the spatial join
grid_join <- reactive({setDT(st_join(noaa_vl_prop_sf(), gridshp, join = st_intersects))})
#grid_join_dt <- setDT(grid_join)

# Filter and aggregate the data
filtered_data <- reactive({grid_join()[!is.na(PROP_DEAD), #& COMMON_NAME == 'SNAPPER, RED',
                              .(PROP_DEAD.mean = mean(PROP_DEAD, na.rm = TRUE)),
                              by = GRID_ID]})
#filtered_data <- filtered_data[!is.na(GRID_ID)]

filtered_data2 <- reactive({
  filtered_data() %>%
  filter(GRID_ID != "NA",
    PROP_DEAD.mean != 'Inf') %>%
  mutate(classification = case_when(
    PROP_DEAD.mean == 0 ~ 'None',
    PROP_DEAD.mean <= 0.10 ~ 'Moderate',
    PROP_DEAD.mean > 0.10 ~ 'High'
  )) #%>%
  #mutate(classification = factor(classification,
                                 #levels = c("None", "Moderate", "High")))
})


# Merge the results back with the grid shapefile
gridvalues <- reactive({st_as_sf(merge(x = gridshp, y = filtered_data2(), by = "GRID_ID", all.x = FALSE))})



vl_prop_mh <- reactive({
  noaa_vl_prop() %>%
    filter(NUM_DEAD > 0)
})

# #Point density
# coords <- reactive({
#   as.matrix(vl_prop_mh()[, c("UNIQUE_RET_LON", "UNIQUE_RET_LAT")])
# })
# bandwidth_x <- reactive({
#   ks::hpi(coords()[ , "UNIQUE_RET_LON"])
# })
# bandwidth_y <- reactive({
#   ks::hpi(coords()[ , "UNIQUE_RET_LAT"])
# })
# kde <- reactive({
#   bkde2D(coords(), bandwidth = c(bandwidth_x(), bandwidth_y()), gridsize=c(1000,1000))
# })
# 
# kdraster <- reactive({
#   raster(list(x=kde()$x1, y=kde()$x2, z=kde()$fhat))
# })
# 
# values_fhat <- reactive({
#   kde()$fhat
# })
# 
# heat_values <- reactive({
#   temp <- values_fhat()
#   temp[temp < 0.15] <- NA
#   return(temp)
# })
# 
# pal_heat <- reactive({
#   colorNumeric("Spectral", domain = heat_values(), na.color="transparent", reverse=TRUE)
# })

# pro_popup2 <- reactive({paste0("<br><strong>Average Fish Damaged: </strong>", round(pro_gridvalues()$Number_damaged, digits=2))
# })

#wind_data <- fromJSON("wind-global.json")

# need to make this into an observer duo for the popups to work and for it to be reactive
  output$examplemap <- renderLeaflet({
    # colors <- c("#7e911d", "#fdcb2f", "#ae002d")
    # pro_levels <- c("None", "Moderate", "High")
    # pro_pal <- colorFactor(colors, levels=pro_levels, domain=gridvalues()$classification)
    # popper <- paste0("<strong>Proportion Dead: </strong>", round(gridvalues()$PROP_DEAD.mean, digits = 2))
    showNotification("Update map in order to view data", duration=20, closeButton=TRUE)
    leaflet() %>%
      #addProviderTiles("Esri.OceanBasemap", options = providerTileOptions(variant = "Ocean/World_Ocean_Base")) %>%
      #addProviderTiles("Esri.OceanBasemap", options = providerTileOptions(variant = "Ocean/World_Ocean_Reference")) %>%
      addProviderTiles("Esri.NatGeoWorldMap", options = providerTileOptions(minZoom = 5, maxZoom = 11)) %>%
      setView(lng=-86.75, lat=29.75, zoom=9)  %>%
      addScaleBar(position = 'topleft',
                  options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = TRUE, updateWhenIdle = FALSE)) 
  })
  
  observeEvent(input$update, {
    
    colors <- c("#18bc9c", "#f39c12", "#e74c3c")
    pro_levels <- c("None", "Moderate", "High")
    pro_pal <- colorFactor(colors, levels=pro_levels, domain=gridvalues()$classification)
    popper <- paste0("<strong>Proportion Dead: </strong>", round(gridvalues()$PROP_DEAD.mean, digits = 2))
    
    valid_data <- data_store$data %>%
      dplyr::filter(!is.na(latitude) & !is.na(longitude))
    
    leafletProxy("examplemap") %>%
      clearHeatmap() %>%
      clearShapes() %>%
      clearImages() %>%
      clearControls() %>%
      leafem::addMouseCoordinates() %>%
      addPolygons(data = gridvalues(),
                  fillColor = ~pro_pal(classification),
                  weight = 0.5,
                  color = "black",
                  fillOpacity = 1,
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                  popup = ~popper,
                  group = "Depredation Intensity") %>%
      # addMarkers(
      #   data=valid_data,
      #   lng=~as.numeric(longitude),
      #   lat=~as.numeric(latitude),
      #   popup=~paste("Species:", species, "<br>",
      #                "Current Intensity:", current, "<br>",
      #                "Depredation Intensity:", depred, "<br>",
      #                "Notes:", notes)
      # ) %>%
      # addCircles(lng = filtered_data()$Longitude,
      #            lat = filtered_data()$Latitude,
      #            radius = 6,
      #            #opacity = opacity(),
      #            color = "black",
      #            group = "Point Data") %>%
      # addRasterImage(kdraster(),
      #                colors = pal_heat(),
      #                opacity = .75,
      #                group = "Depredation Density") %>%
      addHeatmap(
        data = noaa_vl_prop()%>%filter(PROP_DEAD>0),
        lng = ~UNIQUE_RET_LON,
        lat = ~UNIQUE_RET_LAT,
        #gradient = "Spectral",
        intensity = ~NUM_FISH,
        blur = 35,
        #max = 0.05,
        radius = 30,
        group = "Catch Density"
      ) %>%
      addMarkers(lng=-86.3,
                 lat=30.25, icon=boat_icon) %>%
      addSimpleGraticule(interval = 1, 
                         group = "Graticule") %>%
      # addVelocity(content=content,
      #             group = "Current",
      #             options=velocityOptions(speedUnit="kt")) %>%
      addControl(html=html_legend, position="topright") %>%
      leaflet::addLegend(position = 'topright',
                         pal = pro_pal,
                         values = gridvalues()$classification,
                         opacity = 1,
                         title = HTML("Depredation<br>Intensity"),
                         group = "Depredation Intensity",
                         layerId = "Depredation Intensity") %>%
      # addLegend(pal = pal_heat(),
      #           values = heat_values(),
      #           opacity = 1,
      #           title = HTML("Density of<br>Depredation"),
      #           group = "Depredation Density") %>%
      addLayersControl(position="topleft", overlayGroups = c("Depredation Intensity", "Graticule"), 
                       options=layersControlOptions(collapsed=FALSE)) %>%
      #addControl(html = '<div id="combined-legend"></div>', position = "topright") %>%
      hideGroup(c("Depredation Density", "Current Intensity", "Point Data"))
  })
  

############### PRINT NUMBER OF OBSERVATIONS DISPLAYED IN THE MAP ##############
  output$text_obs <- renderText({
    sum(filtered_prop()$NUM_FISH)
  })

  
  
} #end server



 


