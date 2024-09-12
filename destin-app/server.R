################################################################################
# This script is the server for the destin app
# capabilities include: view maps and customization options

################################################################################
#remove(list = ls())

function(input, output, session) {
  
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
    
    # Check if the user wants to use the current location
    lat <- if (input$check_loc == "Yes") {
      NA #input$user_lat  
    } else if (input$text_lat == "") {
      NA  
    } else {
      input$text_lat
    }
    
    lon <- if (input$check_loc == "Yes") {
      NA #input$user_long  
    } else if (input$text_long == "") {
      NA  
    } else {
      input$text_long
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
      notes = notes
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
  
  #observe({
  output$sheet_data <- renderTable({
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
                           Days_Report <= input$days[1],
                           Effort_ID <= input$effort)
})



noaa_vl_prop <- reactive({
  filtered_prop() %>% 
  group_by(UNIQUE_RETRIEVAL, COMMON_NAME) %>%
  summarise(
    NUM_ALIVE = sum(NUM_FISH[CONDITION %in% c('ALIVE', 'ALIVE BAURO - STOM/BLADDER', 'ALIVE BAURO - EYES', 'ALIVE BAURO - BOTH')]),
    NUM_DEAD = sum(NUM_FISH[CONDITION == 'DEAD']),
    UNIQUE_RET_LAT = round(mean(LAT_BEGIN_SET), 6),
    UNIQUE_RET_LON = round(mean(LON_BEGIN_SET), 6)
  ) %>%
  mutate(PROP_DEAD = NUM_DEAD / NUM_ALIVE) %>%
  ungroup() %>%
    mutate(UNIQUE_RET_LAT)
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

#Point density
coords <- reactive({
  as.matrix(vl_prop_mh()[, c("UNIQUE_RET_LON", "UNIQUE_RET_LAT")])
})
bandwidth_x <- reactive({
  ks::hpi(coords()[ , "UNIQUE_RET_LON"])
})
bandwidth_y <- reactive({
  ks::hpi(coords()[ , "UNIQUE_RET_LAT"])
})
kde <- reactive({
  bkde2D(coords(), bandwidth = c(bandwidth_x(), bandwidth_y()), gridsize=c(1000,1000))
})

kdraster <- reactive({
  raster(list(x=kde()$x1, y=kde()$x2, z=kde()$fhat))
})

values_fhat <- reactive({
  kde()$fhat
})

heat_values <- reactive({
  temp <- values_fhat()
  temp[temp < 0.15] <- NA
  return(temp)
})

pal_heat <- reactive({
  colorNumeric("Spectral", domain = heat_values(), na.color="transparent", reverse=TRUE)
})

# pro_popup2 <- reactive({paste0("<br><strong>Average Fish Damaged: </strong>", round(pro_gridvalues()$Number_damaged, digits=2))
# })

#wind_data <- fromJSON("wind-global.json")

# need to make this into an observer duo for the popups to work and for it to be reactive
  output$examplemap <- renderLeaflet({
    # colors <- c("#7e911d", "#fdcb2f", "#ae002d")
    # pro_levels <- c("None", "Moderate", "High")
    # pro_pal <- colorFactor(colors, levels=pro_levels, domain=gridvalues()$classification)
    # popper <- paste0("<strong>Proportion Dead: </strong>", round(gridvalues()$PROP_DEAD.mean, digits = 2))
    
    leaflet() %>%
      #addProviderTiles("Esri.OceanBasemap", options = providerTileOptions(variant = "Ocean/World_Ocean_Base")) %>%
      #addProviderTiles("Esri.OceanBasemap", options = providerTileOptions(variant = "Ocean/World_Ocean_Reference")) %>%
      addProviderTiles("Esri.NatGeoWorldMap") %>%
      setView(lng=-86.5, lat=30.2, zoom=8)  %>%
      addScaleBar(position = 'topleft',
                  options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = TRUE, updateWhenIdle = FALSE)) #%>%
      # # addCircleMarkers(lng = ~Longitude,
      # #                  lat = ~Latitude,
      # #                  radius = 5,
      # #                  color = ~pro_pal(classification),
      # #                  stroke = FALSE, fillOpacity = 0.5,
      # #                  popup = ~pro_popup2(),
      # #                  group = "Points") %>%
      # addPolygons(data = gridvalues(),
      #             fillColor = ~pro_pal(classification),
      #             weight = 0.5,
      #             color = "black",
      #             fillOpacity = 1,
      #             highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
      #             popup = ~popper,
      #             group = "Depredation Intensity") %>%
      # # addCircles(lng = filtered_data()$Longitude,
      # #            lat = filtered_data()$Latitude,
      # #            radius = 6,
      # #            #opacity = opacity(),
      # #            color = "black",
      # #            group = "Point Data") %>%
      # addRasterImage(kdraster(),
      #                colors = pal_heat(),
      #                opacity = .75,
      #                group = "Depredation Density") %>%
      # addMarkers(lng=-86.3,
      #            lat=30.25, icon=boat_icon) %>%
      # addSimpleGraticule(interval = 1, 
      #                    group = "Graticule") %>%
      # # addVelocity(content=content,
      # #             group = "Current",
      # #             options=velocityOptions(speedUnit="kt")) %>%
      # addControl(html=html_legend, position="topright") %>%
      # leaflet::addLegend(position = 'topright',
      #           pal = pro_pal,
      #           values = gridvalues()$classification,
      #           opacity = 1,
      #           title = HTML("Depredation<br>Intensity"),
      #           group = "Depredation Intensity") %>%
      # addLegend(pal = pal_heat(),
      #           values = heat_values(),
      #           opacity = 1,
      #           title = HTML("Density of<br>Depredation"),
      #           group = "Depredation Density") %>%
      # addLayersControl(overlayGroups = c("Depredation Intensity", "Depredation Density", "Current Intensity", "Point Data", "Graticule"), 
      #                  options=layersControlOptions(collapsed=TRUE)) %>%
      # #addControl(html = '<div id="combined-legend"></div>', position = "topright") %>%
      # hideGroup(c("Depredation Density", "Current Intensity", "Point Data"))
  })
  
  observeEvent(input$update, {
    
    colors <- c("#7e911d", "#fdcb2f", "#ae002d")
    pro_levels <- c("None", "Moderate", "High")
    pro_pal <- colorFactor(colors, levels=pro_levels, domain=gridvalues()$classification)
    popper <- paste0("<strong>Proportion Dead: </strong>", round(gridvalues()$PROP_DEAD.mean, digits = 2))
    
    leafletProxy("examplemap") %>%
      clearShapes() %>%
      clearImages() %>%
      clearControls() %>%
      addPolygons(data = gridvalues(),
                  fillColor = ~pro_pal(classification),
                  weight = 0.5,
                  color = "black",
                  fillOpacity = 1,
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                  popup = ~popper,
                  group = "Depredation Intensity") %>%
      # addCircles(lng = filtered_data()$Longitude,
      #            lat = filtered_data()$Latitude,
      #            radius = 6,
      #            #opacity = opacity(),
      #            color = "black",
      #            group = "Point Data") %>%
      addRasterImage(kdraster(),
                     colors = pal_heat(),
                     opacity = .75,
                     group = "Depredation Density") %>%
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
                         group = "Depredation Intensity") %>%
      addLegend(pal = pal_heat(),
                values = heat_values(),
                opacity = 1,
                title = HTML("Density of<br>Depredation"),
                group = "Depredation Density") %>%
      addLayersControl(overlayGroups = c("Depredation Intensity", "Depredation Density", "Current Intensity", "Point Data", "Graticule"), 
                       options=layersControlOptions(collapsed=TRUE)) %>%
      #addControl(html = '<div id="combined-legend"></div>', position = "topright") %>%
      hideGroup(c("Depredation Density", "Current Intensity", "Point Data"))
  })
  

# # Map point data
# filtered_data <- reactive({
#     Data.In_df %>%
#       filter(Number_damaged > 0,
#               Day >= input$days[2],
#               Day <= input$days[1])
# })
# 
# # Point density
# coords <- reactive({
#   as.matrix(filtered_data()[, c("Longitude", "Latitude")])
# })
# bandwidth_x <- reactive({
#   ks::hpi(coords()[ , "Longitude"])
# })
# bandwidth_y <- reactive({
#   ks::hpi(coords()[ , "Latitude"])
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


# reactive grid shapes
# my.grid <- reactive({
#   switch(input$grid.scale,
#          "grid.sq10" = grid.sq10,
#          "grid.sq5" = grid.sq5,
#          "grid.sq2.5" = grid.sq2.5)
# })

# my.grid <- grid.sq1
# 
# # Convert Data.In to an sf object
# Data.In_sf <- reactive({
#   st_as_sf(Data.In, coords = c("Longitude", "Latitude"), crs = st_crs(my.grid()))
# })
# 
# # Perform spatial join
# Data.Grid <- reactive ({
#   as.data.frame(st_join(Data.In_sf(), my.grid(), join = st_intersects))
# })
# 
# #filter data set from GUI
#   filtered_data_cpue <- reactive({
#     Data.Grid() %>%
#       filter(is.na(My.metric) == F,
#              is.na(Discard_proportion) == F,
#              Year >= input$years[1],
#              Year <= input$years[2],
#              Season %in% input$seasons) %>%
#       group_by(GRID_ID) %>%
#       summarise(across(c(all_of(My.metric),Discard_proportion),mean)) %>%
#       unique() %>%
#       na.omit() %>%
#       plyr::rename(c("Discard_proportion"="Discard_proportion.mean")) %>%
#       as.data.frame()
#   })
# 
#   #Merge reactive data sets
#   gridvalues = reactive({
#     sp::merge(x = my.grid(), y = filtered_data_cpue(), by=c("GRID_ID"), all.x = FALSE)
#     })

  #########  Trouble-shooting  ###########
  # # text
  # output$radio.txt <- renderText({
  #    paste("Class working", class(gridvalues()))  # NEED TO MAKE class: sf data.frame
  # })
  #
  # # data tables
  # troubledata <- reactive({
  #   gridvalues()[[2]]
  #   #colnames(gridvalues())
  # })
  # output$trouble <- renderTable({troubledata()})
  #
  # troubledata2 <- reactive({
  #   head(filtered_data_cpue())
  # })
  # output$trouble2 <- renderTable({troubledata2()})
  #####################################3

# calculateOpacity <- function(day_value) {
#   normalized_day <- (day_value - min(data$Day)) / (max(data$Day) - min(data$Day))
#   opacity <- 1 - normalized_day
#   return(opacity)
# }

#Calculate colors for species
# colorsfun <- colorFactor(c("gray","white", "black"), levels = c("None", "Shark", "Dolphin"))

#Calculate opacity for days since present
# opacity <- reactive({
#   calculateOpacity(filtered_data()$Day)
# })  

# #base map: catch map
#   output$catch_map <- renderLeaflet({
#     leaflet(filtered_data()) %>%
#       addProviderTiles("Esri.NatGeoWorldMap", group = "Basemap") %>%
#       setView(lng=-86.5, lat=30.2, zoom=10)  %>%
#       addScaleBar(position = 'topleft',
#                   options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = TRUE, updateWhenIdle = TRUE)) %>%
#       addControl(html=html_legend, position="topright") %>%
#       addLegend(pal = pal_heat(),
#                 values = heat_values(),
#                 opacity = 1,
#                 title = "Density of Dolphin Depredation",
#                 group = "Dolphin Depredation Density") %>%
#       # addLegend(pal = colorsfun,
#       #           values = filtered_data()$Species,
#       #           opacity = 1,
#       #           title = "Species Encountered",
#       #           group = "Points") %>%
#       addRasterImage(kdraster(),
#                      colors = pal_heat(),
#                      opacity = .75,
#                      group = "Point Density") %>%
#       addMarkers(lng=-86.3,
#                  lat=30.25, icon=boat_icon) %>%
#       addCircles(lng = filtered_data()$Longitude,
#                  lat = filtered_data()$Latitude,
#                  radius = 6,
#                  #opacity = opacity(),
#                  color = "black",
#                  group = "Dolphin Depredation Density") %>%
#       # addVelocity(content=content2,
#       #             group = "Current",
#       #             options=velocityOptions(speedUnit="kt")) %>%
#       addSimpleGraticule(interval = 1, group = "Graticule") %>%
#       #addControl(html = '<div id="combined-legend"></div>', position = "topright") %>%
#       addLayersControl(#baseGroups = c("Basemap", "Spring", "Summer", "Fall", "Winter"),
#                        overlayGroups = c("Shark Depredation Density", "Dolphin Depredation Density", "Current Intensity", "Graticule"),
#                        options = layersControlOptions(position = "topright", collapsed = FALSE)) %>%
#       addControl(html = '<div id="combined-legend"></div>', position = "topright") %>%
#       hideGroup(c("Shark Depredation Density", "Current Intensity"))
#   })


  # reactive catch events
  # observe({
  #   if (length(filtered_data()) >= 1) {
  #   # point shapes
  #   leafletProxy("catch_map") %>%
  #     clearShapes() %>%
  #     clearControls() %>%
  #     clearImages() %>%
  #     # addRasterImage(env_spring, colors = pal_env_sp, group = "Spring") %>%
  #     # addRasterImage(env_summer, colors = pal_env_su, group = "Summer") %>%
  #     # addRasterImage(env_fall, colors = pal_env_fa, group = "Fall") %>%
  #     # addRasterImage(env_winter, colors = pal_env_wi, group = "Winter") %>%
  #       addControl(html=html_legend, position="topright") %>%
  #       addLegend(pal = pal_heat(),
  #               values = heat_values(),
  #               opacity = 1,
  #               title = "Density of Damaged Fish Caught",
  #               group = "Point Density") %>%
  #       # addLegend(pal = colorsfun(),
  #       #           values = ~Species,
  #       #           opacity = 1,
  #       #           title = "Species Encountered",
  #       #           group = "Species") %>%
  #     addRasterImage(kdraster(),
  #                    colors = pal_heat(),
  #                    opacity = .75,
  #                    group = "Point Density") %>%
  #       addMarkers(lng=-86.3,
  #                  lat=30.25, icon=boat_icon) %>%
  #       # addCircles(data=filtered_data(),
  #       #            lng = ~Longitude,
  #       #            lat = ~Latitude,
  #       #            radius = 6,
  #       #            color = ~colorsfun()(Species),
  #       #            group = "Points") %>%
  #       # addVelocity(content=content2,
  #       #             group = "Current",
  #       #             options=velocityOptions(speedUnit="kt")) %>%
  #     addSimpleGraticule(interval = 1, group = "Graticule") %>%
  #     addLayersControl(#baseGroups = c("Basemap", "Spring", "Summer", "Fall", "Winter"),
  #                      overlayGroups = c("Point Density", "Current", "Graticule"),
  #                      options = layersControlOptions(position = "topright", collapsed = FALSE)) %>%
  #       addControl(html = '<div id="combined-legend"></div>', position = "topright")
  #   } else {
  #     leafletProxy("catch_map") %>%
  #       clearShapes() %>%
  #       clearControls()
  #   }
  # })


  # #base map: cpue
  # output$cpue_map <- renderLeaflet({
  #   leaflet() %>%
  #     # addProviderTiles("Esri.OceanBasemap", options = providerTileOptions(variant = "Ocean/World_Ocean_Base")) %>%
  #     # addProviderTiles("Esri.OceanBasemap", options = providerTileOptions(variant = "Ocean/World_Ocean_Reference")) %>%
  #     addProviderTiles("Esri.NatGeoWorldMap") %>%
  #     setView(lng=-92, lat=28.5, zoom=7)  %>%
  #     addScaleBar(position = 'topleft',
  #                 options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = TRUE, updateWhenIdle = TRUE))
  # })



  ########NOTE FOR REACTIVE CODE WITH GENERALIZED METRICS, HERE IS USED
  ### [,My.metric]    FOR $Species_CPU_Hook_Hours_BLL1000  with filtered_data_????
  ### gridvalues()[[2]] seems to hold the right metric when grid shapefile columns reduced  .... CONFIRM FOR ANY SELECTED?!!!!!


  #reactive cpue
  # observe({
  #   if (length(filtered_data_cpue()) >= 1 & length(input$seasons) >=1) {
  # 
  #     cpue_popup <- paste0("<strong>Value: </strong>", round(gridvalues()$Discard_CPUE, digits = 2),
  #                          "<br><strong>Proportion Discarded: </strong>", round(gridvalues()$Discard_proportion.mean, digits = 2))
  # 
  #     qpal <- colorQuantile(n = 5, palette = "Reds", domain = gridvalues()$Discard_CPUE, reverse = FALSE)
  # 
  #     # cpue shapes
  #     leafletProxy("cpue_map", data = gridvalues()) %>%
  #       clearShapes() %>%
  #       addPolygons(fillColor = ~qpal(gridvalues()$Discard_CPUE),
  #                   weight = 0.5,
  #                   color = "black",
  #                   fillOpacity = 1,
  #                   highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
  #                   popup = cpue_popup) %>%
  #       clearControls() %>%
  #       addLegend(position = 'topright',
  #                 pal = qpal,
  #                 values = ~gridvalues()$Discard_CPUE,
  #                 opacity = 1,
  #                 title = paste(My.met.label)) %>%
  #       addSimpleGraticule(interval = 1, group = "Graticule") %>%
  #       addLayersControl(overlayGroups = c("Graticule"), options = layersControlOptions(collapsed = FALSE))
  # 
  # 
  #   } else {
  #     leafletProxy("cpue_map") %>%
  #       clearShapes() %>%
  #       clearControls()
  #   }
  # 
  # })

############### PRINT NUMBER OF OBSERVATIONS DISPLAYED IN THE MAP ##############
  output$text_obs <- renderText({
    sum(filtered_prop()$NUM_FISH)
  })

  
  
} #end server



 


