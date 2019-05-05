output$filter_sp_map <- renderUI({
  if (!is.null(input$file2) & !is.null(input$file1)) {
    sp_read <- variables$sp_read
    species_name <- unique(sp_read$sp)
    selectInput("selec_filter_sp_map", label = h4("Select species"),
                choices = species_name,
                selected = 1, multiple = TRUE)
  }
})

select_all_filter <- observeEvent(input$select_all_filter_btn, {
  sp_read <- variables$sp_read
  species_name = unique(sp_read$sp)
  updateSelectInput(session, "selec_filter_sp_map", label = h4("Select species"), 
                    choices = species_name, 
                    selected = species_name)
})

output$scatter_plot <- renderPlotly({
  if (!is.null(input$file2) & !is.null(input$file1)) {
    sp_read <- variables$sp_without_outliers
    sp_selected <- subset(sp_read, sp %in% input$selec_filter_sp_map)
    grid_read <- variables$grid_read
    plot_ly(data = sp_selected, x = ~lat, y = ~lon, color = ~sp)
  }
})

output$sp_freq_plot <- renderPlotly({
  if (!is.null(input$file2) & !is.null(input$file1)) {
    sp_freq <- get_species_freq()
    sp_selected <- subset(sp_freq, Specie %in% input$selec_filter_sp_map)
    a <- list(
      autotick = FALSE,
      showticklabels = FALSE
    )
    plot_ly(data = sp_selected, x = ~Specie, y = ~Freq, color = ~Specie) %>%
      layout(
        xaxis = a
      )
  }
})

# not using
output$sp_out_freq_plot <- renderPlotly({
  if (!is.null(input$file2) & !is.null(input$file1)) {
    sp_out_freq <- get_species_outliers_freq()
    sp_selected <- subset(sp_out_freq, Specie %in% input$selec_filter_sp_map)
    a <- list(
      autotick = FALSE,
      showticklabels = FALSE
    )
    plot_ly(data = sp_selected, x = ~Specie, y = ~Freq, color = ~Specie) %>%
      layout(
        xaxis = a
      )
  }
})

# not using
output$sp_occ_scatter_plot <- renderPlotly({
  if (!is.null(input$file2) & !is.null(input$file1)) {
    results <- variables$results
    
    sp_occ_total <- results[, results[nrow(results), ] != 0]
    
    sp_occ <- sp_occ_total[1:nrow(sp_occ_total)-1, 1:ncol(sp_occ_total)]
    
    sp_occ$X <- NULL
    
    sp_occ <- subset(sp_occ, TOTAL > 0)
    sp_occ$TOTAL <- NULL
    
    sp_occ <- unique(sp_occ)
    
    a <- c()
    for (i in 1:nrow(sp_occ)) {
      a <- c(a, i)
    }
    
    sp_occ$sp <-a 
    
    results_sp_names <- names(sp_occ)[1:(ncol(sp_occ)-3)]
    
    results_sp_names <- gsub(".", " ", results_sp_names, fixed = TRUE)
    for (i in 1:length(results_sp_names)) {
      sp_occ[sp_occ[ , i] == 1, ]$sp <- results_sp_names[i]
    }
    
    sp_occ <- subset(sp_occ, sp %in% input$selec_filter_sp_map)
    
    plot_ly(data = sp_occ, x = ~lat, y = ~lon, color = ~sp, type = 'scatter')
  }
})

output$map_grid <- renderLeaflet({
  if (!is.null(input$file1)) {
    grid_read <- variables$grid_read
    leaflet() %>%
      addProviderTiles("Esri.OceanBasemap", group = "Esri.OceanBasemap") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetmap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
      addLayersControl(baseGroups = c("OpenStreetmap","Esri.OceanBasemap", 'Esri.WorldImagery'),
                       options = layersControlOptions(collapsed = TRUE, autoZIndex = F)) %>%
      setView(lng = -60.85, lat = -15.45, zoom = 3) %>%
      addCircleMarkers(
        lng = grid_read$lon,
        lat = grid_read$lat,
        label = paste("lon:", grid_read$lon, ", lat:", grid_read$lat),
        radius = 7,
        color = "blue",
        stroke = FALSE, fillOpacity = 0.3
      )
  }
})

output$map_sp <- renderLeaflet({
  if (!is.null(input$file2) & !is.null(input$file1)) {
    grid_read <- variables$grid_read
    sp_read <- variables$sp_without_outliers
    
    sp_selected <- subset(sp_read, sp %in% input$selec_filter_sp_map)
    
    colors <- colorRampPalette(palette()[2:length(palette())-1])(length(unique(sp_selected$sp)))
    pal <- colorFactor(colors, domain = unique(sp_selected$sp))
    
    leaflet() %>%
      addProviderTiles("Esri.OceanBasemap", group = "Esri.OceanBasemap") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetmap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
      addLayersControl(baseGroups = c("OpenStreetmap","Esri.OceanBasemap", 'Esri.WorldImagery'),
                       options = layersControlOptions(collapsed = TRUE, autoZIndex = F)) %>%
      setView(lng = -60.85, lat = -15.45, zoom = 3) %>%
      addCircleMarkers(
        data = sp_selected,
        lng = sp_selected$lon,
        lat = sp_selected$lat,
        label = paste(sp_selected$sp, "lon:", sp_selected$lon, ", lat:", sp_selected$lat),
        radius = 7,
        color = ~pal(sp_selected$sp),
        stroke = FALSE, fillOpacity = 0.3
      )
    
  }
})

output$map_sp_clustered <- renderLeaflet({
  if (!is.null(input$file2) & !is.null(input$file1)) {
    grid_read <- variables$grid_read
    sp_read <- variables$sp_without_outliers
    sp_selected <- subset(sp_read, sp %in% input$selec_filter_sp_map, drop = FALSE)
    
    leaflet() %>%
      addProviderTiles("Esri.OceanBasemap", group = "Esri.OceanBasemap") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetmap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
      addLayersControl(baseGroups = c("OpenStreetmap","Esri.OceanBasemap", 'Esri.WorldImagery'),
                       options = layersControlOptions(collapsed = TRUE, autoZIndex = F)) %>%
      setView(lng = -60.85, lat = -15.45, zoom = 3) %>%
      addMarkers(
        data = sp_selected,
        label=~as.character(paste(sp_selected$sp, "lon:", sp_selected$lon, ", lat:", sp_selected$lat)),
        clusterOptions = markerClusterOptions()
      ) %>%
      addLabelOnlyMarkers(data = sp_selected,
                          lng = ~lon, lat = ~lat,
                          clusterOptions = markerClusterOptions()
      )
  }
})

output$map_grid_occ <- renderLeaflet({
  if (!is.null(variables$results)) {
    results <- variables$results
    
    sp_occ_total <- results[, results[nrow(results), ] != 0]
    
    sp_occ <- sp_occ_total[1:nrow(sp_occ_total)-1, 1:ncol(sp_occ_total)]
    
    sp_occ$X <- NULL
    
    sp_occ <- subset(sp_occ, TOTAL > 0)
    sp_occ$TOTAL <- NULL
    
    sp_occ <- unique(sp_occ)
    
    a <- c()
    for (i in 1:nrow(sp_occ)) {
      a <- c(a, i)
    }
    
    sp_occ$sp <-a 
    
    results_sp_names <- names(sp_occ)[1:(ncol(sp_occ)-3)]
    
    results_sp_names <- gsub(".", " ", results_sp_names, fixed = TRUE)
    for (i in 1:length(results_sp_names)) {
      sp_occ[sp_occ[ , i] == 1, ]$sp <- results_sp_names[i]
    }
    
    colors <- colorRampPalette(palette()[2:length(palette())-1])(length(unique(sp_occ$sp)))
    pal <- colorFactor(colors, domain = unique(sp_occ$sp))
    
    leaflet() %>%
      addProviderTiles("Esri.OceanBasemap", group = "Esri.OceanBasemap") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetmap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
      addLayersControl(baseGroups = c("OpenStreetmap","Esri.OceanBasemap", 'Esri.WorldImagery'),
                       options = layersControlOptions(collapsed = TRUE, autoZIndex = F)) %>%
      setView(lng = -60.85, lat = -15.45, zoom = 3) %>%
      addCircleMarkers(
        data = sp_occ,
        lng = as.numeric(sp_occ$lon),
        lat = as.numeric(sp_occ$lat),
        label = paste(sp_occ$sp, ", lon:", sp_occ$lon, ", lat:", sp_occ$lat),
        radius = 7,
        color = ~pal(sp_occ$sp),
        stroke = FALSE, fillOpacity = 0.3
      )
  }
})