library(shinydashboard)
library(leaflet)
library(dplyr)
library(shiny)
library(DT)
library(ggplot2)
library(plotly)

function(input, output, session) {

  variables <- reactiveValues(grid_read = NULL, sp_read = NULL, sp_without_outliers = NULL, results = NULL)
  
  filter_variables <- reactiveValues(sp_filter = NULL)
  
  observeEvent(input$file1, {
    if (!is.null(input$file1)) {
      grid <- input$file1
      variables$grid_read <- read.csv(grid$datapath, header = input$header,
                                    sep = input$sep, quote = input$quote)
    }
  })
  
  observeEvent(input$file2, {
    if (!is.null(input$file2)) {
      sp <- input$file2
      variables$sp_read <- read.csv(sp$datapath, header = input$header,
                                    sep = input$sep, quote = input$quote)
    }
  })
  
  observeEvent(variables$sp_read, {
    if (!is.null(variables$sp_read) & !is.null(variables$grid_read)) {
      variables$sp_without_outliers <- remove_species_outliers(variables$grid_read, variables$sp_read)
    }
  })
  
  observeEvent(variables$sp_without_outliers, {
    if (!is.null(variables$grid_read) & !is.null(variables$sp_without_outliers) & (nrow(variables$sp_without_outliers)*nrow(variables$grid_read) <= 100000)) {
      variables$results <- get_results(variables$grid_read, variables$sp_without_outliers)
    }
  })
  
  remove_species_outliers = function(grid_read, sp_read) {
    subset(sp_read, sp_read$lon >= min(grid_read$lon) 
                             & sp_read$lon <= max(grid_read$lon)
                             & sp_read$lat >= min(grid_read$lat) 
                             & sp_read$lat <= max(grid_read$lat))
  }
  
  get_species_freq = function() {
    if (!is.null(input$file2)) {
      sp_read <- variables$sp_read
      df_freq <- count(sp_read, sp_read$sp)
      data.frame(Specie = df_freq$`sp_read$sp`, Freq = df_freq$n)
    }
  }
  
  get_species_outliers = function() {
    if (!is.null(input$file2) & !is.null(input$file1)) {
      sp_read <- variables$sp_read
      grid_read <- variables$grid_read
      subset(sp_read, sp_read$lon < min(grid_read$lon) 
             | sp_read$lon > max(grid_read$lon)
             | sp_read$lat < min(grid_read$lat) 
             | sp_read$lat > max(grid_read$lat))
    }
  }
  
  get_species_outliers_freq = function() {
    sp_out <- get_species_outliers()
    df_freq <- count(sp_out, sp_out$sp)
    data.frame(Specie = df_freq$`sp_out$sp`, Freq = df_freq$n)
  }
  
  get_results = function(grid_read, sp_read) {
    sp_read <- variables$sp_without_outliers
    
    sp_freq <- count(sp_read, sp_read$sp)
    
    d = 0.5
    r = d/2
    
    result_all <- c()
    for (sp_index in 1:nrow(sp_read)) {
      result <- c()
      for (grid_index in 1:nrow(grid_read)) {
        result <- c(result, ifelse(sp_read[sp_index,2:3]$lon <= grid_read[grid_index,]$lon + r &
                                     sp_read[sp_index,2:3]$lon >= grid_read[grid_index,]$lon - r &
                                     sp_read[sp_index,2:3]$lat <= grid_read[grid_index,]$lat + r &
                                     sp_read[sp_index,2:3]$lat >= grid_read[grid_index,]$lat - r, 1, 0))
      }
      result_all <- c(result_all, result)
    }
    
    res_test <- array(result_all, dim = c(nrow(grid_read), nrow(sp_read)))
    
    colnames(res_test) <- sp_read$sp
    
    start_col <- 1
    
    res_sum_per_sp <- c()
    for (i in 1:nrow(sp_freq)) {
      res_sum_per_sp <- c(res_sum_per_sp, apply(res_test[, start_col:(start_col + sp_freq$n[i] - 1)], 1, sum))
      
      start_col <- sp_freq$n[i] + 1
    }
    
    res_sum_per_sp <- array(res_sum_per_sp, dim = c(nrow(grid_read), nrow(sp_freq)))
    
    colnames(res_sum_per_sp) <- sp_freq$`sp_read$sp`
    
    res_sum_per_sp_bin <- ifelse(res_sum_per_sp[ , ] > 0, 1, 0)
    
    df_res_sum_per_sp_bin <- data.frame(res_sum_per_sp_bin)
    
    df_res_sum_per_sp_bin$TOTAL <- apply(res_sum_per_sp_bin, 1, sum)
    
    df_res_sum_per_sp_bin["TOTAL", ] <- apply(df_res_sum_per_sp_bin, 2, sum)
    
    df_res_sum_per_sp_bin
  }
  
  
  # =====================================================================================
  
  output$grid <- DT::renderDataTable({
    if (!is.null(input$file1)) {
      variables$grid_read
    }
  })
  
  output$sp <- DT::renderDataTable({
    if (!is.null(input$file2) & !is.null(input$file1)) {
      remove_species_outliers(variables$grid_read, variables$sp_read)
      # variables$sp_without_outliers
    }
  })
  
  output$result <- DT::renderDataTable({
    if (!is.null(input$file1) & !is.null(input$file2)) {
      if ((nrow(variables$sp_read)*nrow(variables$grid_read) <= 100000)) {
        # generate_result(variables$grid_read, variables$sp_read)
        results <- variables$results
        # print(results[nrow(results), 2:(ncol(results))-1])
        # results[ ,  >= min(input$range)]
        # results[ , results[nrow(results), 2:(ncol(results))] <= max(input$range)]
      }
      else {
        showModal(modalDialog(
          title = "Hey",
          easyClose = TRUE,
          footer = NULL,
          "There are too many rows in those data. It would take a lot of time to generate the results with the current hardware.",
          br(),
          "But you still can analyze your data normally."
        ))
        data.frame(Message = c("There are too many rows in those data. It would take a lot of time to generate the results with the current hardware. But you still can analyze your data normally."))
      }
    }
  })
  
  # output$filter_sp_occ <- renderUI({
  #   results <- variables$results
  #   if (!is.null(results)) {
  #     total_occ_per_sp <- results[nrow(results), 2:ncol(results)-1]
  #     sliderInput("range", "Occurrence range:",
  #                 min = min(total_occ_per_sp), max = max(total_occ_per_sp),
  #                 value = c(min(total_occ_per_sp), max(total_occ_per_sp)))
  #   }
  # })
  
  output$download_results <- downloadHandler(
    filename = function(){"results.csv"},
    content = function(fname){
      if (!is.null(input$file2) & !is.null(input$file1)) {
        write.csv(variables$results, fname)
      }
    }
  )
  

  output$download_species_freq <- downloadHandler(
    filename = function(){"species_freq.csv"},
    content = function(fname){
      if (!is.null(input$file2) & !is.null(input$file1)) {
        write.csv(get_species_freq(), fname)
      }
    }
  )
  
  output$download_species_outliers <- downloadHandler(
    filename = function(){"species_outliers.csv"},
    content = function(fname){
      if (!is.null(input$file2) & !is.null(input$file1)) {
        write.csv(get_species_outliers(), fname)
      }
    }
  )
  
  output$download_species_outliers_freq <- downloadHandler(
    filename = function(){"species_outliers_freq.csv"},
    content = function(fname){
      if (!is.null(input$file2) & !is.null(input$file1)) {
        write.csv(get_species_outliers_freq(), fname)
      }
    }
  )
  
  output$species <- DT::renderDataTable({
    get_species_freq()
  })
  
  output$species_outliers <- DT::renderDataTable(({
    get_species_outliers()
  }))
  
  output$species_outliers_freq <- DT::renderDataTable(({
    if (!is.null(input$file2) & !is.null(input$file1)) {
      grid_read <- variables$grid_read
      sp_read <- variables$sp_read
      sp_subset <- subset(sp_read, sp_read$lon < min(grid_read$lon) 
             | sp_read$lon > max(grid_read$lon)
             | sp_read$lat < min(grid_read$lat) 
             | sp_read$lat > max(grid_read$lat))
      df_freq <- count(sp_subset, sp_subset$sp)
      data.frame(Specie = df_freq$`sp_subset$sp`, Freq = df_freq$n)
    }
  }))
  
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
      # grid_read <- variables$grid_read
      plot_ly(data = sp_selected, x = ~Specie, y = ~Freq, color = ~Specie)
    }
  })
  
  output$sp_out_freq_plot <- renderPlotly({
    if (!is.null(input$file2) & !is.null(input$file1)) {
      sp_out_freq <- get_species_outliers_freq()
      sp_selected <- subset(sp_out_freq, Specie %in% input$selec_filter_sp_map)
      # grid_read <- variables$grid_read
      plot_ly(data = sp_selected, x = ~Specie, y = ~Freq, color = ~Specie)
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
          popup = paste("lon:", grid_read$lon, ", lat:", grid_read$lat),
          radius = 7,
          color = "blue",
          stroke = FALSE, fillOpacity = 0.3
        )
    }
  })
  
  output$map_sp <- renderLeaflet({
    if (!is.null(input$file2) & !is.null(input$file1)) {
      # sp_read <- variables$sp_read
      grid_read <- variables$grid_read
      sp_read <- variables$sp_without_outliers
      
      sp_selected <- subset(sp_read, sp %in% input$selec_filter_sp_map)
      
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
          popup = paste(sp_selected$sp, "lon:", sp_selected$lon, ", lat:", sp_selected$lat),
          radius = 7,
          color = "orange",
          stroke = FALSE, fillOpacity = 0.3
        )
        
    }
  })
  
  output$map_sp_clustered <- renderLeaflet({
    if (!is.null(input$file2) & !is.null(input$file1)) {
      # sp_read <- variables$sp_read
      grid_read <- variables$grid_read
      
      sp_read <- variables$sp_without_outliers
      
      # print("-> subset species")
      # print(input$selec_filter_sp_map)
      print("-> sp_read")
      print(sp_read)
      print("-> subset - sp_read")
      print(subset(sp_read, sp == input$selec_filter_sp_map, drop = FALSE))

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
  
  output$filter_sp_map <- renderUI({
    if (!is.null(input$file2) & !is.null(input$file1)) {
      sp_read <- variables$sp_read
      species_name <- unique(sp_read$sp)
      selectInput("selec_filter_sp_map", label = h4("Select specie"),
                  choices = species_name,
                  selected = 1, multiple = TRUE)
    }
  })
  
  select_all_filter <- observeEvent(input$select_all_filter_btn, {
    sp_read <- variables$sp_read
    species_name = unique(sp_read$sp)
    updateSelectInput(session, "selec_filter_sp_map", label = h4("Select specie"), 
                      choices = species_name, 
                      selected = species_name)
  })
  
  clean_all_filter <- observeEvent(input$clean_all_filter_btn, {
    sp_read <- variables$sp_read
    species_name = unique(sp_read$sp)
    updateSelectInput(session, "selec_filter_sp_map", label = h4("Select specie"), 
                      choices = species_name, 
                      selected = 1)
  })
  
}

