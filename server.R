library(shinydashboard)
library(leaflet)
library(dplyr)
library(shiny)
library(DT)
library(ggplot2)
library(plotly)

function(input, output, session) {
  
  generate_result = function() {
    grid <- input$file1
    sp <- input$file2
    
    grid_read <- read.csv(grid$datapath, header = input$header,
                          sep = input$sep, quote = input$quote) 
    sp_read <- read.csv(sp$datapath, header = input$header,
                        sep = input$sep, quote = input$quote) 
    
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
  
  
  output$grid <- DT::renderDataTable({
    if (!is.null(input$file1)) {
      grid <- input$file1
      grid_read <- read.csv(grid$datapath, header = input$header,
                            sep = input$sep, quote = input$quote)
      grid_read
    }
  })
  
  output$sp <- DT::renderDataTable({
    if (!is.null(input$file2)) {
      sp <- input$file2
      sp_read <- read.csv(sp$datapath, header = input$header,
                          sep = input$sep, quote = input$quote) 
      data.frame(sp_read)
    }
  })
  
  output$result <- DT::renderDataTable({
    if (!is.null(input$file1) & !is.null(input$file2)) {
      generate_result()      
    }
  })
  
  output$download_results <- downloadHandler(
    filename = function(){"results.csv"},
    content = function(fname){
      write.csv(generate_result(), fname)
    }
  )
  
  output$species <- DT::renderDataTable({
    if (!is.null(input$file2)) {
      sp <- input$file2
      sp_read <- read.csv(sp$datapath, header = input$header,
                            sep = input$sep, quote = input$quote)
      df_freq <- count(sp_read, sp_read$sp)
      data.frame(Specie = df_freq$`sp_read$sp`, Freq = df_freq$n)
    }
  })
  
  output$scatter_plot <- renderPlotly({
    if (!is.null(input$file2) & !is.null(input$file1)) {
      grid <- input$file1
      sp <- input$file2
      
      sp_read <- read.csv(sp$datapath, header = input$header,
                          sep = input$sep, quote = input$quote)
      
      grid_read <- read.csv(grid$datapath, header = input$header,
                            sep = input$sep, quote = input$quote)
      plot_ly(data = sp_read, x = ~lat, y = ~lon, color = ~sp)
    }
  })
  
  output$map_grid <- renderLeaflet({
    if (!is.null(input$file1)) {
      grid <- input$file1
      # sp <- input$file2
      grid_read <- read.csv(grid$datapath, header = input$header,
                              sep = input$sep, quote = input$quote)
      grid_read
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
    if (!is.null(input$file2)) {
      sp <- input$file2
      sp_read <- read.csv(sp$datapath, header = input$header,
                         sep = input$sep, quote = input$quote)
      leaflet() %>%
        addProviderTiles("Esri.OceanBasemap", group = "Esri.OceanBasemap") %>%
        addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetmap") %>%
        addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
        addLayersControl(baseGroups = c("OpenStreetmap","Esri.OceanBasemap", 'Esri.WorldImagery'),
                         options = layersControlOptions(collapsed = TRUE, autoZIndex = F)) %>%
        setView(lng = -60.85, lat = -15.45, zoom = 3) %>%
        addMarkers(
          data = sp_read,
          label=~as.character(paste(sp_read$sp, "lon:", sp_read$lon, ", lat:", sp_read$lat)), 
          clusterOptions = markerClusterOptions()
        ) %>%
        addLabelOnlyMarkers(data = sp_read,
                            lng = ~lon, lat = ~lat,
                            # label = ~as.character(paste(sp_read$sp, "lon:", sp_read$lon, ", lat:", sp_read$lat)),
                            clusterOptions = markerClusterOptions()
                            # labelOptions = labelOptions(noHide = T,
                                                        # direction = "auto")
                            )
        # addCircleMarkers(
        #   lng = sp_read$lon,
        #   lat = sp_read$lat,
        #   popup = paste(sp_read$sp, "lon:", sp_read$lon, ", lat:", sp_read$lat),
        #   radius = 7,
        #   color = "green",
        #   stroke = FALSE, fillOpacity = 0.3
        # )
        
    }
  })
  
  

}

