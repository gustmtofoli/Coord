library(shinydashboard)
library(leaflet)
library(dplyr)
library(shiny)
library(DT)
library(ggplot2)

function(input, output, session) {
  output$grid <- DT::renderDataTable({
    if (!is.null(input$file1)) {
      grid <- input$file1
      grid_read <- read.csv(grid$datapath, header = input$header,
                            sep = input$sep, quote = input$quote)
      grid_read
    }
  })
  
  output$species <- DT::renderDataTable({
    if (!is.null(input$file2)) {
      sp <- input$file2
      sp_read <- read.csv(sp$datapath, header = input$header,
                            sep = input$sep, quote = input$quote)
      df_freq <- count(sp_read, sp_read$sp)
      data.frame(Specie = df_freq$`sp_read$sp`, Freq = df_freq$n)
    }
  })
  
  output$sp <- DT::renderDataTable({
    if (!is.null(input$file2)) {
      sp <- input$file2
      sp_read <- read.csv(sp$datapath, header = input$header,
                          sep = input$sep, quote = input$quote) 
      sp_read
    }
  })
  
  output$scatter_plot <- renderPlot({
    if (!is.null(input$file2) & !is.null(input$file1)) {
      grid <- input$file1
      sp <- input$file2

       sp_read <- read.csv(sp$datapath, header = input$header,
                          sep = input$sep, quote = input$quote)

      grid_read <- read.csv(grid$datapath, header = input$header,
                            sep = input$sep, quote = input$quote)
      
      ggplot(data = sp_read, aes(x = lat, y = lon)) +
          geom_point(aes(color = sp))
    }
  })
  
  output$boxplot <- renderPlot({
    if (!is.null(input$file2) & !is.null(input$file1)) {
      grid <- input$file1
      sp <- input$file2
      
      sp_read <- read.csv(sp$datapath, header = input$header,
                          sep = input$sep, quote = input$quote)
      
      grid_read <- read.csv(grid$datapath, header = input$header,
                            sep = input$sep, quote = input$quote)
      
      ggplot(data = sp_read, aes(x = lat, y = lon)) +
        geom_boxplot(aes(color = sp))
    }
  })
  
  output$map_sp <- renderLeaflet({
    
    if (!is.null(input$file2)) {
      # grid <- input$file1
      sp <- input$file2
      
      # grid_read <- read.csv(grid$datapath, header = input$header,
                            # sep = input$sep, quote = input$quote)
      
      sp_read <- read.csv(sp$datapath, header = input$header,
                        sep = input$sep, quote = input$quote)
      # lon <- c(-90.85, -96.85)
      # lat <- c(30.45, 36.45)
      pal <- colorFactor(c("navy", "red"), domain = c("ship", "pirate"))
      leaflet() %>%
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
        ) %>%
        setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
        # addMarkers(lon, lat) %>%
        addCircleMarkers(
          # lng = ~ifelse(is.null(sp_read), 0, sp_read$lon),
          # lat = ~ifelse(is.null(sp_read), 0,sp_read$lat),
          lng = sp_read$lon,
          lat = sp_read$lat,
          popup = sp_read$sp,
          radius = 5,
          color = "red",
          stroke = FALSE, fillOpacity = 0.5
        )
      
    }
      # return(NULL)
  })
  
  output$map_grid <- renderLeaflet({
    
    if (!is.null(input$file1)) {
      # grid <- input$file1
      grid <- input$file1
      
      # grid_read <- read.csv(grid$datapath, header = input$header,
      # sep = input$sep, quote = input$quote)
      
      grid_read <- read.csv(grid$datapath, header = input$header,
                          sep = input$sep, quote = input$quote)
      # lon <- c(-90.85, -96.85)
      # lat <- c(30.45, 36.45)
      pal <- colorFactor(c("navy", "red"), domain = c("ship", "pirate"))
      leaflet() %>%
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
        ) %>%
        setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
        # addMarkers(lon, lat) %>%
        addCircleMarkers(
          # lng = ~ifelse(is.null(sp_read), 0, sp_read$lon),
          # lat = ~ifelse(is.null(sp_read), 0,sp_read$lat),
          lng = grid_read$lon,
          lat = grid_read$lat,
          # popup = grid_read$sp,
          radius = 5,
          color = "blue",
          stroke = FALSE, fillOpacity = 0.5
        )
      
    }
    # return(NULL)
  })
  
  output$result <- DT::renderDataTable({
    if (!is.null(input$file1) & !is.null(input$file2)) {
      grid <- input$file1
      sp <- input$file2
      
      grid_read <- read.csv(grid$datapath, header = input$header,
                            sep = input$sep, quote = input$quote) 
      sp_read <- read.csv(sp$datapath, header = input$header,
                          sep = input$sep, quote = input$quote) 
      
      
      # teste <- data.frame(sp = c('A', 'A', 'A', 'B', 'B'), lon = c(10, 7, 5, 5, 11), lat = c(4, 2, 7, 7, 8))

      # sp_freq <- count(teste, teste$sp)
      
      sp_freq <- count(sp_read, sp_read$sp)
      
      d = 4
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
      
      # col_sum = apply(res_test, 2, sum)
      res <- data.frame(row_sum = apply(res_test, 1, sum))

      res
    }
  })
}

