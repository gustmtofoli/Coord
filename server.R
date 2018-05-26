library(shinydashboard)
library(leaflet)
library(dplyr)
library(shiny)
library(DT)

function(input, output, session) {
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
      sp_read
    }
  })
  
  output$map <- renderLeaflet({
    
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
  
  output$result <- DT::renderDataTable({
    if (!is.null(input$file1) & !is.null(input$file2)) {
      grid <- input$file1
      sp <- input$file2
      
      grid_read <- read.csv(grid$datapath, header = input$header,
                            sep = input$sep, quote = input$quote) 
      sp_read <- read.csv(sp$datapath, header = input$header,
                          sep = input$sep, quote = input$quote) 
      result_df <- data.frame(Sp_lau = c(0,1,0,1), Sp_lau_2 = c(1,1,0,0))
      result_df
    }
    
  })
  
  output$total_grid_coordinates <- renderInfoBox({
    total <- 0
    if (!is.null(input$file2)) {
      sp <- input$file2
      sp_read <- read.csv(sp$datapath, header = input$header,
                            sep = input$sep, quote = input$quote) 
      total <- length(unique(sp_read$sp))
    }
    
    infoBox(
      "Species", total, icon = icon("list"),
      color = "navy", fill = TRUE
    )
  })
  
}

