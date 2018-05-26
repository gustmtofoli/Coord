library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(DT)

shinyServer(function(input, output) {
   
  output$contents_grid <- renderTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
  })
  
  output$contents_sp <- renderTable({
    inFile2 <- input$file2
    
    if (is.null(inFile2))
      return(NULL)
    
    read.csv(inFile2$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
  })
  
  output$occ <- renderTable({
   diam <- 5
   r <- diam/2
    
    grid <- input$file1
    sp <- input$file2
    
    if (is.null(grid) | is.null(sp))
      return(NULL)
    
    grid_read <- read.csv(grid$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
    
    sp_read <- read.csv(sp$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
    
    condition <- sp_read$lon <= grid_read$lon + r & sp_read$lon >= grid_read$lon - r &
      sp_read$lat <= grid_read$lat + r & sp_read$lat >= grid_read$lat - r
    
    
    
    # for (i in sp_read$lon) {
    #   for (j in grid_read$lon) {
    #     if (i <= j + r & i >= j - r) 
    #       print(1)
    #     else 
    #       print(0)
    #     
    #   }
    # }
    # 
    
    # sp_read$occ <- condition
    
    
    sp_read
    
  })
  
  
  
  output$map <- renderLeaflet({
    grid <- input$file1
    sp <- input$file2
    
    if (is.null(grid) | is.null(sp))
      return(NULL)
    
    grid_read <- read.csv(grid$datapath, header = input$header,
                          sep = input$sep, quote = input$quote)
    
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
  })
  
  output$grid <- DT::renderDataTable({
    grid <- input$file1
    grid_read <- read.csv(grid$datapath, header = input$header,
                          sep = input$sep, quote = input$quote) 
    grid_read
  })
  
  output$sp <- DT::renderDataTable({
    sp <- input$file2
    sp_read <- read.csv(sp$datapath, header = input$header,
                          sep = input$sep, quote = input$quote) 
    sp_read
  })
  
  output$result <- DT::renderDataTable({
    grid <- input$file1
    sp <- input$file2
    
    if (is.null(grid) | is.null(sp))
      return(NULL)
    
    grid_read <- read.csv(grid$datapath, header = input$header,
                          sep = input$sep, quote = input$quote) 
    sp_read <- read.csv(sp$datapath, header = input$header,
                        sep = input$sep, quote = input$quote) 
    
    result_df <- data.frame(Sp_lau = c(0,1,0,1), Sp_lau_2 = c(1,1,0,0))
    result_df
  })
  
})
