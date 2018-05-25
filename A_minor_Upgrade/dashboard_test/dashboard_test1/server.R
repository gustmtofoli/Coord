library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl) # make the jsonlite suggested dependency explicit
library(stream)
library(shiny)
library(devtools)
# library(shinySignals) #devtools::install_github("hadley/shinySignals")
library(DT)

function(input, output, session) {
  
 
  
  # if (is.null(grid) | is.null(sp))
  #   return(NULL)
  # 
  # grid_read <- read.csv(grid$datapath, header = input$header,
  #                       sep = input$sep, quote = input$quote)
  # 
  # sp_read <- read.csv(sp$datapath, header = input$header,
  #                     sep = input$sep, quote = input$quote)
  # 
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
  
  
  
  
  # pkgStream <- packageStream(session)
  
  # maxAgeSecs <- 60 * 5
  
  # pkgData <- packageData(pkgStream, maxAgeSecs)
  
  # dlCount <- downloadCount(pkgStream)
  
  # usrCount <- userCount(pkgStream)
  
  # startTime <- as.numeric(Sys.time())
  
  # output$rate <- renderValueBox({
  #   elapsed <- as.numeric(Sys.time()) - startTime
  #   downloadRate <- nrow(pkgData()) / min(maxAgeSecs, elapsed)
  #   
  #   valueBox(
  #     value = formatC(downloadRate, digits = 1, format = "f"),
  #     subtitle = "Downloads per sec (last 5 min)",
  #     icon = icon("area-chart"),
  #     color = if (downloadRate >= input$rateThreshold) "yellow" else "aqua"
  #   )
  # })
  # 
  # output$count <- renderValueBox({
  #   valueBox(
  #     value = dlCount(),
  #     subtitle = "Total downloads",
  #     icon = icon("download")
  #   )
  # })
  # 
  # output$users <- renderValueBox({
  #   valueBox(
  #     usrCount(),
  #     "Unique users",
  #     icon = icon("users")
  #   )
  # })
  # 
  # output$packagePlot <- renderBubbles({
  #   if (nrow(pkgData()) == 0)
  #     return()
  #   
  #   order <- unique(pkgData()$package)
  #   df <- pkgData() %>%
  #     group_by(package) %>%
  #     tally() %>%
  #     arrange(desc(n), tolower(package)) %>%
  #     # Just show the top 60, otherwise it gets hard to see
  #     head(60)
  #   
  #   bubbles(df$n, df$package, key = df$package)
  # })
  # 
  # output$packageTable <- renderTable({
  #   pkgData() %>%
  #     group_by(package) %>%
  #     tally() %>%
  #     arrange(desc(n), tolower(package)) %>%
  #     mutate(percentage = n / nrow(pkgData()) * 100) %>%
  #     select("Package name" = package, "% of downloads" = percentage) %>%
  #     as.data.frame() %>%
  #     head(15)
  # }, digits = 1)
  # 
  # output$downloadCsv <- downloadHandler(
  #   filename = "cranlog.csv",
  #   content = function(file) {
  #     write.csv(pkgData(), file)
  #   },
  #   contentType = "text/csv"
  # )
  # 
  # output$rawtable <- renderPrint({
  #   orig <- options(width = 1000)
  #   print(tail(pkgData(), input$maxrows), row.names = FALSE)
  #   options(orig)
  # })
}

