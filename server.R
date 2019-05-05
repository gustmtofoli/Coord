library(shinydashboard)
library(leaflet)
library(dplyr)
library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(raster)
library(caret)
library(pROC)
library(shinycssloaders)
library(highcharter)
library(spocc)
library(maptools)
library(rgdal)
library(sp)
library(caret)
# library(sdm)
library(dismo)
library(biomod2)
library(rgdal)

function(input, output, session) {

  variables <- reactiveValues(grid_read = NULL, 
                              sp_read = NULL, 
                              sp_download_db = NULL,
                              sp_download_db_processed = NULL,
                              sp_without_outliers = NULL, 
                              results = NULL)
  
  status <- reactiveValues(species_status = FALSE,
                           predictors_status = FALSE
                           # species_value = "",
                           # predictors_value = ""
                           )
  
  secondary_variables <- reactiveValues(duplicated_sp = NULL, 
                                        duplicated_grid = NULL,
                                        original_sp_nrow = 0,
                                        group_predictive_maps = FALSE)
  
  filter_variables <- reactiveValues(sp_filter = NULL)
  
  predict_variables <- reactiveValues(algorithms = NULL, 
                                      training = NULL, 
                                      testing = NULL,
                                      roc = NULL,
                                      auc = NULL,
                                      predictive_map = NULL,
                                      predictive_model = NULL,
                                      ensemble_model = NULL,
                                      execution_time = 0,
                                      can_run_algorithm = FALSE,
                                      # data_from_DB = NULL,
                                      ensemble_map = NULL,
                                      data_bases = c("gbif", 
                                                     "ecoengine",
                                                     "bison",
                                                     "inat",
                                                     "ebird",
                                                     "antweb",
                                                     "vertnet",
                                                     "idigbio",
                                                     "obis",
                                                     "ala"))
  
  predict_variables$algorithms <- data.frame(name = c("GAM - Generalized Linear Model", 
                                                      "RF - Random Forest",
                                                      "GLM - Logistic Regression",
                                                      "GBM - Gradient Boosting Machine",
                                                      "CTA - Classification Tree Analysis",
                                                      "ANN - Artificial Neural Network",
                                                      "BIOCLIM - Surface Range Envelop (SRE)",
                                                      "FDA - Flexible Discriminant Analysis",
                                                      "MARS - Multiple Adaptive Regression Splines "),
                                                       
                                             method = c("GAM", 
                                                        "RF",
                                                        "GLM",
                                                        "GBM",
                                                        "CTA",
                                                        "ANN",
                                                        "SRE",
                                                        "FDA",
                                                        "MARS")
                                                        
                                             )
  
  
  
  observeEvent(input$download_from_DB, {
    species_download <- c()
    
    if (input$upload_file_switch_btn) {
      species_download <- input$selec_filter_download_sp
    }
    else {
      species_download <- input$sp_name
    }
    
    showModal(modalDialog(
      title = "Searching Data",
      footer = NULL,
      easyClose = FALSE,
      paste0("Species: ", species_download),
      br(),
      paste0("Data base(s): ", input$select_data_bases)

    ))
    
    print("\n\n========================")
    print(input$select_data_bases)
    
    
    data_from_DB <- occ(species_download, from = input$select_data_bases)
    df_data <- occ2df(data_from_DB)
    colnames(df_data) <- c("sp", "lon", "lat", "data_base")
    print(df_data)
    if (!is.null(df_data) & nrow(df_data) > 0) {
      variables$sp_download_db <- df_data[, 1:4]
      status$species_status <- TRUE
      showModal(modalDialog(
        title = "Nice work!!",
        footer = NULL,
        easyClose = TRUE
      ))
    }
    else {
      showModal(modalDialog(
        title = "Oh no :(",
        footer = NULL,
        easyClose = TRUE,
        paste0("No records found in ", input$select_data_bases, " for ", input$sp_name)
      ))
    }
  })
  
  observeEvent(input$predictors_files, {
    predict_variables$can_run_algorithm <- FALSE
    status$predictors_status <- TRUE
  })
  
  observeEvent(input$occ_file, {
    predict_variables$can_run_algorithm <- FALSE
  })
  
  observeEvent(input$training_set, {
    predict_variables$can_run_algorithm <- FALSE
  })
  
  # ========== SERVICES ===============================================================
  
  source("PredictService.R", local=TRUE)
  source("PresenceAbsenceService.R", local=TRUE)
  
  # ===================================================================================
  
  # ========== OUTPUTS ===============================================================
  
  source("PredictOutputs.R", local=TRUE)
  source("PresenceAbsenceOutputs.R", local=TRUE)
  
  # ===================================================================================
  
  
  # ========== FUNCTIONS ===============================================================
  
  source("SpeciesFreq.R", local=TRUE)
  source("SpeciesOutliers.R", local=TRUE)
  source("CalculatePresenceAbsense.R", local=TRUE)
  source("RunAlgorithm.R", local=TRUE)
  
  # ===================================================================================

 output$download_data_from_db_btn <- downloadHandler(
    filename = function() { "downloaded_species_occ.csv" },
    content = function(fname) {
      if (!is.null(variables$sp_download_db)) {
        write.csv(unique(na.omit(variables$sp_download_db)), fname)
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
  
  clean_all_filter <- observeEvent(input$clean_all_filter_btn, {
    sp_download_db <- variables$sp_download_db
    species_name = unique(sp_download_db$sp)
    updateSelectInput(session, "selec_filter_sp_map", label = h4("Select species"), 
                      choices = species_name, 
                      selected = 1)
  })
  
  observeEvent(input$selet_all_download_sp_btn, {
    if (!is.null(input$file_species_download)) {
      uploaded_file <- input$file_species_download
      uploaded_data <- read.csv(uploaded_file$datapath, header = TRUE,
                                 sep = ",")
      species_name = unique(uploaded_data$sp)
      updateSelectInput(session, "selec_filter_download_sp", label = h4("Select species"), 
                        choices = species_name, 
                        selected = species_name)
    }
  })
  
  observeEvent(input$clean_download_sp_btn, {
    if (!is.null(input$file_species_download)) {
      uploaded_file <- input$file_species_download
      uploaded_data <- read.csv(uploaded_file$datapath, header = TRUE,
                                sep = ",")
      species_name = unique(uploaded_data$sp)
      updateSelectInput(session, "selec_filter_download_sp", label = h4("Select species"), 
                        choices = species_name, 
                        selected = 1)
    }
  })
  
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
  
  output$duplicated_sp <- DT::renderDataTable({
    secondary_variables$duplicated_sp
  })
  
  output$download_duplicated_sp <- downloadHandler(
    filename = function(){"duplicated_records_species.csv"},
    content = function(fname){
      if (!is.null(input$file2) & !is.null(input$file1)) {
        write.csv(secondary_variables$duplicated_sp, fname)
      }
    }
  )
  
  output$duplicated_grid <- DT::renderDataTable({
    secondary_variables$duplicated_grid
  })
  
  output$download_duplicated_grid <- downloadHandler(
    filename = function(){"duplicated_records_grid.csv"},
    content = function(fname){
      if (!is.null(input$file2) & !is.null(input$file1)) {
        write.csv(secondary_variables$duplicated_grid, fname)
      }
    }
  )
  
  output$show_predictors <- renderUI({
    selectInput("select_predictors", label = "",
               choices = input$predictors_files,
               selected = 1)
  })
  

  output$show_predictors_test <- renderPlot({
    if (!is.null(input$predictors_files)) {
      datafiles <- input$predictors_files
      stck = stack() 
      for(i in 1:NROW(datafiles)){
        tempraster = raster(datafiles[i, ]$datapath)
        stck = stack(stck,tempraster)
      }
      names(stck) <- datafiles$name
      n_array <- c()
      for (i in 1:length(names(stck))) {
        n_array <- c(n_array, i)
      }
      df_select <- data.frame(n = n_array, predictors = names(stck))
      x = subset(df_select, predictors %in% input$select_predictors)
      plot(stck, x$n)
    }
  })

  output$show_auc_curve <- renderPlot({
    # if (!is.null(predict_variables$roc) & !is.null(predict_variables$auc)) {
    #   plot(predict_variables$roc)
    #   text(0.5,0.5,paste("AUC = ",format(predict_variables$auc, digits=5, scientific=FALSE)))
    # }
    if (!is.null(predict_variables$roc)) {
      predict_variables$roc
      # text(0.5,0.5,paste("AUC = ",format(predict_variables$auc, digits=5, scientific=FALSE)))
    }
  })
  
  
  
  output$sp_duplicated_percent <- renderInfoBox({
    duplicated_percent <- (nrow(secondary_variables$duplicated_sp) / secondary_variables$original_sp_nrow)*100
    infoBox(
      "Sp - Duplicated ocurrences", 
      paste0(round(duplicated_percent, 2), "%"),
      paste0(nrow(secondary_variables$duplicated_sp), " of ", secondary_variables$original_sp_nrow, " occ."),
      icon = icon("list"),
      color = "light-blue", 
      fill = TRUE
    )
  })
  
  output$grid_duplicated_percent <- renderInfoBox({
    duplicated_percent <- (nrow(secondary_variables$duplicated_grid) / nrow(variables$grid_read))*100
    infoBox(
      "Grid - Duplicated occurences", 
      paste0(round(duplicated_percent, 2), "%"),
      paste0(nrow(secondary_variables$duplicated_grid), " of ", nrow(variables$grid_read), " occ."),
      icon = icon("list"),
      color = "light-blue", 
      fill = TRUE
    )
  })
  
  output$sp_outliers_percent <- renderInfoBox({
    outliers <- get_species_outliers()
    outliers_percent <- (nrow(outliers) / secondary_variables$original_sp_nrow)*100
    infoBox(
      "Sp - Outliers", 
      paste0(round(outliers_percent, 2), "%"),
      paste0(nrow(outliers), " of ", secondary_variables$original_sp_nrow, " occ."),
      icon = icon("list"),
      color = "light-blue", 
      fill = TRUE
    )
  })
  
  output$sp_total_percent <- renderInfoBox({
    total_percent <- (nrow(variables$sp_read) / secondary_variables$original_sp_nrow)*100
    infoBox(
      "Used occurrences",
      paste0("Used: ", round(total_percent, 2), "%", " (",nrow(variables$sp_read), " occ.)"),
      paste0("Removed: ", 100 - round(total_percent, 2), "%", " (", secondary_variables$original_sp_nrow - nrow(variables$sp_read), " occ.)"),
      icon = icon("list"),
      color = "light-blue", 
      fill = TRUE
    )
  })
  
  output$number_of_species <- renderInfoBox({
    number_of_species <- length(unique(variables$sp_read$sp))
    infoBox(
      "Number of species", 
      paste0(number_of_species),
      icon = icon("list"),
      color = "light-blue", 
      fill = TRUE
    )
  })
  
  output$select_DB <- renderUI({
    selectInput("select_data_bases", label = "Select Data base: ",
                choices = predict_variables$data_bases,
                selected = 1, multiple = TRUE)
  })
  
  output$filter_sp_download <- renderUI({
    if (!is.null(input$file_species_download)) {
      file_species_download <- input$file_species_download
      species_download <- grid_read <- read.csv(file_species_download$datapath, header = input$header,
                                      sep = input$sep, quote = input$quote)
      species_name <- unique(species_download$sp)
      selectInput("selec_filter_download_sp", label = h5("Select species"),
                  choices = species_name,
                  selected = 1, multiple = TRUE)
    }
  })
  
  output$sp_download_na <- renderInfoBox({
    downloaded_species <- variables$sp_download_db
    total_nrow <- nrow(downloaded_species)
    number_of_na <- abs(nrow(na.omit(downloaded_species)) - total_nrow)
    infoBox(
      "Number of empty records",
      paste0(round(((100*number_of_na)/total_nrow), 2), "%"),
      paste0(number_of_na, " of ", nrow(downloaded_species)),
      icon = icon("list"),
      color = "light-blue", 
      fill = TRUE
    )
  })
  
  output$sp_download_count <- renderInfoBox({
    downloaded_species <- variables$sp_download_db
    number_of_species <- length(unique(downloaded_species$sp))
    infoBox(
      "Number of Species",
      paste0(number_of_species),
      icon = icon("list"),
      color = "light-blue", 
      fill = TRUE
    )
  })
  
  output$sp_download_duplicated <- renderInfoBox({
    downloaded_species <- variables$sp_download_db
    downloaded_species.na.omit <- na.omit(downloaded_species)
    n_duplicated_rows <- nrow(downloaded_species[duplicated(downloaded_species.na.omit), ])
    infoBox(
      "Duplicated occurences",
      paste0(round(((100*n_duplicated_rows)/nrow(downloaded_species)), 2), "%"),
      paste0(n_duplicated_rows, " of ", nrow(downloaded_species)),
      icon = icon("list"),
      color = "light-blue", 
      fill = TRUE
    )
  })
  
  output$sp_download_db <- renderInfoBox({
    downloaded_species <- variables$sp_download_db
    n_data_bases <- length(unique(downloaded_species$data_base))
    infoBox(
      "Data Bases with records",
      paste0(n_data_bases),
      icon = icon("list"),
      color = "light-blue",
      fill = TRUE
    )
  })
  
  output$show_downloaded_data <- DT::renderDataTable({
    unique(na.omit(variables$sp_download_db))
  })
}

