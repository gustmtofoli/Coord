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
library(sdm)
library(dismo)

function(input, output, session) {

  variables <- reactiveValues(grid_read = NULL, 
                              sp_read = NULL, 
                              sp_without_outliers = NULL, 
                              results = NULL)
  
  secondary_variables <- reactiveValues(duplicated_sp = NULL, 
                                        duplicated_grid = NULL,
                                        original_sp_nrow = 0)
  
  filter_variables <- reactiveValues(sp_filter = NULL)
  
  predict_variables <- reactiveValues(algorithms = NULL, 
                                      training = NULL, 
                                      testing = NULL,
                                      roc = NULL,
                                      auc = NULL,
                                      predictive_map = NULL,
                                      execution_time = 0,
                                      can_run_algorithm = FALSE,
                                      data_from_DB = NULL,
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
  
  predict_variables$algorithms <- data.frame(name = c("SVM - Support Vector Machine", 
                                                      "Random Forest",
                                                      "GLM - Logistic Regression",
                                                      "GBM - Gradient Boosting Machine",
                                                      "KNN - k-nearest neighbors"), 
                                             method = c("svm", 
                                                        "rf",
                                                        "glm",
                                                        "gbm",
                                                        "knn")
                                             )
  
  observeEvent(input$run_algorithm_btn, {
    predict_variables$can_run_algorithm <- TRUE
  })
  
  observeEvent(input$download_from_DB, {
    showModal(modalDialog(
      title = "Hmmm...",
      footer = NULL,
      easyClose = FALSE,
      "Searching Data..."
    ))
    data_from_DB <- occ(input$sp_name, from = input$select_data_bases)
    df_data <- occ2df(data_from_DB)
    colnames(df_data) <- c("sp", "long", "lat")
    if (!is.null(df_data) & nrow(df_data) > 0) {
      variables$sp_read <- df_data[, 1:3]
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
  
  observeEvent(input$select_input_algorithm, {
    predict_variables$can_run_algorithm <- FALSE
  })
  
  observeEvent(input$predictors_files, {
    predict_variables$can_run_algorithm <- FALSE
  })
  
  observeEvent(input$occ_file, {
    predict_variables$can_run_algorithm <- FALSE
  })
  
  observeEvent(input$training_set, {
    predict_variables$can_run_algorithm <- FALSE
  })
  
  observeEvent(input$file1, {
    if (!is.null(input$file1)) {
      grid <- input$file1
      variables$grid_read <- read.csv(grid$datapath, header = input$header,
                                    sep = input$sep, quote = input$quote)
      secondary_variables$duplicated_grid <- variables$grid_read[duplicated(variables$grid_read), ]
      variables$grid_read <- unique(variables$grid_read)
    }
  })
  
  observeEvent(input$file2, {
    if (!is.null(input$file2)) {
      sp <- input$file2
      variables$sp_read <- read.csv(sp$datapath, header = input$header,
                                    sep = input$sep, quote = input$quote)
      secondary_variables$original_sp_nrow <- nrow(variables$sp_read)
      secondary_variables$duplicated_sp <- variables$sp_read[duplicated(variables$sp_read), ]
      variables$sp_read <- unique(variables$sp_read)
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
    # lon_bound <- c()
    # lat_bound <- c()
    # for (i in 1:length(unique(grid_read$lon))) {
    #   subset_df <- subset(grid_read, lon == unique(grid_read$lon)[i])
    #   subset_df <- subset_df[order(subset_df$lat, decreasing = FALSE), ]
    #   lon_bound <- c(lon_bound, subset_df[1, ]$lon)
    #   lon_bound <- c(lon_bound, subset_df[nrow(subset_df), ]$lon)
    #   lat_bound <- c(lat_bound, subset_df[1, ]$lat)
    #   lat_bound <- c(lat_bound, subset_df[nrow(subset_df), ]$lat)
    # }
    # 
    # for (i in 1:length(unique(grid_read$lat))) {
    #   subset_df <- subset(grid_read, lat == unique(grid_read$lat)[i])
    #   subset_df <- subset_df[order(subset_df$lon, decreasing = FALSE), ]
    #   lon_bound <- c(lon_bound, subset_df[1, ]$lon)
    #   lon_bound <- c(lon_bound, subset_df[nrow(subset_df), ]$lon)
    #   lat_bound <- c(lat_bound, subset_df[1, ]$lat)
    #   lat_bound <- c(lat_bound, subset_df[nrow(subset_df), ]$lat)
    # }
    # 
    # boundary_coord <- data.frame(lon = lon_bound, lat = lat_bound)
    # boundary_coord <- unique(boundary_coord)
    
    # print(boundary_coord)
    
    
    # subset(sp_read, 
    #                 sp_read$lon <= boundary_coord$lon
    #               & sp_read$lon > boundary_coord$lon
    #               & sp_read$lat <= boundary_coord$lat
    #               & sp_read$lat > boundary_coord$lat
    #        )
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
    
    grid_read["-", ] <- "-"
    
    df_res_sum_per_sp_bin$lon <- grid_read$lon
    
    df_res_sum_per_sp_bin$lat <- grid_read$lat
    
    df_res_sum_per_sp_bin
    
  }
  
  runAlgorithm = function(predictors, pres_abs) {
    if (predict_variables$can_run_algorithm) {
      
    
      start_time <- Sys.time()
      showModal(modalDialog(
        title = "Hey",
        footer = NULL,
        easyClose = TRUE,
        "This may take some time... coffee?"
      ))
      
      
      
      datafiles <- predictors
      
      stck = stack() 
      names_stack <- c()
      for(i in 1:NROW(datafiles)){
        names_stack <- c(names_stack, substring(datafiles[i, ]$name, 1, nchar(datafiles[i, ]$name) - 4))
        tempraster = raster(datafiles[i, ]$datapath)
        stck = stack(stck,tempraster)
      }
      names(stck) <- names_stack
      print(stck)
      
      data <- pres_abs
      
      data <- data[, 2:3]
      print(data)
      
      
     
      prs1 = extract(stck, data)
      prs1_df <- data.frame(prs1)
      prs1_df$long <- data$long
      prs1_df$lat <- data$lat
      print(prs1)
      
      n <- runif(1, min=1, max=999999);
      set.seed(n)
      backgr = randomPoints(stck, nrow(data)) 
      absvals = extract(stck, backgr) 
      absvals_df <- data.frame(absvals)
      absvals_df$long <- backgr[, 'x']
      absvals_df$lat <- backgr[, 'y']
      pb = c(rep(1, nrow(prs1)), rep(0, nrow(absvals)))
      print(pb)
      sdmdata = data.frame(cbind(pb, rbind(prs1_df, absvals_df)))
      # head(sdmdata)
      sdmdata=na.omit(sdmdata)
      # summary(sdmdata)
      
      WGScoor2 <- sdmdata
      coordinates(WGScoor2)=~long+lat
      proj4string(WGScoor2)<- CRS("+proj=longlat +datum=WGS84")
      sdmData_shapefile <-spTransform(WGScoor2,CRS("+proj=longlat"))
      
      d <- sdmData(formula=pb~., train=sdmData_shapefile, predictors=stck)
      
      
      algorithm_selected <- subset(predict_variables$algorithms, name %in% input$select_input_algorithm)$method
      
      algorithm <- c()
      for (algo in algorithm_selected) {
        algorithm <- c(algorithm, algo)
      }
      
      m <- sdm(pb~.,data=d,methods=algorithm, replicatin='sub', 
               test.percent = (100 - as.numeric(input$training_set)), n = 2)
      print(m)
      
      
      
      # pa <- read.csv(occ_file$datapath, header = input$header,
      #                sep = input$sep, quote = input$quote)
      # n <- runif(1, min=1, max=999999);
      # set.seed(n)
      # 
      # trainIndex = createDataPartition(pa$pb, p = as.numeric(input$training_set)/100, 
      #                                  list = FALSE,
      #                                  times = 1) 
      # 
      # training = pa[ trainIndex,]
      # predict_variables$training <- training
      # testing= pa[-trainIndex,]
      # predict_variables$testing <- testing
      # train_control = trainControl(method="cv", number=10)
      # algorithm_selected <- subset(predict_variables$algorithms, name %in% input$select_input_algorithm)$method
      # 
      # if (algorithm_selected == "glm") {
      #   mod_fit1 = train(pb~.,
      #                    data=training, trControl=train_control, method=algorithm_selected, family="binomial")
      # }
      # 
      # if (algorithm_selected == "gbm") {
      #   mod_fit1 = train(pb~.,
      #                    data=training, trControl=train_control, method=algorithm_selected)
      # }
      # else {
      #   mod_fit1 = train(pb~.,
      #                  data=training, trControl=train_control, method=algorithm_selected, importance=TRUE)
      # }
      
      
      
      
      # p1=predict(mod_fit1, newdata=testing) 
      # roc = pROC::roc(testing[,"pb"], p1)
      # predict_variables$roc <- roc
      # auc= pROC::auc(roc)
      # predict_variables$auc <- auc
      
      
      
      
      
      
      # p1 = predict(stck, mod_fit1)
      # predict_variables$predictive_map <- p1
      end_time <- Sys.time()
      predict_variables$execution_time = round((end_time - start_time), 2)
      
      showModal(modalDialog(
        title = "Nice work!",
        footer = NULL,
        easyClose = TRUE
      ))
    }
  }
  
  
  # =====================================================================================
  
  output$grid <- DT::renderDataTable({
    if (!is.null(input$file1)) {
      variables$grid_read
    }
  })
  
  output$sp <- DT::renderDataTable({
    if (!is.null(input$file2) & !is.null(input$file1)) {
      variables$sp_read
    }
  })
  
  # output$result <- DT::renderDataTable({
  #   if (!is.null(input$file1) & !is.null(input$file2)) {
  #     if ((nrow(variables$sp_read)*nrow(variables$grid_read) <= 100000)) {
  #       results <- variables$results
  #     }
  #     else {
  #       showModal(modalDialog(
  #         title = "Hey",
  #         easyClose = TRUE,
  #         footer = NULL,
  #         "There are too many rows in those data. It would take a lot of time to generate the results with the current hardware.",
  #         br(),
  #         "But you still can explore your data."
  #       ))
  #       data.frame(Message = c("There are too many rows in those data. It would take a lot of time to generate the results with the current hardware. But you still can explore your data."))
  #     }
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
  
  
  output$duplicated_grid <- DT::renderDataTable({
    secondary_variables$duplicated_grid
  })
  
  output$show_predictors <- renderUI({
    selectInput("select_predictors", label = "",
               choices = input$predictors_files,
               selected = 1)
  })
  
  library(rgdal)
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
    if (!is.null(predict_variables$roc) & !is.null(predict_variables$auc)) {
      plot(predict_variables$roc)
      text(0.5,0.5,paste("AUC = ",format(predict_variables$auc, digits=5, scientific=FALSE)))
    }
  })
  
  # output$show_predict_map <- renderPlot({
  #   if (!is.null(input$predictors_files) & predict_variables$can_run_algorithm) {
  #     runAlgorithm(input$predictors_files, input$sp_read)
  #     title_predictive_map <- paste0("Predictive Map - ", input$select_input_algorithm)
  #     plot(predict_variables$predictive_map, main = title_predictive_map)
  #   }
  # })
  
  output$select_algorithm <- renderUI({
    selectInput("select_input_algorithm", label = "Choose Algorithm",
                choices = predict_variables$algorithms,
                selected = 1, multiple = TRUE)
  })
  
  output$info_training_testing <- DT::renderDataTable({
    if (!is.null(input$predictors_files) & predict_variables$can_run_algorithm) {
      runAlgorithm(input$predictors_files, variables$sp_read)
      df_info <- data.frame(info = c('Execution time: ', 
                                     'Algorithm: ', 
                                     'Training set: ', 
                                     'Test set: ',
                                     'AUC: '), 
                            value = c(as.character(predict_variables$execution_time), 
                                      input$select_input_algorithm, 
                                      paste0(input$training_set, "%"), 
                                      paste0(100 - as.numeric(input$training_set), "%"),
                                      as.character(round(predict_variables$auc, 5))))
      df_info
    }
  })
  
  output$sp_duplicated_percent <- renderInfoBox({
    duplicated_percent <- (nrow(secondary_variables$duplicated_sp) / secondary_variables$original_sp_nrow)*100
    infoBox(
      "Sp - Duplicated", 
      paste0(round(duplicated_percent, 2), "%"),
      paste0(nrow(secondary_variables$duplicated_sp), " of ", secondary_variables$original_sp_nrow),
      icon = icon("list"),
      color = "green", 
      fill = TRUE
    )
  })
  
  output$grid_duplicated_percent <- renderInfoBox({
    duplicated_percent <- (nrow(secondary_variables$duplicated_grid) / nrow(variables$grid_read))*100
    infoBox(
      "Grid - Duplicated", 
      paste0(round(duplicated_percent, 2), "%"),
      paste0(nrow(secondary_variables$duplicated_grid), " of ", nrow(variables$grid_read)),
      icon = icon("list"),
      color = "green", 
      fill = TRUE
    )
  })
  
  output$sp_outliers_percent <- renderInfoBox({
    outliers <- get_species_outliers()
    outliers_percent <- (nrow(outliers) / secondary_variables$original_sp_nrow)*100
    infoBox(
      "Sp - Outliers", 
      paste0(round(outliers_percent, 2), "%"),
      paste0(nrow(outliers), " of ", secondary_variables$original_sp_nrow),
      icon = icon("list"),
      color = "green", 
      fill = TRUE
    )
  })
  
  output$sp_total_percent <- renderInfoBox({
    total_percent <- (nrow(variables$sp_read) / secondary_variables$original_sp_nrow)*100
    infoBox(
      "Sp - Total", 
      paste0("Used: ", round(total_percent, 2), "%", " (",nrow(variables$sp_read), ")"),
      paste0("Removed: ", 100 - round(total_percent, 2), "%", " (", secondary_variables$original_sp_nrow - nrow(variables$sp_read), ")"),
      icon = icon("list"),
      color = "green", 
      fill = TRUE
    )
  })
  
  output$number_of_species <- renderInfoBox({
    number_of_species <- length(unique(variables$sp_read$sp))
    infoBox(
      "Number of species", 
      paste0(number_of_species),
      icon = icon("list"),
      color = "green", 
      fill = TRUE
    )
  })
  
  output$select_DB <- renderUI({
    selectInput("select_data_bases", label = "Select Data base: ",
                choices = predict_variables$data_bases,
                selected = 1, multiple = TRUE)
  })
  
  output$show_downloaded_data <- DT::renderDataTable({
    variables$sp_read
  })
  
  
  
}

