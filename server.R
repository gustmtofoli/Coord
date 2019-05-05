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
  
  # ========== FUNCTIONS ===============================================================
  
  source("SpeciesFreq.R", local=TRUE)
  source("SpeciesOutliers.R", local=TRUE)
  source("CalculatePresenceAbsense.R", local=TRUE)
  source("RunAlgorithm.R", local=TRUE)
  
  # ===================================================================================
  
  # ========== SERVICES ===============================================================
  
  source("PredictService.R", local=TRUE)
  source("PresenceAbsenceService.R", local=TRUE)
  source("SpeciesDataFromDataBasesService.R", local=TRUE)
  source("PredictorsFromUploadService.R", local=TRUE)
  source("PredictorsFromUploadService.R", local=TRUE)
  
  # ===================================================================================
  
  # ========== OUTPUTS ===============================================================
  
  source("PredictOutputs.R", local=TRUE)
  source("PresenceAbsenceOutputs.R", local=TRUE)
  source("SpeciesDataFromDataBasesOutputs.R", local=TRUE)
  source("EDASpatialDistributionOutputs.R", local=TRUE)
  source("PredictorsFromUploadOutputs.R", local=TRUE)
  
  # ===================================================================================

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
}

