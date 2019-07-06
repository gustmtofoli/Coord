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
                              species_file = NULL,
                              sp_download_db = NULL,
                              sp_download_db_processed = NULL,
                              sp_without_outliers = NULL, 
                              results = NULL)
  
  status <- reactiveValues(species_status = FALSE,
                           predictors_status = FALSE
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
  
  source("UploadSpeciesService.R", local=TRUE)
  source("PredictService.R", local=TRUE)
  source("PresenceAbsenceService.R", local=TRUE)
  source("SpeciesDataFromDataBasesService.R", local=TRUE)
  source("PredictorsFromUploadService.R", local=TRUE)
  source("PredictorsFromUploadService.R", local=TRUE)
  
  # ===================================================================================
  
  # ========== OUTPUTS ===============================================================
  
  source("UploadSpeciesOutputs.R", local=TRUE)
  source("PredictOutputs.R", local=TRUE)
  source("PresenceAbsenceOutputs.R", local=TRUE)
  source("SpeciesDataFromDataBasesOutputs.R", local=TRUE)
  source("EDASpatialDistributionOutputs.R", local=TRUE)
  source("EDASummaryOutputs.R", local=TRUE)
  source("PredictorsFromUploadOutputs.R", local=TRUE)
  
  # ===================================================================================
}

