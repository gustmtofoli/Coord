output$show_predict_map <- renderPlot({
  group_maps <- secondary_variables$group_predictive_maps
  if (!is.null(input$predictors_files) & (predict_variables$can_run_algorithm) & !is.null(predict_variables$predictive_map)) {
    predictive_map <- predict_variables$predictive_map
    models_projected_names <- predictive_map@models.projected
    projections <- stack(predictive_map@proj@link)

    if (group_maps) {
      plot(predictive_map@proj@val)
    }
    else {
      plot(predictive_map@proj@val[[input$select_input_predictive_maps]])
    }
  }
})

output$select_predictive_maps <- renderUI(
  if (!is.null(predict_variables$predictive_map)) {
    predictive_map <- predict_variables$predictive_map
    models_projected_names <- predictive_map@models.projected
    selectInput("select_input_predictive_maps", label = "Choose predictive map: ",
                choices = models_projected_names,
                selected = 1, multiple = FALSE)
  }
)

output$show_ensemble_map <- renderPlot({
  if ((predict_variables$can_run_algorithm) & !is.null(predict_variables$ensemble_map)) {
    ensemble_map <- predict_variables$ensemble_map
    plot(ensemble_map@proj@val)
  }
})

output$select_algorithm <- renderUI({
  selectInput("select_input_algorithm", label = "Choose Algorithms: ",
              choices = predict_variables$algorithms,
              selected = 1, multiple = TRUE)
})

output$select_eval_method <- renderUI({
  selectInput("select_input_eval_method", label = "Choose evaluation method(s): ",
              choices = c("ROC", "TSS", "KAPPA"),
              selected = 1, multiple = TRUE)
})

output$select_eval_method_ensemble <- renderUI({
  selectInput("select_input_eval_method_ensemble", label = "Choose evaluation method(s): ",
              choices = c("ROC", "TSS", "KAPPA"),
              selected = 1, multiple = TRUE)
})

output$info_training_testing <- DT::renderDataTable({
  if (!is.null(input$predictors_files) & (predict_variables$can_run_algorithm)) {
    df_info <- data.frame(info = c('Execution time: ', 
                                   # 'Algorithm: ', 
                                   'Training set: ', 
                                   'Test set: '
                                   # 'AUC: '
    ), 
    value = c(as.character(predict_variables$execution_time), 
              paste0(input$training_set, "%"), 
              paste0(100 - as.numeric(input$training_set), "%")
    ))
    df_info
  }
})

output$info_eval_AUC <- DT::renderDataTable({
  print(input$select_eval_method)
  if (!is.null(predict_variables$predictive_model)) {
    models <- predict_variables$predictive_model
    evaluations <- get_evaluations(models)
    df_eval <- data.frame(evaluations)
    ini_col <- 1
    df_eval_auc <- data.frame(df_eval['ROC', ini_col:(ini_col+3)])
    rownames(df_eval_auc) <-c(models@models.computed[1])
    ini_col <- ini_col + 3
    for (i in 2:length(models@models.computed)) {
      ini_col <- ini_col + 1
      df_eval_auc[models@models.computed[i], ] <- df_eval['ROC', ini_col:(ini_col+3)]
      ini_col <- ini_col + 3
    }
    
    if (!is.null(predict_variables$ensemble_model)) {
      ini_col <- 1
      ensemble_model <- predict_variables$ensemble_model
      ensemble_evaluations <- get_evaluations(ensemble_model)
      df_eval_ensemble <- data.frame(ensemble_evaluations)
      for (j in 1:length(ensemble_model@em.computed)) {
        df_eval_auc[ensemble_model@em.computed[j], ] <- df_eval_ensemble['ROC', ini_col:(ini_col+3)]
        ini_col <- ini_col + 4
      }
    }
    
    colnames(df_eval_auc) <- c("testing data", "cutoff", "sensitivity", "Specificity")
    df_eval_auc
  }
})

output$info_eval_TSS <- DT::renderDataTable({
  if (!is.null(predict_variables$predictive_model)) {
    models <- predict_variables$predictive_model
    evaluations <- get_evaluations(models)
    df_eval <- data.frame(evaluations)
    ini_col <- 1
    df_eval_tss <- data.frame(df_eval['TSS', ini_col:(ini_col+3)])
    rownames(df_eval_tss) <-c(models@models.computed[1])
    ini_col <- ini_col + 3
    for (i in 2:length(models@models.computed)) {
      ini_col <- ini_col + 1
      df_eval_tss[models@models.computed[i], ] <- df_eval['TSS', ini_col:(ini_col+3)]
      ini_col <- ini_col + 3
    }
    
    if (!is.null(predict_variables$ensemble_model)) {
      ensemble_model <- predict_variables$ensemble_model
      ensemble_evaluations <- get_evaluations(ensemble_model)
      df_eval_ensemble <- data.frame(ensemble_evaluations)
      ini_col <- 1
      for (j in 1:length(ensemble_model@em.computed)) {
        df_eval_tss[ensemble_model@em.computed[j], ] <- df_eval_ensemble['TSS', ini_col:(ini_col+3)]
        ini_col <- ini_col + 4
      }
    }
    colnames(df_eval_tss) <- c("testing data", "cutoff", "sensitivity", "specificity")
    df_eval_tss
  }
})

output$species_infobox <- renderInfoBox({
  infoBox(
    "Species", 
    ifelse(status$species_status, "Loaded", "Not loaded"),
    icon = icon("list"),
    color = ifelse(status$species_status, "green", "red"), 
    fill = TRUE
  )
})

output$predictors_infobox <- renderInfoBox({
  infoBox(
    "Predictors", 
    ifelse(status$predictors_status, "Loaded", "Not loaded"),
    icon = icon("list"),
    color = ifelse(status$predictors_status, "green", "red"), 
    fill = TRUE
  )
})

output$download_distribution_map <- downloadHandler(
  filename <- function() {
    paste(str(input$select_input_predictive_maps), ".tif")
  },
  content = function(file) {
    predictive_map <- predict_variables$predictive_map
    writeRaster(predictive_map@proj@val[[input$select_input_predictive_maps]], filename=file, format="GTiff", overwrite=TRUE)
  }
)

output$download_ensemble_map <- downloadHandler(
  
  filename <- function() {
    paste('ensemble_map', ".tif")
  },
  content = function(file) {
    predictive_map <- predict_variables$ensemble_map
    writeRaster(predictive_map@proj@val[[predictive_map@models.projected[1]]], filename=file, format="GTiff", overwrite=TRUE)
  }
)

output$download_ensemble_weighted_map <- downloadHandler(
  filename <- function() {
    paste('ensemble_weighted_map', ".tif")
  },
  content = function(file) {
    predictive_map <- predict_variables$ensemble_map
    print(predictive_map@models.projected)
    writeRaster(predictive_map@proj@val[[predictive_map@models.projected[2]]], filename=file, format="GTiff", overwrite=TRUE)
  }
)
