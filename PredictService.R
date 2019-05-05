observeEvent(input$run_algorithm_btn, {
  predict_variables$can_run_algorithm <- TRUE
  if (!is.null(input$predictors_files) & (predict_variables$can_run_algorithm)) {
    presence_absence_data <- variables$sp_download_db[, 1:3]
    colnames(presence_absence_data) <- c("sp", "long", "lat")
    runAlgorithm(input$predictors_files, presence_absence_data)
  }
})

observeEvent(input$select_input_algorithm, {
  predict_variables$can_run_algorithm <- FALSE
})

observeEvent(input$group_pred_maps_btn, {
  if (input$group_pred_maps_btn) {
    secondary_variables$group_predictive_maps <- TRUE
  }
  else {
    secondary_variables$group_predictive_maps <- FALSE
  }
})

observeEvent(input$training_set, {
  predict_variables$can_run_algorithm <- FALSE
})