observeEvent(input$predictors_files, {
  predict_variables$can_run_algorithm <- FALSE
  status$predictors_status <- TRUE
})