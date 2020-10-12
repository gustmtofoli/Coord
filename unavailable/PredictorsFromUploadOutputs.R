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