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
    secondary_variables$duplicated_sp <- variables$sp_read[duplicated(na.omit(variables$sp_read)), ]
    variables$sp_read <- unique(variables$sp_read)
    status$species_status <- TRUE
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

output$download_results <- downloadHandler(
  filename = function(){"results.csv"},
  content = function(fname){
    if (!is.null(input$file2) & !is.null(input$file1)) {
      write.csv(variables$results, fname)
    }
  }
)