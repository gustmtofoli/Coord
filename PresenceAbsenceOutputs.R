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

output$result <- DT::renderDataTable({
  if (!is.null(input$file1) & !is.null(input$file2)) {
    if ((nrow(variables$sp_read)*nrow(variables$grid_read) <= 100000)) {
      results <- variables$results
    }
    else {
      showModal(modalDialog(
        title = "Hey",
        easyClose = TRUE,
        footer = NULL,
        "There are too many rows in those data. It would take a lot of time to generate the results with the current hardware."
      ))
      data.frame(Message = c("There are too many rows in those data. It would take a lot of time to generate the results with the current hardware."))
    }
  }
})