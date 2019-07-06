output$species_file_table <- DT::renderDataTable({
  if (!is.null(input$species_file)) {
    variables$species_file
  }
})