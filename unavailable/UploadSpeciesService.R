observeEvent(input$species_file, {
  if (!is.null(input$species_file)) {
    sp_file <- input$species_file
    variables$species_file <- read.csv(sp_file$datapath, header = input$header,
                                    sep = input$sep, quote = input$quote)
    # secondary_variables$original_sp_nrow <- nrow(variables$sp_read)
    # secondary_variables$duplicated_sp <- variables$sp_read[duplicated(na.omit(variables$sp_read)), ]
    variables$sp_read <- unique(variables$species_file)
    status$species_status <- TRUE
  }
})