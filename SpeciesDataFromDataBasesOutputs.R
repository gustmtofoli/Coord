output$download_data_from_db_btn <- downloadHandler(
  filename = function() { "downloaded_species_occ.csv" },
  content = function(fname) {
    if (!is.null(variables$sp_download_db)) {
      write.csv(unique(na.omit(variables$sp_download_db)), fname)
    }
  }
)

output$filter_sp_download <- renderUI({
  if (!is.null(input$file_species_download)) {
    file_species_download <- input$file_species_download
    species_download <- grid_read <- read.csv(file_species_download$datapath, header = input$header,
                                              sep = input$sep, quote = input$quote)
    species_name <- unique(species_download$sp)
    selectInput("selec_filter_download_sp", label = h5("Select species"),
                choices = species_name,
                selected = 1, multiple = TRUE)
  }
})

output$sp_download_na <- renderInfoBox({
  downloaded_species <- variables$sp_download_db
  total_nrow <- nrow(downloaded_species)
  number_of_na <- abs(nrow(na.omit(downloaded_species)) - total_nrow)
  infoBox(
    "Number of empty records",
    paste0(round(((100*number_of_na)/total_nrow), 2), "%"),
    paste0(number_of_na, " of ", nrow(downloaded_species)),
    icon = icon("list"),
    color = "light-blue", 
    fill = TRUE
  )
})

output$sp_download_count <- renderInfoBox({
  downloaded_species <- variables$sp_download_db
  number_of_species <- length(unique(downloaded_species$sp))
  infoBox(
    "Number of Species",
    paste0(number_of_species),
    icon = icon("list"),
    color = "light-blue", 
    fill = TRUE
  )
})

output$sp_download_duplicated <- renderInfoBox({
  downloaded_species <- variables$sp_download_db
  downloaded_species.na.omit <- na.omit(downloaded_species)
  n_duplicated_rows <- nrow(downloaded_species[duplicated(downloaded_species.na.omit), ])
  infoBox(
    "Duplicated occurences",
    paste0(round(((100*n_duplicated_rows)/nrow(downloaded_species)), 2), "%"),
    paste0(n_duplicated_rows, " of ", nrow(downloaded_species)),
    icon = icon("list"),
    color = "light-blue", 
    fill = TRUE
  )
})

output$sp_download_db <- renderInfoBox({
  downloaded_species <- variables$sp_download_db
  n_data_bases <- length(unique(downloaded_species$data_base))
  infoBox(
    "Data Bases with records",
    paste0(n_data_bases),
    icon = icon("list"),
    color = "light-blue",
    fill = TRUE
  )
})

output$select_DB <- renderUI({
  selectInput("select_data_bases", label = "Select Data base: ",
              choices = predict_variables$data_bases,
              selected = 1, multiple = TRUE)
})

output$show_downloaded_data <- DT::renderDataTable({
  unique(na.omit(variables$sp_download_db))
})