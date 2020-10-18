output$download_data_from_db <- renderUI(
    if (!is.null(variables$sp_download_db)) {
        downloadButton("download_data_from_db_btn", "Download")
    }
)

output$download_data_from_db_btn <- downloadHandler(
  filename = function() { "downloaded_species_occ.csv" },
  content = function(fname) {
    if (!is.null(variables$sp_download_db)) {
      write.csv(unique(na.omit(variables$sp_download_db)), fname)
    }
  }
)

output$show_distribution_map <- renderUI(
    if (!is.null(variables$sp_download_db)) {
        actionButton("show_distribution_map_btn", "Distribution map", icon = icon("map-marked-alt"))
    }
)

output$download_sample_sp <- downloadHandler(
  filename <- function() {
    paste("sample_download_species", "csv", sep=".")
  },
  content <- function(file) {
    file.copy("./sample_download_species.csv", file)
  },
  contentType = "application/csv"
)

output$filter_sp_download <- renderUI({
    if (!is.null(input$file_species_download)) {
        file_species_download <- input$file_species_download
        species_download <- grid_read <- read.csv(file_species_download$datapath, header = TRUE, sep = ",")
        species_name <- unique(species_download$sp)
        selectInput("selec_filter_download_sp", label = "Select species:", choices = species_name, selected = 1, multiple = TRUE)
    }
})

output$sp_download_na <- renderUI({
    downloaded_species <- variables$sp_download_db
    total_nrow <- nrow(downloaded_species)
    number_of_na <- abs(nrow(na.omit(downloaded_species)) - total_nrow)
    
    if (!is.null(downloaded_species)) {
        infoBox(
            "Number of empty records",
            paste0(round(((100*number_of_na)/total_nrow), 2), "%"),
            paste0(number_of_na, " of ", nrow(downloaded_species)),
            icon = icon("list"),
            color = "light-blue", 
            fill = TRUE
        )
    }
})

output$sp_download_count <- renderUI({
    downloaded_species <- variables$sp_download_db
    number_of_species <- length(unique(downloaded_species$sp))
    if (!is.null(downloaded_species)) {
        infoBox(
            "Number of Species",
            paste0(number_of_species),
            icon = icon("list"),
            color = "light-blue",
            fill = TRUE,
            actionButton("see_downloaded_species", "Detail")
        )
    }
})

output$sp_download_duplicated <- renderUI({
    downloaded_species <- variables$sp_download_db
    downloaded_species_without_na <- na.omit(downloaded_species)
    n_duplicated_rows <- nrow(downloaded_species_without_na[duplicated(downloaded_species_without_na), ])
    
    if (!is.null(downloaded_species)) {
        infoBox(
            "Duplicated occurences",
            paste0(round(((100*n_duplicated_rows)/nrow(downloaded_species)), 2), "%"),
            paste0(n_duplicated_rows, " of ", nrow(downloaded_species)),
            icon = icon("list"),
            color = "light-blue", 
            fill = TRUE
        )
    }
})

output$sp_download_db <- renderUI({
    downloaded_species <- variables$sp_download_db
    n_data_bases <- length(unique(downloaded_species$data_base))
    
    if (!is.null(downloaded_species)) {
        infoBox(
            "Databases with records",
            paste0(n_data_bases),
            icon = icon("list"),
            color = "light-blue",
            fill = TRUE,
            actionButton("see_db_with_records", "Detail")
        )
    }
})

output$select_DB <- renderUI({
  selectInput("select_data_bases", label = "Select database: ",
              choices = predict_variables$data_bases,
              selected = 1, multiple = TRUE)
})

output$show_downloaded_data <- DT::renderDataTable({
  unique(na.omit(variables$sp_download_db))
})