require(stringr)

observeEvent(input$download_from_DB, {
    if (is.null(input$sp_name) || str_replace_all(input$sp_name, " ", "") == "") {
        showModal(modalDialog(
            title = "Nope",
            footer = modalButton("OK"),
            size = c("m"),
            easyClose = TRUE,
            "You must fill the species name field"
        ))
    }
    
    if (is.null(input$select_data_bases)) {
        showModal(modalDialog(
            title = "Nope",
            footer = modalButton("OK"),
            size = c("m"),
            easyClose = TRUE,
            "You must select at least one database"
        ))
    }
    
    
    if (length(input$select_data_bases) > 0 & length(input$sp_name) > 0) {
        species_download <- c()

        if (input$upload_file_switch_btn) {
          species_download <- input$selec_filter_download_sp
        }
        else {
          species_download <- input$sp_name
        }

        showModal(modalDialog(
            title = "Searching Data",
            footer = NULL,
            easyClose = FALSE,
            loadingState()
        ))

        data_from_DB <- occ(species_download, from = input$select_data_bases)
        df_data <- occ2df(data_from_DB)
        colnames(df_data) <- c("sp", "lon", "lat", "data_base")
        if (!is.null(df_data) & nrow(df_data) > 0) {
          variables$sp_download_db <- df_data[, 1:4]
          status$species_status <- TRUE
          showModal(modalDialog(
            title = "Nice work!",
            footer = modalButton("OK"),
            easyClose = TRUE
          ))
        }
        else {
            showModal(modalDialog(
                title = "Oh no :(",
                footer = modalButton("OK"),
                easyClose = TRUE,
                paste0("No records found in '", paste(unlist(input$select_data_bases), collapse = ', '), "' for '", input$sp_name, "'")
            ))
        }
    }
})

observeEvent(input$see_db_with_records, {
  downloaded_species <- variables$sp_download_db
  data_bases = unique(downloaded_species$data_base)
  df <- data.frame(data_bases = data_bases)
  showModal(modalDialog(
    title = "Data Bases with records:",
    footer = NULL,
    easyClose = TRUE,
    renderDataTable(df, options = list(lengthChange = FALSE))
  ))
})

observeEvent(input$see_downloaded_species, {
  downloaded_species <- variables$sp_download_db
  species = unique(downloaded_species$sp)
  df <- data.frame(species = species)
  showModal(modalDialog(
    title = "Species:",
    footer = NULL,
    easyClose = TRUE,
    renderDataTable(df, options = list(lengthChange = FALSE))
  ))
})

observeEvent(input$selet_all_download_sp_btn, {
  if (!is.null(input$file_species_download)) {
    uploaded_file <- input$file_species_download
    uploaded_data <- read.csv(uploaded_file$datapath, header = TRUE, sep = ",")
    species_name = unique(uploaded_data$sp)
    updateSelectInput(session, "selec_filter_download_sp", label = "Select species:", 
                      choices = species_name, 
                      selected = species_name)
  }
})

observeEvent(input$clean_download_sp_btn, {
  if (!is.null(input$file_species_download)) {
    updateSelectInput(session, "selec_filter_download_sp", label = "Select species:", 
                      choices = c(),
                      selected = 1)
  }
})

observeEvent(input$clean_all_filter_btn, {
  sp_download_db <- variables$sp_download_db
  species_name = unique(sp_download_db$sp)
  updateSelectInput(session, "selec_filter_sp_map", label = h4("Select species"), 
                    choices = species_name, 
                    selected = 1)
})
