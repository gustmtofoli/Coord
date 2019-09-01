observeEvent(input$download_from_DB, {
  if (length(input$select_data_bases) > 0) {
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
  
              
        
  
        # paste0("Species: ", species_download),
        # br(),
      # paste0("Data base(s): ", input$select_data_bases)

    ))
    
    # value <- reactiveVal(0)
    # 
    # withProgress(message = 'Calculation in progress', value = 0,detail="0%", {
    #   # run calculation
    #   for (i in 1:10) {
    #     Sys.sleep(0.5)
    #     newValue <- value() + 1
    #     value(newValue)
    #     incProgress(1/10,detail = paste0(i*10,"%"))
    #   }
    #   Sys.sleep(0.5)
    # })
    
    print("\n\n========================")
    print(input$select_data_bases)
    print(species_download)
    
    
    data_from_DB <- occ(species_download, from = input$select_data_bases)
    df_data <- occ2df(data_from_DB)
    colnames(df_data) <- c("sp", "lon", "lat", "data_base")
    print(df_data)
    if (!is.null(df_data) & nrow(df_data) > 0) {
      variables$sp_download_db <- df_data[, 1:4]
      status$species_status <- TRUE
      showModal(modalDialog(
        title = "Nice work!!",
        footer = NULL,
        easyClose = TRUE
      ))
    }
    else {
      showModal(modalDialog(
        title = "Oh no :(",
        footer = NULL,
        easyClose = TRUE,
        paste0("No records found in ", input$select_data_bases, " for ", input$sp_name)
      ))
    }
  }
})








observeEvent(input$download_from_DB, {
  {
    # This is certainly wrong:
    output$console_text <- renderPrint(result <- myfunction(20))
  }
})

observeEvent(input$see_db_with_records, {
  downloaded_species <- variables$sp_download_db
  data_bases = unique(downloaded_species$data_base)
  df <- data.frame(data_bases = data_bases)
  print(unique(downloaded_species$data_base))
  print(df)
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
  print(unique(downloaded_species$data_base))
  print(df)
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
    uploaded_data <- read.csv(uploaded_file$datapath, header = TRUE,
                              sep = ",")
    species_name = unique(uploaded_data$sp)
    updateSelectInput(session, "selec_filter_download_sp", label = h4("Select species"), 
                      choices = species_name, 
                      selected = species_name)
  }
})

observeEvent(input$clean_download_sp_btn, {
  if (!is.null(input$file_species_download)) {
    uploaded_file <- input$file_species_download
    uploaded_data <- read.csv(uploaded_file$datapath, header = TRUE,
                              sep = ",")
    species_name = unique(uploaded_data$sp)
    updateSelectInput(session, "selec_filter_download_sp", label = h4("Select species"), 
                      choices = species_name, 
                      selected = 1)
  }
})

clean_all_filter <- observeEvent(input$clean_all_filter_btn, {
  sp_download_db <- variables$sp_download_db
  species_name = unique(sp_download_db$sp)
  updateSelectInput(session, "selec_filter_sp_map", label = h4("Select species"), 
                    choices = species_name, 
                    selected = 1)
})
