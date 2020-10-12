output$download_species_freq <- downloadHandler(
  filename = function(){"species_freq.csv"},
  content = function(fname){
    if (!is.null(input$file2) & !is.null(input$file1)) {
      write.csv(get_species_freq(), fname)
    }
  }
)

output$download_species_outliers <- downloadHandler(
  filename = function(){"species_outliers.csv"},
  content = function(fname){
    if (!is.null(input$file2) & !is.null(input$file1)) {
      write.csv(get_species_outliers(), fname)
    }
  }
)

output$download_species_outliers_freq <- downloadHandler(
  filename = function(){"species_outliers_freq.csv"},
  content = function(fname){
    if (!is.null(input$file2) & !is.null(input$file1)) {
      write.csv(get_species_outliers_freq(), fname)
    }
  }
)

output$species <- DT::renderDataTable({
  get_species_freq()
})

output$species_outliers <- DT::renderDataTable(({
  get_species_outliers()
}))

output$species_outliers_freq <- DT::renderDataTable(({
  if (!is.null(input$file2) & !is.null(input$file1)) {
    grid_read <- variables$grid_read
    sp_read <- variables$sp_read
    sp_subset <- subset(sp_read, sp_read$lon < min(grid_read$lon) 
                        | sp_read$lon > max(grid_read$lon)
                        | sp_read$lat < min(grid_read$lat) 
                        | sp_read$lat > max(grid_read$lat))
    df_freq <- count(sp_subset, sp_subset$sp)
    data.frame(Specie = df_freq$`sp_subset$sp`, Freq = df_freq$n)
  }
}))

output$duplicated_sp <- DT::renderDataTable({
  secondary_variables$duplicated_sp
})

output$download_duplicated_sp <- downloadHandler(
  filename = function(){"duplicated_records_species.csv"},
  content = function(fname){
    if (!is.null(input$file2) & !is.null(input$file1)) {
      write.csv(secondary_variables$duplicated_sp, fname)
    }
  }
)

output$duplicated_grid <- DT::renderDataTable({
  secondary_variables$duplicated_grid
})

output$download_duplicated_grid <- downloadHandler(
  filename = function(){"duplicated_records_grid.csv"},
  content = function(fname){
    if (!is.null(input$file2) & !is.null(input$file1)) {
      write.csv(secondary_variables$duplicated_grid, fname)
    }
  }
)

output$sp_duplicated_percent <- renderInfoBox({
  duplicated_percent <- (nrow(secondary_variables$duplicated_sp) / secondary_variables$original_sp_nrow)*100
  infoBox(
    "Sp - Duplicated ocurrences", 
    paste0(round(duplicated_percent, 2), "%"),
    paste0(nrow(secondary_variables$duplicated_sp), " of ", secondary_variables$original_sp_nrow, " occ."),
    icon = icon("list"),
    color = "light-blue", 
    fill = TRUE
  )
})

output$grid_duplicated_percent <- renderInfoBox({
  duplicated_percent <- (nrow(secondary_variables$duplicated_grid) / nrow(variables$grid_read))*100
  infoBox(
    "Grid - Duplicated occurences", 
    paste0(round(duplicated_percent, 2), "%"),
    paste0(nrow(secondary_variables$duplicated_grid), " of ", nrow(variables$grid_read), " occ."),
    icon = icon("list"),
    color = "light-blue", 
    fill = TRUE
  )
})

output$sp_outliers_percent <- renderInfoBox({
  outliers <- get_species_outliers()
  outliers_percent <- (nrow(outliers) / secondary_variables$original_sp_nrow)*100
  infoBox(
    "Sp - Outliers", 
    paste0(round(outliers_percent, 2), "%"),
    paste0(nrow(outliers), " of ", secondary_variables$original_sp_nrow, " occ."),
    icon = icon("list"),
    color = "light-blue", 
    fill = TRUE
  )
})

output$sp_total_percent <- renderInfoBox({
  total_percent <- (nrow(variables$sp_read) / secondary_variables$original_sp_nrow)*100
  infoBox(
    "Used occurrences",
    paste0("Used: ", round(total_percent, 2), "%", " (",nrow(variables$sp_read), " occ.)"),
    paste0("Removed: ", 100 - round(total_percent, 2), "%", " (", secondary_variables$original_sp_nrow - nrow(variables$sp_read), " occ.)"),
    icon = icon("list"),
    color = "light-blue", 
    fill = TRUE
  )
})

output$number_of_species <- renderInfoBox({
  number_of_species <- length(unique(variables$sp_read$sp))
  infoBox(
    "Number of species", 
    paste0(number_of_species),
    icon = icon("list"),
    color = "light-blue", 
    fill = TRUE
  )
})