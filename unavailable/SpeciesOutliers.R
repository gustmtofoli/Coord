get_species_outliers = function() {
  if (!is.null(input$file2) & !is.null(input$file1)) {
    sp_read <- variables$sp_read
    grid_read <- variables$grid_read
    subset(sp_read, sp_read$lon < min(grid_read$lon) 
           | sp_read$lon > max(grid_read$lon)
           | sp_read$lat < min(grid_read$lat) 
           | sp_read$lat > max(grid_read$lat))
  }
}

get_species_outliers_freq = function() {
  sp_out <- get_species_outliers()
  df_freq <- count(sp_out, sp_out$sp)
  data.frame(Specie = df_freq$`sp_out$sp`, Freq = df_freq$n)
}

remove_species_outliers = function(grid_read, sp_read) {
  subset(sp_read, sp_read$lon >= min(grid_read$lon)
         & sp_read$lon <= max(grid_read$lon)
         & sp_read$lat >= min(grid_read$lat)
         & sp_read$lat <= max(grid_read$lat))
}