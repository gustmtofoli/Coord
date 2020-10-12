get_species_freq = function() {
  if (!is.null(input$file2)) {
    sp_read <- variables$sp_read
    df_freq <- count(sp_read, sp_read$sp)
    data.frame(Specie = df_freq$`sp_read$sp`, Freq = df_freq$n)
  }
}