get_results = function(grid_read, sp_read) {
  sp_read <- variables$sp_without_outliers
  
  sp_freq <- count(sp_read, sp_read$sp)
  
  d = 0.5
  r = d/2
  
  result_all <- c()
  for (sp_index in 1:nrow(sp_read)) {
    result <- c()
    for (grid_index in 1:nrow(grid_read)) {
      result <- c(result, ifelse(sp_read[sp_index,2:3]$lon <= grid_read[grid_index,]$lon + r &
                                   sp_read[sp_index,2:3]$lon >= grid_read[grid_index,]$lon - r &
                                   sp_read[sp_index,2:3]$lat <= grid_read[grid_index,]$lat + r &
                                   sp_read[sp_index,2:3]$lat >= grid_read[grid_index,]$lat - r, 1, 0))
    }
    result_all <- c(result_all, result)
  }
  
  res_test <- array(result_all, dim = c(nrow(grid_read), nrow(sp_read)))
  
  colnames(res_test) <- sp_read$sp
  
  start_col <- 1
  
  res_sum_per_sp <- c()
  for (i in 1:nrow(sp_freq)) {
    res_sum_per_sp <- c(res_sum_per_sp, apply(res_test[, start_col:(start_col + sp_freq$n[i] - 1)], 1, sum))
    
    start_col <- sp_freq$n[i] + 1
  }
  
  res_sum_per_sp <- array(res_sum_per_sp, dim = c(nrow(grid_read), nrow(sp_freq)))
  
  colnames(res_sum_per_sp) <- sp_freq$`sp_read$sp`
  
  res_sum_per_sp_bin <- ifelse(res_sum_per_sp[ , ] > 0, 1, 0)
  
  df_res_sum_per_sp_bin <- data.frame(res_sum_per_sp_bin)
  
  df_res_sum_per_sp_bin$TOTAL <- apply(res_sum_per_sp_bin, 1, sum)
  
  df_res_sum_per_sp_bin["TOTAL", ] <- apply(df_res_sum_per_sp_bin, 2, sum)
  
  grid_read["-", ] <- "-"
  
  df_res_sum_per_sp_bin$lon <- grid_read$lon
  
  df_res_sum_per_sp_bin$lat <- grid_read$lat
  
  df_res_sum_per_sp_bin
  
}