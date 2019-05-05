lon_bound <- c()
lat_bound <- c()
for (i in 1:length(unique(grid_read$lon))) {
  subset_df <- subset(grid_read, lon == unique(grid_read$lon)[i])
  subset_df <- subset_df[order(subset_df$lat, decreasing = FALSE), ]
  lon_bound <- c(lon_bound, subset_df[1, ]$lon)
  lon_bound <- c(lon_bound, subset_df[nrow(subset_df), ]$lon)
  lat_bound <- c(lat_bound, subset_df[1, ]$lat)
  lat_bound <- c(lat_bound, subset_df[nrow(subset_df), ]$lat)
}

for (i in 1:length(unique(grid_read$lat))) {
  subset_df <- subset(grid_read, lat == unique(grid_read$lat)[i])
  subset_df <- subset_df[order(subset_df$lon, decreasing = FALSE), ]
  lon_bound <- c(lon_bound, subset_df[1, ]$lon)
  lon_bound <- c(lon_bound, subset_df[nrow(subset_df), ]$lon)
  lat_bound <- c(lat_bound, subset_df[1, ]$lat)
  lat_bound <- c(lat_bound, subset_df[nrow(subset_df), ]$lat)
}

boundary_coord <- data.frame(lon = lon_bound, lat = lat_bound)
boundary_coord <- unique(boundary_coord)

print(boundary_coord)


subset(sp_read,
                sp_read$lon <= boundary_coord$lon
              & sp_read$lon > boundary_coord$lon
              & sp_read$lat <= boundary_coord$lat
              & sp_read$lat > boundary_coord$lat
       )