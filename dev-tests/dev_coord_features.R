library(leaflet)

# sudo apt-get install libprotobuf-dev
library(protolite) # install.packages('protolite')

# sudo add-apt-repository -y ppa:opencpu/jq
# sudo apt-get update
# sudo apt-get install libjq-dev
library(jqr) # install.packages('jqr')

# sudo apt-get install libv8-dev
# install.packages('V8')
library(V8)


library(geojsonio) # install.packages('geojsonio')

grid_read <- read.csv("grid.csv", header = TRUE, sep = ",")
sp_read <- read.csv("sp.csv", header = TRUE, sep = ",")
count <- length(unique(sp_read$sp))
uni <- unique(sp_read$sp)
pal <- colorFactor(c("navy", "red"), domain = c("ship", "pirate"))
leaflet() %>%
  addProviderTiles("Esri.OceanBasemap", group = "Esri.OceanBasemap") %>%
  addProviderTiles("CartoDB.DarkMatter", group = "DarkMatter (CartoDB)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetmap") %>%
  addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
  addLayersControl(baseGroups = c("OpenStreetmap","Esri.OceanBasemap",'DarkMatter (CartoDB)', 'Esri.WorldImagery'),
                   options = layersControlOptions(collapsed = TRUE, autoZIndex = F)) %>%
  # addTiles(
  #   urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
  #   attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
  # ) %>%
  setView(lng = -60.85, lat = -15.45, zoom = 3) %>%
  # addMarkers(lon, lat) %>%
  addCircleMarkers(
    lng = grid_read$lon,
    lat = grid_read$lat,
    radius = 5,
    color = "red",
    stroke = FALSE, fillOpacity = 0.5
  ) %>%
    addCircleMarkers(
      lng = sp_read$lon,
      lat = sp_read$lat,
      popup = sp_read$sp,
      radius = 7,
      color = ifelse(sp_read$sp == "teste", "blue", "yellow"),
      stroke = FALSE, fillOpacity = 0.3
    )







# From http://leafletjs.com/examples/choropleth/us-states.js

# url <- "http://leafletjs.com/examples/choropleth/us-states.js"
# doc <- readLines(url)
# doc2 <- gsub("var statesData = ", "", doc)
# write(doc2, file = "tempgeo.json")
states <- geojsonio::geojson_read("https://raw.githubusercontent.com/codeforamerica/click_that_hood/master/public/data/brazil-states.geojson", what = "sp")

# bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
# pal <- colorBin("YlOrRd", domain = states$density, bins = bins)

# m <- leaflet(states) %>%
#   setView(-60.85, -15.45, 3) %>%
#   addProviderTiles("MapBox", options = providerTileOptions(
#     id = "mapbox.light",
#     accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

m <- leaflet() %>% #addTiles() %>% 
  setView(-60.85, -15.45, 3) %>%
  addProviderTiles("Esri.OceanBasemap", group = "Esri.OceanBasemap") %>%
  addProviderTiles("CartoDB.DarkMatter", group = "DarkMatter (CartoDB)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetmap") %>%
  addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
  addLayersControl(baseGroups = c("OpenStreetmap","Esri.OceanBasemap",'DarkMatter (CartoDB)', 'Esri.WorldImagery'),
                   options = layersControlOptions(collapsed = TRUE, autoZIndex = F))

m

# observe({
  
# print('update map size/opa/color')
x <- grid_read$lon
y <- grid_read$lat
leafletProxy('map')%>%
  addCircleMarkers(lng=x,fillColor = pal(),
                   lat=y,
                   stroke = F,
                   layerId = as.character(1:length(x)),
                   radius = input$size/10,
                   fillOpacity = 1
  )
  
# })



# m %>% addPolygons()
m %>% addPolygons(
  # fillColor = ~pal(density),
  weight = 2,
  opacity = 1,
  color = "#607D8B",
  dashArray = "3",
  fillOpacity = 0) %>%
  # highlight = highlightOptions(
  # weight = 5,
  # color = "black",
  # dashArray = "1",
  # fillOpacity = 0.2,
  # bringToFront = TRUE) %>%
  # label = states$name,
  # labelOptions = labelOptions(
  #   style = list("font-weight" = "normal", padding = "3px 8px"),
  #   textsize = "15px",
  #   direction = "auto"))
  addCircleMarkers(
  lng = grid_read$lon,
  lat = grid_read$lat,
  radius = 10,
  color = "red",
  stroke = FALSE, fillOpacity = 0.1
  ) %>%
  addCircleMarkers(
    lng = sp_read$lon,
    lat = sp_read$lat,
    popup = sp_read$sp,
    radius = 5,
    color = ifelse(sp_read$sp == "teste", "blue", "yellow"),
    stroke = FALSE, 
    fillOpacity = 0.4
  )


# =========================================================


library(plotly)

species <- read.csv('Registros_spp_Modelagem_csv.csv')
bacias <- read.csv('Ids_coord_bacias_csv.csv')
# bacias <- bacias[,4:5]

summary(bacias)

min(bacias$lat)

max(bacias$lat)
max(bacias$lon)


species <- subset(species, species$lon >= min(bacias$lon) 
                      & species$lon <= max(bacias$lon)
                      & species$lat >= min(bacias$lat) 
                      & species$lat <= max(bacias$lat))


plot_ly(data = species_subset, x = ~lat, y = ~lon, color = ~sp)
# outliers <- boxplot.stats(species$lat)$out

ggplot(data = species_subset, aes(x = lat, y = lon))+
  geom_jitter()


grid_read <- bacias
sp_read <- species
sp_freq <- count(sp_read, sp_read$sp)

d = 0.5
r = d/2

result_all <- c()
count <- 1
for (sp_index in 1:nrow(sp_read)) {
  result <- c()
  for (grid_index in 1:nrow(grid_read)) {
    result <- c(result, ifelse(sp_read[sp_index,2:3]$lon <= grid_read[grid_index,]$lon + r &
                                 sp_read[sp_index,2:3]$lon >= grid_read[grid_index,]$lon - r &
                                 sp_read[sp_index,2:3]$lat <= grid_read[grid_index,]$lat + r &
                                 sp_read[sp_index,2:3]$lat >= grid_read[grid_index,]$lat - r, 1, 0))
  }
  print(count)
  count <- count + 1
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

df_res_sum_per_sp_bin

# =====================================================================


results <- read.csv('/home/gustavo/Desenvolvimento/Coord/dev-tests/results.csv')
grid <- read.csv('/home/gustavo/Desenvolvimento/Coord/Testing/grid.csv')
sp <- read.csv('/home/gustavo/Desenvolvimento/Coord/resources/sp.csv')
min(results[nrow(results), 2:ncol(results)])

grid["-", ] <- "-"
results$lon <- grid$lon
results$lat <- grid$lat

spp <- names(results)
spp[2:4]
spp[2:(length(spp)-3)]

# =====================================================================
results

# todas as espécies com occ (com total)
sp_occ_total <- results[, results[nrow(results), ] != 0]

# todas as espécies com occ (sem total)
sp_occ <- sp_occ_total[1:nrow(sp_occ_total)-1, 1:ncol(sp_occ_total)]

sp_occ

# names <- names(sp_occ)
# names <- names[2:(length(names)-3)]

sp_occ$X <- NULL

sp_occ <- subset(sp_occ, TOTAL > 0)
sp_occ$TOTAL <- NULL

# sp_occ$teste <- NULL

# sp_occ <- subset(sp_occ, sp_occ$Bradypus.variegatus == 1)

# sp_occ[sp_occ$Bradypus.variegatus == 1, ]$Bradypus.variegatus <- 'Bradypus.variegatus'

# sp_occ[sp_occ$teste == 1, ]$teste <- 'teste'

# sp_occ

sp_occ <- unique(sp_occ)

a <- c()
for (i in 1:nrow(sp_occ)) {
  a <- c(a, i)
}

sp_occ$sp <-a 

sp_occ

sp_occ[sp_occ$Bradypus.variegatus == 1, ]$sp <- 'Bradypus.variegatus'

sp_occ[sp_occ$teste == 1, ]$sp <- 'teste'

results_sp_names <- names(sp_occ)[1:(ncol(sp_occ)-3)]
for (i in 1:length(results_sp_names)) {
  sp_occ[sp_occ[ , i] == 1, ]$sp <- results_sp_names[i]
}


sp_occ

# sp_names <- subset(sp, sp %in% names)

# sp_occ$Bradypus.variegatus <- NULL

# sp_occ$sp <- c('Bradypus.variegatus')

plot_ly(data = sp_occ, x = ~lat, y = ~lon, color = ~sp, type = 'scatter')



# -----------------------
# OCC PER COORDINATES TO IMPLEMENT:
# MAP OF SP OCC, SP OCC SCATTER PLOT, MULTIPLE SELECT FILTER, COORDINATES WITHOUT OCC,
# SPECIES WITHOUT OCCURENCE, COORDINATES WITH > 1 OCC AND WHICH SPECIES, ...  

subset(results, results$Bradypus.variegatus == 1)
df <- subset(results, results$teste == 1)
df$X <- NULL
df$TOTAL <- NULL
df$teste2 <- NULL
df$Bradypus.variegatus <- NULL

df
# df_t <- t(df)

plot_ly(data = df, x = ~lat, y = ~lon, color = ~sp, type = 'scatter')

# ================================================================
# GRID BOUNDARY TO DETECT PRETTY GOOD OUTLIERS
grid_brasil <- read.csv('/home/gustavo/Desenvolvimento/Coord/dev-tests/Ids_coord_bacias_csv.csv')

x <- max(grid_brasil$lat) - min(grid_brasil$lat)
y <- max(grid_brasil$lon) - min(grid_brasil$lon)

library(raster)
install.packages('qlcVisualize')
library(qlcVisualize)

# boundary(grid_brasil, density = 0.02, grid = 20, box.offset = 0.1, tightness = 7.5, plot = TRUE)
# b <- boundary(grid_brasil, density = 0.02, grid = 10, box.offset = 0.1, tightness = 7.5, plot = FALSE)
# df_b <- data.frame(b)

library(leaflet)

# procedures_sub[order(procedures_sub$CODIGO_PROCEDIMENTO, decreasing = FALSE), ] 

# ordena grid por longitude

# boundary_coord$lon = lon_bound
# boundary_coord$lat = lat_bound

# insere lista de longitude mais baixa
# boundary_coord <- append(boundary_coord, list_l[[1]])

#insere lista de longitude mais alta
# boundary_coord <- append(boundary_coord, list_l[[length(list_l)]])





# pal <- colorNumeric(c("red", "green", "blue"), 1:10)
# pal(c(1,6,9))

# values <- unique(sp_selected$sp)
colors <- colorRampPalette(palette()[2:length(palette())-1])(length(unique(sp_selected$sp)))
pal <- colorFactor(colors, domain = unique(sp_selected$sp))

## Use n equally spaced breaks to assign each value to n-1 equal sized bins 
# ii <- cut(values, breaks = seq(min(values), max(values), len = len(values)),
          # include.lowest = TRUE)
## Use bin indices, ii, to select color from vector of n-1 equally spaced colors





# ================================
library(dplyr)

sp_a_lot <- read.csv("/home/gustavo/Desenvolvimento/Coord/resources/Registros_spp_Modelagem_csv.csv")

Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoiZ210b2ZvbGkiLCJhIjoiY2ptaDJrbnhqN2Q3MTN3b2dsbDIzcnR4YyJ9.dbW79tYM5wW8z7Po5nx7XA')
p <- sp_a_lot %>%
  plot_mapbox(lat = ~lat, lon = ~lon, 
              split = ~sp,
              mode = 'scattermapbox', hoverinfo='all') %>%
  layout(title = 'Occurences', 
         font = list(color='black'), 
         # plot_bgcolor = '#191A1A', 
         # paper_bgc_color = '#191A1A',
         mapbox = list(style = 'dark'), 
         legend = list(orientation = 'v',
                      font = list(size = 8
         )),
        margin = list(l = 25, r = 25, 
                      b = 25, t = 25, 
                      pad = 2)
        )

# chart_link = api_create(p, filename="mapbox-multiple")
p


# =============================================================================

# DETECT BOUNDARY COORDINATES
grid_brasil <- grid_brasil[order(grid_brasil$lon, decreasing = FALSE), ]

lon_bound <- c()
lat_bound <- c()
for (i in 1:length(unique(grid_brasil$lon))) {
  subset_df <- subset(grid_brasil, lon == unique(grid_brasil$lon)[i])
  subset_df <- subset_df[order(subset_df$lat, decreasing = FALSE), ]
  lon_bound <- c(lon_bound, subset_df[1, ]$lon)
  lon_bound <- c(lon_bound, subset_df[nrow(subset_df), ]$lon)
  lat_bound <- c(lat_bound, subset_df[1, ]$lat)
  lat_bound <- c(lat_bound, subset_df[nrow(subset_df), ]$lat)
}

for (i in 1:length(unique(grid_brasil$lat))) {
  subset_df <- subset(grid_brasil, lat == unique(grid_brasil$lat)[i])
  subset_df <- subset_df[order(subset_df$lon, decreasing = FALSE), ]
  lon_bound <- c(lon_bound, subset_df[1, ]$lon)
  lon_bound <- c(lon_bound, subset_df[nrow(subset_df), ]$lon)
  lat_bound <- c(lat_bound, subset_df[1, ]$lat)
  lat_bound <- c(lat_bound, subset_df[nrow(subset_df), ]$lat)
}

boundary_coord <- data.frame(lon = lon_bound, lat = lat_bound)
boundary_coord <- unique(boundary_coord)


leaflet() %>%
  addProviderTiles("Esri.OceanBasemap", group = "Esri.OceanBasemap") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetmap") %>%
  addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
  addLayersControl(baseGroups = c("OpenStreetmap","Esri.OceanBasemap", 'Esri.WorldImagery'),
                   options = layersControlOptions(collapsed = TRUE, autoZIndex = F)) %>%
  setView(lng = -60.85, lat = -15.45, zoom = 3) %>%
  addTiles() %>%
  addCircleMarkers(
    data = boundary_coord,
    lng = as.numeric(boundary_coord$lon),
    lat = as.numeric(boundary_coord$lat),
    label = paste("lon:", boundary_coord$lon, ", lat:", boundary_coord$lat),
    radius = 7,
    color = "blue",
    # group = 
    # fillColor = ~pal,
    stroke = FALSE, fillOpacity = 0.3
  )
