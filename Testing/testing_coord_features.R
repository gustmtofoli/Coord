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


results <- read.csv('/home/gustavo/Desenvolvimento/Coord/Testing/results.csv')
grid <- read.csv('/home/gustavo/Desenvolvimento/Coord/Testing/grid.csv')
min(results[nrow(results), 2:ncol(results)])

# =====================================================================

# todas as espécies com occ (com total)
sp_occ_total <- results[, results[nrow(results), ] != 0]

# todas as espécies com occ (sem total)
sp_occ <- sp_occ_total[1:nrow(sp_occ_total)-1, 1:ncol(sp_occ_total)-1]

# relaciona as occ com coordenadas da grid
sp_occ$lon <- grid$lon
sp_occ$lat <- grid$lat
sp_occ

sp_occ <- sp_occ[,2:ncol(sp_occ)]

# -----------------------
# OCC PER COORDINATES TO IMPLEMENT:
# MAP OF SP OCC, SP OCC SCATTER PLOT, MULTIPLE SELECT FILTER, COORDINATES WITHOUT OCC,
# SPECIES WITHOUT OCCURENCE, COORDINATES WITH > 1 OCC AND WHICH SPECIES, ...  
results <- results[1:nrow(results)-1, 1:ncol(results)-1]
results$lon <- grid$lon
results$lat <- grid$lat

subset(sp_occ, sp_occ$Bradypus.variegatus == 1)
subset(sp_occ, sp_occ$teste == 1)

# ================================================================
# GRID BOUDARY TO DETECT PRETTY GOOD OUTLIERS
grid_brasil <- read.csv('/home/gustavo/Desenvolvimento/Coord/Testing/Ids_coord_bacias_csv.csv')

x <- max(grid_brasil$lat) - min(grid_brasil$lat)
y <- max(grid_brasil$lon) - min(grid_brasil$lon)

library(raster)
install.packages('qlcVisualize')
library(qlcVisualize)

boundary(grid_brasil)
