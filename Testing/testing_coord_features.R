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
  addTiles(
    urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
  ) %>%
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

m <- leaflet(states) %>%
  setView(-60.85, -15.45, 3) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

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




