# install.packages("ggmap")
# library(ggmap)
# 
brazil <- data.frame(lat = c(-6.081689, 5.826789, 6.569828, 9.443383, 3.583828),
                  lon = c(145.3919, 144.2959, 146.7262, 147.2200, 143.6692))
# 
# map <- get_map(location = 'Brazil', zoom = 4)
# 
# mapPoints <- ggmap(map) + 
#   geom_point(aes(x = long, y = lat), data = sp1, alpha = .5)
# 
# mapPoints

install.packages("dismo")
install.packages("raster")
install.packages("rgdal")
install.packages("rJava")
install.packages("maptools")
library(dismo)
library(maptools)

# file <- paste(system.file(package="dismo"), "/home/gustavo.tofoli/Área de Trabalho/bradypus.csv", sep="")

data("wrld_simpl")
sp <- read.table("/home/gustavo.tofoli/Área de Trabalho/sp.csv",  header=TRUE,  sep=",")
grid <- read.table("/home/gustavo.tofoli/Área de Trabalho/grid.csv",  header=TRUE,  sep=",")

sps <- unique(file$sp)
plot(wrld_simpl, xlim=c(-80,70), ylim=c(-60,10), axes=TRUE, col="light yellow")
box()

for (sp in sort(sps)) {
  sp <- subset(file, file$sp == sps[sp])[,2:3]
  points(sp$lon, sp$lat, col=c('blue'), pch=20, cex=0.75)
  # legend(x='right', legend=rev(sps),
         # box.lty=0, fill=rev(c("red", "blue")),cex=.8, ncol=1)
}

# add pints
# points(teste$lon, teste$lat, col='blue', pch=20, cex=0.75)

# result <- c()
diam <- 5
r <- diam/2
# coord_sp <- file[,2:3]
# condition <- coord_sp$lon <= brazil$long + r & coord_sp$lon >= brazil$long - r & 
#   coord_sp$lat <= brazil$lat + r & coord_sp$lat >= brazil$lat - r
# file

# for (coord_sp in sp[,2:3]) {
#   print(coord_sp[1])
#   for (coord_brazil in brazil) {
#     print(coord_sp[1] <= coord_brazil[1])
#   }
# }

condition <- sp$lon <= grid$lon + r & sp$lon >= grid$lon - r &
             sp$lat <= grid$lat + r & sp$lat >= grid$lat - r

teste <- c()
for (specie in levels(unique(sp$sp))) {
  # print(specie)
  teste <- c(teste, specie)
}
teste_df <- data.frame(teste)

teste_df <- as.data.frame(matrix(rep(0, 2 + length(teste)), nrow=1))
names(teste_df) <- c("lon", "lat", teste)

teste_df$`Bradypus variegatus` <- 1


teste
teste_df



goalsMenu <- paste("Name", 1:40, sep="")
output <- as.data.frame(matrix(rep(0, 5 + length(teste)), nrow=1))
names(output) <- c("analysis", "patient", "date", teste, "CR1", "CR2")

output







col.names <- teste  # Example column names
data <- vector("list", length(col.names))
names(data) <- col.names
print(str(data))            # Inspect the structure

data$`Bradypus variegatus` <- condition

