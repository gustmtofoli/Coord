library(maptools)
library(rgdal)
library(sp)
library(caret)
library(sdm)

# le arquivo
horn=read.csv("/home/gustavo/Documentos/SDMGISR-files/2_SDM Data/hornbill_my1.csv")

# le arquivo 2
# pa=read.csv("/home/gustavo/Documentos/SDMGISR-files/2_SDM Data/Pres_abs.csv")

library(spocc)
spp = c("Buceros rhinoceros")
buceros_rhinoceros = occ(spp, from = c('gbif'), gbifopts = list(hasCoordinate=TRUE))
head(buceros_rhinoceros)
data = occ2df(buceros_rhinoceros)
data

prs1= extract(stck, horn1)

set.seed(1)

backgr = randomPoints(stck, 500) #500 random points
absvals = extract(stck, backgr) #choose absence values from the background
pb = c(rep(1, nrow(prs1)), rep(0, nrow(absvals)))
sdmdata = data.frame(cbind(pb, rbind(prs1, absvals)))

head(sdmdata)

sdmdata=na.omit(sdmdata)
summary(sdmdata)

tail(sdmdata)





# buceros_rhinoceros_coord <- buceros_rhinoceros$gbif
# b1 <- buceros_rhinoceros_coord$data
# b2 <- b1$Buceros_rhinoceros
# names(b2)


# filtra apenas as informações referentes a espécie Buceros rhinoceros
buceros_rhinoceros = subset(horn, species == 'Buceros rhinoceros')

# retira a coluna do nome da espécie
buceros_rhinoceros_coord = buceros_rhinoceros

# training and testing
trainIndex = createDataPartition(buceros_rhinoceros_coord$species, 
                                 p = .75, 
                                 list = FALSE) 

training = buceros_rhinoceros_coord[ trainIndex,] 
# nrow(training)
testing= buceros_rhinoceros_coord[-trainIndex,] 
# nrow(testing)

training <- training[, 2:3]

# converte o data frame training em um shapefile da classe SpatialPoints
WGScoor <- training
coordinates(WGScoor)=~long+lat
proj4string(WGScoor)<- CRS("+proj=longlat +datum=WGS84")
training_data <-spTransform(WGScoor,CRS("+proj=longlat"))

# cria uma stack de predictors
datafiles = Sys.glob("*.tif")
datafiles #list of predictors

stck = stack() #empty raster stack for storing raster layers
for(i in 1:NROW(datafiles)){
  tempraster = raster(datafiles[i])
  stck = stack(stck,tempraster)
}

d <- sdmData(formula=Ocurrence~., train=training_data, predictors=stck)
d

m <- sdm(species~.,data=d,methods=c('rf', 'fda','mars','svm'),
         replicatin='boot',n=10)
