library(maptools)
library(rgdal)
library(sp)
library(caret)
library(sdm)
library(raster)
library(dismo)

# # le arquivo
# horn=read.csv("/home/gustavo/Documentos/SDMGISR-files/2_SDM Data/hornbill_my1.csv")
# 
# # le arquivo 2
# # pa=read.csv("/home/gustavo/Documentos/SDMGISR-files/2_SDM Data/Pres_abs.csv")
# 

setwd("/home/gustavo/Documentos/SDMGISR-files/2_SDM Data/bioclim")

# cria uma stack de predictors ====================================================
datafiles = Sys.glob("*.tif")
datafiles #list of predictors
stck = stack() #empty raster stack for storing raster layers
for(i in 1:NROW(datafiles)){
  tempraster = raster(datafiles[i])
  stck = stack(stck,tempraster)
}
# =================================================================================


# download das ocorrencias da espécie utilizando o GBIF ===========================
library(spocc)
spp = c("Buceros rhinoceros")
buceros_rhinoceros = occ(spp, from = c('gbif'))
df <-occ2df(buceros_rhinoceros)
df[df$prov == 'bison', ]
nrow(df)
head(buceros_rhinoceros)
data = occ2df(buceros_rhinoceros)
# apenas lat e long
data <- data[, 2:3]
nrow(data)
# =================================================================================


# pegando as informações do predictors em cada ponto de ocorrência  ===============
prs1 = extract(stck, data)
prs1_df <- data.frame(prs1)
prs1_df$long <- data$longitude
prs1_df$lat <- data$latitude
# =================================================================================


# gerando pontos de ausências para o modelo =======================================
set.seed(1)
backgr = randomPoints(stck, 500) #500 random points
absvals = extract(stck, backgr) #choose absence values from the background
absvals_df <- data.frame(absvals)
absvals_df$long <- backgr[, 'x']
absvals_df$lat <- backgr[, 'y']
pb = c(rep(1, nrow(prs1)), rep(0, nrow(absvals)))
sdmdata = data.frame(cbind(pb, rbind(prs1_df, absvals_df)))
head(sdmdata)
sdmdata=na.omit(sdmdata)
summary(sdmdata)
# =================================================================================

# separando conjuntos de treino e teste ===========================================
# trainIndex = createDataPartition(sdmdata$pb, p = .75, list = FALSE) 
# training = sdmdata[ trainIndex,] 
# testing= sdmdata[-trainIndex,] 
# =================================================================================


# convertendo o data frame training em um shapefile da classe SpatialPoints =======
# WGScoor <- sdmdata
# coordinates(WGScoor)=~long+lat
# proj4string(WGScoor)<- CRS("+proj=longlat +datum=WGS84")
# training_data <-spTransform(WGScoor,CRS("+proj=longlat"))
# =================================================================================


# convertendo o data frame sdmData em um shapefile da classe SpatialPoints ========
WGScoor2 <- sdmdata
coordinates(WGScoor2)=~long+lat
proj4string(WGScoor2)<- CRS("+proj=longlat +datum=WGS84")
sdmData_shapefile <-spTransform(WGScoor2,CRS("+proj=longlat"))
# =================================================================================


# gerando sdmData =================================================================
plot(sdmData_shapefile)
d <- sdmData(formula=pb~., train=sdmData_shapefile, predictors=stck)
d
plot(d)
# =================================================================================


# modelo  =========================================================================
m <- sdm(pb~.,data=d,methods=c('rf', 'fda','mars','svm'), replicatin='sub', 
         test.percent = 25, n = 1)

evaluations <- getEvaluation(m)
evaluations

slot(models$pb, "statistics")

model_info <- getModelInfo(m)
model_info

roc(m)
roc(m,smooth=T)
# =================================================================================


# predict =========================================================================
p1 <- predict(m, newdata = stck, filename = 'p1.img', overwrite = TRUE) 
plot(p1)
nlayers(p1)
plot(p1[[1:2]])

# média
p1m <- predict(m, newdata = stck, filename = 'p2m.img', mean=T, overwrite = TRUE)
plot(p1m)
# =================================================================================


# ensemble ========================================================================

e1 <- ensemble(m, newdata = stck, filename = 'e1.img', 
               setting = list(method = 'weighted', stat = 'AUC'), overwrite = TRUE) 

plot(e1)

e2 <- ensemble(m, newdata = stck, filename = 'e2.img',
               setting=list(method = 'weighted', stat = 'TSS', opt = 2))
e2
plot(e2)

e3 <- ensemble(m, newdata = stck, filename = 'e3.img', setting = list(method = 'unweighted'))
plot(e3)
# ==================================================================================


# gui ==============================================================================
installAll()
gui(m)
# ==================================================================================