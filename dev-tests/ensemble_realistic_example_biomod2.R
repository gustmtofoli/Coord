library(maptools)
library(rgdal)
library(sp)
library(caret)
library(sdm)
library(raster)
library(dismo)
library(biomod2)
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
buceros_rhinoceros = occ(spp, from = c('gbif'), gbifopts = list(hasCoordinate=TRUE))
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


#  biomod data ====================================================================
spName <- "Buceros rhinoceros"
myBiomodData <- BIOMOD_FormatingData(resp.var = sdmdata$pb,
                                     expl.var = stck,
                                     resp.xy = sdmdata[, c('long', 'lat')],
                                     resp.name = spName)
myBiomodData
plot(myBiomodData)
# =================================================================================


# options =========================================================================
myBiomodOption <- BIOMOD_ModelingOptions()
# =================================================================================

nlayers(stck)
# models ==========================================================================
myBiomodModelOut <- BIOMOD_Modeling(
  myBiomodData,
  models = c('GBM','RF'),
  models.options = myBiomodOption,
  NbRunEval=1,
  DataSplit=80,
  Prevalence=0.5,
  VarImport=3, #length(stck@layers),
  models.eval.meth = c('TSS','ROC'),
  SaveObj = TRUE,
  rescal.all.models = TRUE,
  do.full.models = FALSE,
  modeling.id = "dsadsa")

myBiomodModelOut
evaluations <- get_evaluations(myBiomodModelOut)
colnames(evaluations)
df_eval <- data.frame(evaluations)
df_eval[, 5]




# =================================================================================

# 4. Doing Ensemble Modelling
myBiomodEM <- BIOMOD_EnsembleModeling( modeling.output = myBiomodModelOut,
                                       chosen.models = 'all',
                                       em.by = 'all',
                                       eval.metric = c('TSS'),
                                       eval.metric.quality.threshold = c(0.7),
                                       models.eval.meth = c('TSS','ROC'),
                                       prob.mean = TRUE,
                                       prob.cv = FALSE,
                                       prob.ci = FALSE,
                                       prob.ci.alpha = 0.05,
                                       prob.median = FALSE,
                                       committee.averaging = FALSE,
                                       prob.mean.weight = TRUE,
                                       prob.mean.weight.decay = 'proportional' )   

myBiomodEM
get_evaluations(myBiomodEM)



myBiomodProjection <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
                                        new.env = stck,
                                        proj.name = 'current',
                                        selected.models = 'all',
                                        binary.meth = 'TSS',
                                        compress = FALSE,
                                        build.clamping.mask = FALSE)

plot(myBiomodProjection)
