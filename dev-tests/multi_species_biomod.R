library(spocc)
library(maptools)
library(rgdal)
library(sp)
library(caret)
library(sdm)
library(raster)
library(dismo)
library(biomod2)

setwd("/home/gustavo/Documentos/SDMGISR-files/2_SDM Data/bioclim")

datafiles = Sys.glob("*.tif")
datafiles #list of predictors
stck = stack() #empty raster stack for storing raster layers
for(i in 1:NROW(datafiles)){
  tempraster = raster(datafiles[i])
  stck = stack(stck,tempraster)
}

sp.names = c("Buceros rhinoceros", "Falco tinnunculus")
# falco = c("Falco tinnunculus")
species = occ(spp, from = c('gbif'), gbifopts = list(hasCoordinate=TRUE))
head(species)
data = occ2df(species)
# apenas lat e long
data_falco = subset(data, name == "Falco tinnunculus")
data <- data[, 1:3]
nrow(data)

sdmData = NULL
col_names <- c()
count <- 0
backgr = NULL
absvals = NULL
absvals_df = NULL
pb = c()
pb_list = list(list())
prs1 = NULL
prs1_df = NULL
colu <- 0
for (sp.n in sp.names) {
  colu <- colu + 1
  count <- count + 1
  data_subset = subset(data, name == sp.n)[2:3]
  prs1 = extract(stck, data_subset)
  prs1_df <- data.frame(prs1)
  prs1_df$long <- data_subset$longitude
  prs1_df$lat <- data_subset$latitude
  
  if (count <= 1) {
    set.seed(1)
    backgr = randomPoints(stck, 500) #500 random points
  }

  absvals = extract(stck, backgr) #choose absence values from the background
  absvals_df <- data.frame(absvals)
  absvals_df$long <- backgr[, 'x']
  absvals_df$lat <- backgr[, 'y']
  pb = c(rep(1, nrow(prs1)), rep(0, nrow(absvals)))
  
  
  pb_list[[colu]] <- pb
  
  
  col_names <- c(col_names, sp.n)
  col_names <- c(col_names, colnames(prs1))
}
sdmdata = NULL
length(pb)
pb_list[[1]]

sdmdata = data.frame(rbind(prs1_df, absvals_df))
k = 0
for (sp.n in sp.names) {
  k <- k + 1
  sdmdata[ , paste0(sp.n)] = pb_list[[k]]
}

# sdmdata = data.frame(cbind(pb_list, rbind(prs1_df, absvals_df)))


head(sdmdata)
sdmdata=na.omit(sdmdata)
summary(sdmdata)
plot(sdmdata)

myBiomodData = NULL
for (sp.n in sp.names) {
  myBiomodData <- BIOMOD_FormatingData(resp.var = sdmdata[, sp.names[2]],
                                       expl.var = stck,
                                       resp.xy = sdmdata[, c('long', 'lat')],
                                       resp.name = sp.n)
  plot(myBiomodData)
  
  myBiomodOption <- BIOMOD_ModelingOptions()
  
  
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
  
  myBiomodEM <- BIOMOD_EnsembleModeling( modeling.output = myBiomodModelOut,
                                         chosen.models = 'all',
                                         em.by = 'all',
                                         eval.metric = c("ROC", "TSS"),
                                         eval.metric.quality.threshold = NULL,
                                         models.eval.meth = c('TSS','ROC'),
                                         prob.mean = TRUE,
                                         prob.cv = FALSE,
                                         prob.ci = FALSE,
                                         prob.ci.alpha = 0.05,
                                         prob.median = FALSE,
                                         committee.averaging = FALSE,
                                         prob.mean.weight = TRUE,
                                         prob.mean.weight.decay = 'proportional' )   
  
  myBiomodProjection <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
                                          new.env = stck,
                                          proj.name = 'current',
                                          selected.models = 'all',
                                          binary.meth = 'TSS',
                                          compress = FALSE,
                                          build.clamping.mask = FALSE)
  
  myBiomodEF <- BIOMOD_EnsembleForecasting(
    projection.output = myBiomodProjection,
    EM.output = myBiomodEM)
  
}

plot(myBiomodData)

