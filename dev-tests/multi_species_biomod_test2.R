library(maptools)
library(rgdal)
library(sp)
library(caret)
library(sdm)
library(raster)
library(dismo)
library(biomod2)

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
sp.names = c("Buceros rhinoceros", "Megalaima chrysopogon")
species = occ(sp.names, from = c('gbif'), gbifopts = list(hasCoordinate=TRUE))
head(species)
data = occ2df(species)
other_sp <- subset(data, name == sp.names[2])
# apenas lat e long
# data <- data[, 2:3]
nrow(data)
# =================================================================================


for (sp.n in sp.names) {
      # pegando as informações do predictors em cada ponto de ocorrência  ===============
      subset_data = subset(data, data$name == sp.n)[, 2:3]
      prs1 = extract(stck, subset_data)
      prs1_df <- data.frame(prs1)
      prs1_df$long <- subset_data$longitude
      prs1_df$lat <- subset_data$latitude
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
      spName <- sp.n
      myBiomodData <- BIOMOD_FormatingData(resp.var = sdmdata$pb,
                                           expl.var = stck,
                                           resp.xy = sdmdata[, c('long', 'lat')],
                                           resp.name = spName)
      # myBiomodData
      # plot(myBiomodData)
      # level.plot(sdmdata$pb, 
      #            sdmdata[, c('long', 'lat')],
      #            show.scale = TRUE,
      #            title = "Teste")
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
      
      # =================================================================================
      
      # 4. Doing Ensemble Modelling
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
                                             prob.mean.weight.decay = 'proportional' 
                                             
                                             
                                             )   
      
      
      myBiomodProjection <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
                                              new.env = stck,
                                              proj.name = 'current',
                                              selected.models = 'all',
                                              binary.meth = 'TSS',
                                              compress = FALSE,
                                              build.clamping.mask = FALSE)
      
      
      ensemble_forecasting_proj <- BIOMOD_EnsembleForecasting(
        projection.output = myBiomodProjection,
        EM.output = myBiomodEM)
}




# define a mask of studied
alphaMap <- reclassify(subset(stck,1), c(-Inf,Inf,0))
# # add all other species map
for(sp.n in sp.names){
  # add layer
  alphaMap <-
    alphaMap +
    subset(stack(file.path(paste0(gsub(" ", ".", sp.n),
                             paste0("/proj_current/individual_projections/",
                                   gsub(" ", ".", sp.n),
                                   "_EMmeanByROC_mergedAlgo_mergedRun_mergedData.grd", sep="")))), 1)
}
# summary of created raster
alphaMap

plot(alphaMap)
