# load the library
library(biomod2)
# load our species data
DataSpecies <- read.csv(system.file("external/species/mammals_table.csv",
                                    package="biomod2"))
head(DataSpecies)

# the name of studied species
myRespName <- 'GuloGulo'
# the presence/absences data for our species
myResp <- as.numeric(DataSpecies[,myRespName])
# the XY coordinates of species data
myRespXY <- DataSpecies[,c("X_WGS84","Y_WGS84")]
# load the environmental raster layers (could be .img, ArcGIS
# rasters or any supported format by the raster package)
# Environmental variables extracted from Worldclim (bio_3, bio_4,
# bio_7, bio_11 & bio_12)
myExpl = stack( system.file( "external/bioclim/current/bio3.grd",
                             package="biomod2"),
                system.file( "external/bioclim/current/bio4.grd",
                             package="biomod2"),
                system.file( "external/bioclim/current/bio7.grd",
                             package="biomod2"),
                system.file( "external/bioclim/current/bio11.grd",
                             package="biomod2"),
                system.file( "external/bioclim/current/bio12.grd",
                             package="biomod2"))

myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = myExpl,
                                     resp.xy = myRespXY,
                                     resp.name = myRespName)

myBiomodData
plot(myBiomodData)

# 2. Defining Models Options using default options.
myBiomodOption <- BIOMOD_ModelingOptions()

# 3. Computing the models
myBiomodModelOut <- BIOMOD_Modeling(
  myBiomodData,
  models = c('SRE','CTA','RF','MARS','FDA'),
  models.options = myBiomodOption,
  NbRunEval=3,
  DataSplit=80,
  Prevalence=0.5,
  VarImport=3,
  models.eval.meth = c('TSS','ROC'),
  SaveObj = TRUE,
  rescal.all.models = TRUE,
  do.full.models = FALSE,
  modeling.id = paste(myRespName,"FirstModeling",sep=""))

myBiomodModelOut

# get all models evaluation
myBiomodModelEval <- get_evaluations(myBiomodModelOut)
# print the dimnames of this object
dimnames(myBiomodModelEval)

# let's print the TSS scores of Random Forest
myBiomodModelEval["TSS","Testing.data","RF",,]

# let's print the ROC scores of all selected models
myBiomodModelEval["ROC","Testing.data",,,]

# print variable importances
get_variables_importance(myBiomodModelOut)

myBiomodEM <- BIOMOD_EnsembleModeling(
  modeling.output = myBiomodModelOut,
  chosen.models = 'all',
  em.by='all',
  eval.metric = c('TSS'),
  eval.metric.quality.threshold = c(0.7),
  prob.mean = T,
  prob.cv = T,
  prob.ci = T,
  prob.ci.alpha = 0.05,
  prob.median = T,
  committee.averaging = T,
  prob.mean.weight = T,
  prob.mean.weight.decay = 'proportional' )

# print summary
myBiomodEM
# get evaluation scores
get_evaluations(myBiomodEM)

# projection over the globe under current conditions
myBiomodProj <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = myExpl,
  proj.name = 'current',
  selected.models = 'all',
  binary.meth = 'TSS',
  compress = 'xz',
  clamping.mask = F,
  output.format = '.grd')

# summary of crated oject
myBiomodProj

# files created on hard drive
list.files("GuloGulo/proj_current/")

# make some plots sub-selected by str.grep argument
plot(myBiomodProj, str.grep = 'MARS')

# if you want to make custom plots, you can also get the projected map
myCurrentProj <- get_predictions(myBiomodProj)
myCurrentProj

# load environmental variables for the future.
myExplFuture = stack( system.file( "external/bioclim/future/bio3.grd",
                                   package="biomod2"),
                      system.file( "external/bioclim/future/bio4.grd",
                                   package="biomod2"),
                      system.file( "external/bioclim/future/bio7.grd",
                                   package="biomod2"),
                      system.file( "external/bioclim/future/bio11.grd",
                                   package="biomod2"),
                      system.file( "external/bioclim/future/bio12.grd",
                                   package="biomod2"))
myBiomodProjFuture <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = myExplFuture,
  proj.name = 'future',
  selected.models = 'all',
  binary.meth = 'TSS',
  compress = 'xz',
  clamping.mask = T,
  output.format = '.grd')

# make some plots, sub-selected by str.grep argument
plot(myBiomodProjFuture, str.grep = 'MARS')

myBiomodEF <- BIOMOD_EnsembleForecasting(
  EM.output = myBiomodEM,
  projection.output = myBiomodProj)

myBiomodEF
# reduce layer names for plotting convegences
plot(myBiomodEF)
