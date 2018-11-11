#######################################################################
############### ENSEMBLE

setwd("/home/gustavo/Documentos/SDMGISR-files/2_SDM Data/bioclim")

pa=read.csv("/home/gustavo/Documentos/SDMGISR-files/2_SDM Data/Pres_abs.csv")
# subset(pa, pb != 1)
#pa=na.omit(pa)

head(pa)

summary(pa)

pb=as.factor(training$pb) #1 stands for presence and 0 for absence
land=as.factor(training$land) #land use categories are categorical

head(pa)
library(caret)

set.seed(1) #pseudo-repeatability
trainIndex = createDataPartition(pa$pb, p = .75, 
                                 list = FALSE, 
                                 times = 1) #y as basis of splitting

training = pa[ trainIndex,] #75% data for model training
testing= pa[-trainIndex,] #25% for model testing

head(training)


## caret
# define training control--> 10fold cv
train_control = trainControl(method="cv", number=10)


# mod_fit1=train(pb~altitude+aspect1+preciptn+roughness1+slope+tempAvg+tempMin,
#                data=training,trControl=train_control,method="rf", importance=TRUE)






# SSDM
library(SSDM)
data(Env)
data(Occurrences)
Occurrences <- subset(Occurrences, Occurrences$SPECIES == 'elliptica')
# ensemble SDM building
CTA <- modelling('CTA', Occurrences, Env, Xcol = 'LONGITUDE', Ycol = 'LATITUDE')
SVM <- modelling('SVM', Occurrences, Env, Xcol = 'LONGITUDE', Ycol = 'LATITUDE')
ESDM <- ensemble(CTA, SVM, ensemble.thresh = c(0.6))

# Results plotting
plot(ESDM)

# BIOMOD2
library(biomod2)
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = myExpl,
                                     resp.xy = myRespXY,
                                     resp.name = myRespName)

myBiomodOption <- BIOMOD_ModelingOptions()

myBiomodModelOut <- BIOMOD_Modeling( training, 
                                     models = c('SRE','CTA','RF'), 
                                     models.options = myBiomodOption, 
                                     NbRunEval=1, 
                                     DataSplit=80, 
                                     Yweights=NULL, 
                                     VarImport=3, 
                                     models.eval.meth = c('TSS'),
                                     SaveObj = TRUE,
                                     rescal.all.models = FALSE,
                                     do.full.models = FALSE)

















datafiles = Sys.glob("*.tif") #Or whatever identifies your files

datafiles #list of predictors
stck = stack() #empty raster stack for storing raster layers
for(i in 1:NROW(datafiles)){
  tempraster = raster(datafiles[i])
  stck = stack(stck,tempraster)
}
