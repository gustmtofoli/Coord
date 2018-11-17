## Not run: 
# Loading data
library(SSDM)
data(Env)
data(Occurrences)
Occurrences_elliptica <- subset(Occurrences, SPECIES == 'elliptica')

SDM <- modelling('GLM', Occurrences_elliptica, 
                 Env, Xcol = 'LONGITUDE', Ycol = 'LATITUDE', verbose = FALSE)
plot(SDM@projection, main = 'SDM\nfor Cryptocarya elliptica\nwith GLM algorithm')


# ==================================================================================
# ensemble SDM building
ESDM <- ensemble_modelling(c('CTA', 'MARS'), Occurrences, Env, rep = 1,
                           Xcol = 'LONGITUDE', Ycol = 'LATITUDE',
                           ensemble.thresh = c(0.6))

# Results plotting
plot(ESDM)

plot(ESDM@projection, main = 'ESDM\nfor Cryptocarya elliptica\nwith CTA and MARS algorithms')


# =========================================================================
# ==============================
SSDM <- stack_modelling(c('CTA', 'SVM'), Occurrences, Env, rep = 1,
                        Xcol = 'LONGITUDE', Ycol = 'LATITUDE',
                        Spcol = 'SPECIES', method = "pSSDM", verbose = FALSE)
plot(SSDM@diversity.map, main = 'SSDM\nfor Cryptocarya genus\nwith CTA and SVM algorithms')
# =========================================================================


knitr::kable(ESDM@evaluation)
knitr::kable(ESDM@variable.importance)

plot(SSDM)

install.packages('shinyFiles')
library(shinyFiles)
gui()
