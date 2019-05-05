runAlgorithm = function(predictors, pres_abs) {
  if (predict_variables$can_run_algorithm) {
    start_time <- Sys.time()
    showModal(modalDialog(
      title = "Hey",
      footer = NULL,
      easyClose = FALSE,
      "This may take some time... coffee?"
    ))
    
    datafiles <- predictors
    
    stck = stack() 
    names_stack <- c()
    for(i in 1:NROW(datafiles)){
      names_stack <- c(names_stack, substring(datafiles[i, ]$name, 1, nchar(datafiles[i, ]$name) - 4))
      tempraster = raster(datafiles[i, ]$datapath)
      stck = stack(stck,tempraster)
    }
    names(stck) <- names_stack
    
    data <- pres_abs
    
    data <- data[, 2:3]
    
    prs1 = extract(stck, data)
    prs1_df <- data.frame(prs1)
    prs1_df$long <- data$long
    prs1_df$lat <- data$lat
    
    n <- runif(1, min=1, max=999999);
    set.seed(n)
    backgr = randomPoints(stck, nrow(data)) 
    absvals = extract(stck, backgr) 
    absvals_df <- data.frame(absvals)
    absvals_df$long <- backgr[, 'x']
    absvals_df$lat <- backgr[, 'y']
    pb = c(rep(1, nrow(prs1)), rep(0, nrow(absvals)))
    sdmdata = data.frame(cbind(pb, rbind(prs1_df, absvals_df)))
    sdmdata=na.omit(sdmdata)
    
    # === SDM ===========================================================
    # WGScoor2 <- sdmdata
    # coordinates(WGScoor2)=~long+lat
    # proj4string(WGScoor2)<- CRS("+proj=longlat +datum=WGS84")
    # sdmData_shapefile <-spTransform(WGScoor2,CRS("+proj=longlat"))
    # 
    # d <- sdmData(formula=pb~., train=sdmData_shapefile, predictors=stck)
    # ===================================================================
    
    # === BIOMOD =========================================================
    spName <- input$sp_name
    myBiomodData <- BIOMOD_FormatingData(resp.var = sdmdata$pb,
                                         expl.var = stck,
                                         resp.xy = sdmdata[, c('long', 'lat')],
                                         resp.name = spName)
    
    # ======================================================================
    
    
    algorithm_selected <- subset(predict_variables$algorithms, name %in% input$select_input_algorithm)$method
    
    algorithm <- c()
    for (algo in algorithm_selected) {
      algorithm <- c(algorithm, algo)
    }
    
    
    # === SDM ========================================================================
    # m <- sdm(pb~.,data=d,methods=algorithm, replicatin='sub', 
    #          test.percent = (100 - as.numeric(input$training_set)), n = as.numeric(input$number_of_executions))
    # ================================================================================
    
    # === BIOMOD =====================================================================
    print(">>>>>>>>>>")
    print(input$select_input_eval_method)
    print(">>>>>>>>>>")
    myBiomodOption <- BIOMOD_ModelingOptions()
    myBiomodModelOut <- BIOMOD_Modeling(
      myBiomodData,
      models = algorithm,
      models.options = myBiomodOption,
      NbRunEval=as.numeric(input$number_of_executions),
      DataSplit=as.numeric(input$training_set),
      Prevalence=0.5,
      VarImport=3, #length(stck@layers),
      # models.eval.meth = c('TSS','ROC'),
      models.eval.meth = input$select_input_eval_method,
      SaveObj = FALSE,
      rescal.all.models = TRUE,
      do.full.models = FALSE,
      modeling.id = paste("só pra passar","FirstModeling",sep=""))
    
    # ================================================================================
    
    predict_variables$predictive_model <- myBiomodModelOut
    
    print(">>> model:")
    print(predict_variables$predictive_model)
    print(">>> model info:")
    # model_info <- getModelInfo(m)
    model_info <- get_evaluations(myBiomodModelOut)
    print("\n>>>>> MODELO")
    print(model_info)
    
    BiomodModelsProjection <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
                                                new.env = stck,
                                                proj.name = 'current',
                                                selected.models = 'all',
                                                binary.meth = 'TSS',
                                                compress = FALSE,
                                                build.clamping.mask = FALSE)
    
    
    predict_variables$predictive_map <- BiomodModelsProjection
    
    #   SDM ==========================================================================
    # e1 <- ensemble(m, newdata = stck, filename = 'e1.img',
    #                setting = list(method = 'weighted', stat = 'AUC'), overwrite = TRUE)
    
    # ===============================================================================
    if (input$ensemble_switch_btn) {
      myBiomodEM <- BIOMOD_EnsembleModeling( modeling.output = myBiomodModelOut,
                                             chosen.models = 'all',
                                             em.by = 'all',
                                             eval.metric = input$select_input_eval_method_ensemble,
                                             eval.metric.quality.threshold = c(0.7),
                                             models.eval.meth = input$select_input_eval_method,
                                             prob.mean = TRUE,
                                             prob.cv = FALSE,
                                             prob.ci = FALSE,
                                             prob.ci.alpha = 0.05,
                                             prob.median = FALSE,
                                             committee.averaging = FALSE,
                                             prob.mean.weight = TRUE,
                                             prob.mean.weight.decay = 'proportional' )   
      
      # myBiomodEM
      print("\n>>>>>>ENSEMBLE")
      print(get_evaluations(myBiomodEM))
      predict_variables$ensemble_model <- myBiomodEM
      predict_variables$ensemble_map <- BIOMOD_EnsembleForecasting( projection.output = BiomodModelsProjection,
                                                                    EM.output = myBiomodEM)
    }
    
    end_time <- Sys.time()
    predict_variables$execution_time = round((end_time - start_time), 2)
    
    showModal(modalDialog(
      title = "Nice work!",
      footer = NULL,
      easyClose = TRUE
    ))
  }
}