#########################################################################################################################################################################
# This script 1. and 2. generate a batch of parameter files for the Omniscape algorithm , and 3. identifies ecological continuities from Omniscape outputs 
#########################################################################################################################################################################

#####################################################################################################################
###################################### Prelim. Output folder creation  ##############################################
#####################################################################################################################
# Create the folder of Omniscape parameter files if does not exist 
if (!dir.exists(here::here('data/derived-data/OmniscapeParamFiles/'))) {
  dir.create(here::here('data/derived-data/OmniscapeParamFiles/'))
}
# Create the folder of Omniscape outputs if does not exist 
if (!dir.exists(here::here('data/derived-data/OmniscapeOutput/'))) {
  dir.create(here::here('data/derived-data/OmniscapeOutput/'))
}
# Create the folder of parameter combinations if does not exist 
if (!dir.exists(here::here('data/derived-data/BatchRun/'))) {
  dir.create(here::here('data/derived-data/BatchRun/'))
}
# Create the folder of ecological continuities if does not exist 
if (!dir.exists(here::here('outputs/EcologicalContinuities/'))) {
  dir.create(here::here('outputs/EcologicalContinuities/'))
  dir.create(here::here('outputs/EcologicalContinuities/Raster/'))
  dir.create(here::here('outputs/EcologicalContinuities/Raster/Probs'))
  dir.create(here::here('outputs/EcologicalContinuities/Vector/'))
}

#####################################################################################################################
########################### 1. Generate Omniscape parameter files: for static state #################################
#####################################################################################################################
# The parameter files are located in the /data/derived-data/OmniscapeParamFiles folder

# Required general dataset 
combi.doable <- openxlsx::read.xlsx(here::here('data/raw-data/FunctionalGroups/List-of-clustering-schemes.xlsx'))
# Parameter setting 
timestep <- 5
p.threshold <- seq(0.5, 0.5, by = 0.1) #suitability threshold 
coef.c <- c(4) #suitability to resistance transformation threshold 
time.per = seq(2020, 2050, by = timestep)
ssp = c('ssp1', 'ssp3')
gcm = c('gfdl-esm4', 'mpi-esm1-2-hr', 'ukesm1-0-ll')
norm.flow <- seq(0.7, 0.9, by = 0.1) #threshold to select pixels from the normalized flow 

path.start <- "/bettik/primam/RFLC-SCP_FutureProjections" #path to the root folder from which Omniscape algorithm will be run

full.param <- data.frame()
full.param.full <- data.frame()

for (i in 1:nrow(combi.doable)) {
  
  g <- combi.doable$group[i] #general class
  k <- combi.doable$Nclus[i] #total number of groups for a class
  
  # Read species list
  lst.sp <- openxlsx::read.xlsx(here::here(paste0('data/raw-data/FunctionalGroups/Species-list_withTraits_', g , '_GroupID_K=', k, '.xlsx')))
  lst.sp <- lst.sp[lst.sp$SPECIES_NAME_SYNONYM != "Pelophylax grafi",]

  for (c in c(1:k)) { #for each group
    
    sub.lst <- lst.sp[lst.sp$cluster.id %in% c, ]
    seq.dd <- quantile(sub.lst$DISPERSAL_KM, probs = c(0.2, 0.5, 0.8))
    if(seq.dd[1] > 1) {
      seq.dd <-  round(seq.dd)
    } else {
     seq.dd <-  round(seq.dd, digits = 1)
    }
    seq.dd <- unique(seq.dd)

    lst.param <- expand.grid(TransfoCoef = as.character(coef.c), DD = seq.dd, SourceThre = p.threshold, TimePeriod = time.per, 
                             SSP = ssp, GCM = gcm)
    lst.param$Group <- g
    lst.param$clusID <- c 
    full.param <- rbind(full.param, lst.param)
    
    lst.param.full <- expand.grid(TransfoCoef = as.character(coef.c), DD = seq.dd, SourceThre = p.threshold, NormFlowThre = norm.flow, 
                                  TimePeriod = time.per, SSP = ssp, GCM = gcm)
    lst.param.full$Group <- g
    lst.param.full$clusID <- c 
    full.param.full <- rbind(full.param.full, lst.param.full)
    
    min.dd <- min(seq.dd)
    
    for (j in 1:nrow(lst.param)) {
      
      transfocoef <- lst.param$TransfoCoef[j]
      dd <- lst.param$DD[j]
      sourcethre <- lst.param$SourceThre[j]
      scen.ssp <- lst.param$SSP[j]
      scen.time <- lst.param$TimePeriod[j]
      scen.gcm <- lst.param$GCM[j]
      
      config.tplate <- read.table(here::here('data/derived-data/OmniscapeTemplate.ini'))
      config.tplate$V3[config.tplate$V1 == 'solver'] <- "cholmod"
      colnames(config.tplate)[1] <- "param"
      colnames(config.tplate)[3] <- "val"
      
      config.tplate$val[config.tplate$param == "resistance_file"] <- ifelse(min.dd >= 1, paste0(path.start,"/data/derived-data/ResistanceSurfaces/ResistanceSurface_", 
                                                                                                g , "_GroupID_", c, "_TransfoCoef_", transfocoef,'_', scen.time, '_', scen.ssp, '_', scen.gcm, ".tif"), 
                                                                            paste0(path.start,"/data/derived-data/ResistanceSurfaces/100m/ResistanceSurface_", 
                                                                                   g , "_GroupID_", c, "_TransfoCoef_", transfocoef, '_', scen.time, '_', scen.ssp, '_', scen.gcm, ".tif"))
      
      config.tplate$val[config.tplate$param == "source_file"] <- ifelse(min.dd >= 1, paste0(path.start, "/data/derived-data/SourceLayers/SourceLayer_", 
                                                                                            g, "_GroupID_", c, "_SuitThreshold_",sourcethre, '_', scen.time, '_', scen.ssp, '_', scen.gcm, ".tif"), 
                                                                        paste0(path.start, "/data/derived-data/SourceLayers/100m/SourceLayer_", 
                                                                               g, "_GroupID_", c, "_SuitThreshold_",sourcethre, '_', scen.time, '_', scen.ssp, '_', scen.gcm, ".tif"))
      
      config.tplate$val[config.tplate$param == "project_name"] <- paste0(path.start, "/data/derived-data/OmniscapeOutput/OmniscapeOutput_", 
                                                                         g, "_GroupID_", c, "_TransfoCoef_", transfocoef, "_SuitThreshold_", sourcethre,"_DispDist_", dd, '_', scen.time, '_', scen.ssp, '_', scen.gcm)
      dd.use <- ifelse(dd < 1, dd*1000, dd) #if less than 1km, convert to meters 
      res.pix <- ifelse(dd < 1, 100, 1) #if dd less than 1km, resolution goes to 100meters otherwise stays at 1km
      block.use <- max(1, floor(dd.use/(res.pix*10)))
      config.tplate$val[config.tplate$param == "radius"] <- dd.use/res.pix  #in pixels 
      config.tplate$val[config.tplate$param == "block_size"] <- block.use
      config.tplate <- paste(config.tplate$param, config.tplate$V2, config.tplate$val)
      
      write.table(config.tplate, here::here(paste0("data/derived-data/OmniscapeParamFiles/IniFile_",g, "_GroupID_", c, "_TransfoCoef_", transfocoef,  "_SuitThreshold_", sourcethre,"_DispDist_", dd,"km", 
                                                   '_', scen.time, '_', scen.ssp, '_', scen.gcm, ".ini")),
                  quote = F, row.names = F, col.names = F)
    }
  }
}

# Generate the file listing all parameter combination (without the normalized flow), useful for batch running on distance cluster
full.param$jobID <- c(1:nrow(full.param))
full.param <- full.param[, c("jobID","Group", "clusID", "TransfoCoef", "SourceThre", "DD", "TimePeriod", "SSP", "GCM")]
full.param$DD <- as.character(full.param$DD)
full.param <- as.matrix(full.param)
write.table(full.param, here::here('data/derived-data/BatchRun/list-of-params-for-batchrun-step1.txt'), row.names = F, col.names = F, quote = F)

# Generate the file listing all parameter combination (with the normalized flow), useful for batch running on distance cluster
full.param.full$jobID <- c(1:nrow(full.param.full))
full.param.full <- full.param.full[, c("jobID","Group", "clusID", "TransfoCoef", "SourceThre", "DD", "NormFlowThre", "TimePeriod", "SSP", "GCM")]
full.param.full$DD <- as.character(full.param.full$DD)
full.param.full <- as.matrix(full.param.full)
write.table(full.param.full, here::here('data/derived-data/BatchRun/list-of-params-for-batchrun-step2.txt'), row.names = F, col.names = F, quote = F)

#####################################################################################################################
########################### 2. Generate Omniscape parameter files: for transient state ##############################
#####################################################################################################################
# The parameter files are located in the /data/derived-data/OmniscapeParamFiles folder

# Required general dataset 
combi.doable <- openxlsx::read.xlsx(here::here('data/raw-data/FunctionalGroups/List-of-clustering-schemes.xlsx'))
# Parameter setting 
timestep <- 5
p.threshold <- seq(0.5, 0.5, by = 0.1) #suitability threshold 
coef.c <- c(4) #suitabilitiy to resistance transformation threshold 
time.per = seq(2020, 2045, by = timestep)
ssp = c('ssp1', 'ssp3')
gcm = c('gfdl-esm4', 'mpi-esm1-2-hr', 'ukesm1-0-ll')
norm.flow <- seq(0.7, 0.9, by = 0.1) #threshold to select pixels from the normalized flow 

path.start <- "/bettik/primam/RFLC-SCP_FutureProjections" #path to the root folder from which Omniscape algorithm will be run

full.param <- data.frame()
full.param.full <- data.frame()

for (i in 1:nrow(combi.doable)) {
  
  g <- combi.doable$group[i] #general class
  k <- combi.doable$Nclus[i] #total number of groups for a class
  
  # Read species list
  lst.sp <- openxlsx::read.xlsx(here::here(paste0('data/raw-data/FunctionalGroups/Species-list_withTraits_', g , '_GroupID_K=', k, '.xlsx')))
  lst.sp <- lst.sp[lst.sp$SPECIES_NAME_SYNONYM != "Pelophylax grafi",]

  for (c in c(1:k)) { #for each group
    
    sub.lst <- lst.sp[lst.sp$cluster.id %in% c, ]
    
    seq.dd <- quantile(sub.lst$DISPERSAL_KM, probs = c(0.2, 0.5, 0.8))
    if(seq.dd[1] > 1) {
      seq.dd <-  round(seq.dd)
    } else {
      seq.dd <-  round(seq.dd, digits = 1)
    }
    seq.dd <- unique(seq.dd)
    
    lst.param <- expand.grid(TransfoCoef = as.character(coef.c), DD = seq.dd, SourceThre = p.threshold, TimePeriod = time.per, 
                             SSP = ssp, GCM = gcm)
    lst.param$Group <- g
    lst.param$clusID <- c 
    full.param <- rbind(full.param, lst.param)
    
    lst.param.full <- expand.grid(TransfoCoef = as.character(coef.c), DD = seq.dd, SourceThre = p.threshold, NormFlowThre = norm.flow, 
                                  TimePeriod = time.per, SSP = ssp, GCM = gcm)
    lst.param.full$Group <- g
    lst.param.full$clusID <- c 
    full.param.full <- rbind(full.param.full, lst.param.full)
    
    min.dd <- min(seq.dd)
    
    for (j in 1:nrow(lst.param)) {
      
      transfocoef <- lst.param$TransfoCoef[j]
      dd <- lst.param$DD[j]
      sourcethre <- lst.param$SourceThre[j]
      scen.ssp <- lst.param$SSP[j]
      scen.time <- lst.param$TimePeriod[j]
      scen.gcm <- lst.param$GCM[j]
      
      config.tplate <- read.table(here::here('data/derived-data/OmniscapeTemplate.ini'))
      config.tplate$V3[config.tplate$V1 == 'solver'] <- "cholmod"
      colnames(config.tplate)[1] <- "param"
      colnames(config.tplate)[3] <- "val"
      
      config.tplate$val[config.tplate$param == "conditional"] <- 'true'
      config.tplate$val[config.tplate$param == "condition1_file"] <- ifelse(min.dd >= 1, paste0(path.start,"/data/derived-data/ConditionLayers/ConditionLayerCurrent_", 
                                                                                                g , "_GroupID_", c, "_SuitThreshold_", sourcethre ,'_', scen.time + timestep/2, '_', scen.ssp, '_', scen.gcm, ".tif"), 
                                                                            paste0(path.start,"/data/derived-data/ConditionLayers/100m/ConditionLayerCurrent_", 
                                                                                   g , "_GroupID_", c, "_SuitThreshold_",sourcethre , '_', scen.time + timestep/2, '_', scen.ssp, '_', scen.gcm, ".tif"))
     
       config.tplate$val[config.tplate$param == "condition1_future_file"] <- ifelse(min.dd >= 1, paste0(path.start,"/data/derived-data/ConditionLayers/ConditionLayerFuture_", 
                                                                                                g , "_GroupID_", c, "_SuitThreshold_", sourcethre ,'_', scen.time + timestep/2, '_', scen.ssp, '_', scen.gcm, ".tif"), 
                                                                            paste0(path.start,"/data/derived-data/ConditionLayers/100m/ConditionLayerFuture_", 
                                                                                   g , "_GroupID_", c, "_SuitThreshold_", sourcethre , '_', scen.time + timestep/2, '_', scen.ssp, '_', scen.gcm, ".tif"))
      
      config.tplate$val[config.tplate$param == "resistance_file"] <- ifelse(min.dd >= 1, paste0(path.start,"/data/derived-data/ResistanceSurfaces/ResistanceSurface_", 
                                                                                                g , "_GroupID_", c, "_TransfoCoef_", transfocoef,'_', scen.time + timestep/2, '_', scen.ssp, '_', scen.gcm, ".tif"), 
                                                                            paste0(path.start,"/data/derived-data/ResistanceSurfaces/100m/ResistanceSurface_", 
                                                                                   g , "_GroupID_", c, "_TransfoCoef_", transfocoef, '_', scen.time + timestep/2, '_', scen.ssp, '_', scen.gcm, ".tif"))
      
      config.tplate$val[config.tplate$param == "source_file"] <- ifelse(min.dd >= 1, paste0(path.start, "/data/derived-data/SourceLayers/SourceLayer_", 
                                                                                            g, "_GroupID_", c, "_SuitThreshold_",sourcethre, '_', scen.time + timestep/2, '_', scen.ssp, '_', scen.gcm, ".tif"), 
                                                                        paste0(path.start, "/data/derived-data/SourceLayers/100m/SourceLayer_", 
                                                                               g, "_GroupID_", c, "_SuitThreshold_",sourcethre, '_', scen.time + timestep/2, '_', scen.ssp, '_', scen.gcm, ".tif"))
      
      config.tplate$val[config.tplate$param == "project_name"] <- paste0(path.start, "/data/derived-data/OmniscapeOutput/OmniscapeOutput_Transient_", 
                                                                         g, "_GroupID_", c, "_TransfoCoef_", transfocoef, "_SuitThreshold_", sourcethre,"_DispDist_", 
                                                                         dd, '_', scen.time, '_', scen.ssp, '_', scen.gcm)
      
      dd.use <- ifelse(dd < 1, dd*1000, dd) #if less than 1km, convert to meters 
      res.pix <- ifelse(dd < 1, 100, 1) #if dd less than 1km, resolution goes to 100meters otherwise stays at 1km
      block.use <- 1 #max(1, floor(dd.use/(res.pix*10))) #block size needs to be 1 for conditions to work
      config.tplate$val[config.tplate$param == "radius"] <- dd.use/res.pix  #in pixels 
      config.tplate$val[config.tplate$param == "block_size"] <- block.use
      config.tplate <- paste(config.tplate$param, config.tplate$V2, config.tplate$val)
      
      write.table(config.tplate, here::here(paste0("data/derived-data/OmniscapeParamFiles/IniFile_Transient_",g, "_GroupID_", c, "_TransfoCoef_", transfocoef,  "_SuitThreshold_", sourcethre,"_DispDist_", dd,"km",
                                                   '_', scen.time, '_', scen.ssp, '_', scen.gcm, ".ini")),
                  quote = F, row.names = F, col.names = F)
      
    }
  }
}

# Generate the file listing all parameter combination (without the normalized flow), useful for batch running on distance cluster
full.param$jobID <- c(1:nrow(full.param))
full.param <- full.param[, c("jobID","Group", "clusID", "TransfoCoef", "SourceThre", "DD", "TimePeriod", "SSP", "GCM")]
full.param$DD <- as.character(full.param$DD)
full.param <- as.matrix(full.param)
write.table(full.param, here::here('data/derived-data/BatchRun/list-of-params-for-batchrun-step1_transient.txt'), row.names = F, col.names = F, quote = F)

# Generate the file listing all parameter combination (with the normalized flow), useful for batch running on distance cluster
full.param.full$jobID <- c(1:nrow(full.param.full))
full.param.full <- full.param.full[, c("jobID","Group", "clusID", "TransfoCoef", "SourceThre", "DD", "NormFlowThre", "TimePeriod", "SSP", "GCM")]
full.param.full$DD <- as.character(full.param.full$DD)
full.param.full <- as.matrix(full.param.full)
write.table(full.param.full, here::here('data/derived-data/BatchRun/list-of-params-for-batchrun-step2_transient.txt'), row.names = F, col.names = F, quote = F)#########################################################################################################################

#########################################################################################################################
######################## Omniscape needs to be run in Julia for both steady and transient states ########################
#  ==>     ClusterBatchRun_JuliaScript_OmniscapeRun.jl and ClusterBatchRun_JuliaScript_OmniscapeRun_Transient.jl
#########################################################################################################################
#########################################################################################################################

#####################################################################################################################
######################## 3. Delineate ecological continuities from Omniscape outputs ################################
#####################################################################################################################
# The delineation of ecological continuities are located in the /outputs/EcologicalContinuities/ folder

# Required general dataset 
transient = F #to run for T and F 
if (transient) {
  lst.param <- read.table(here::here('data/derived-data/BatchRun/list-of-params-for-batchrun-step1_transient.txt')) #table of all parameter combinations 
} else {
  lst.param <- read.table(here::here('data/derived-data/BatchRun/list-of-params-for-batchrun-step1.txt')) #table of all parameter combinations 
}

fnorm <- seq(0.7, 0.9, by = 0.1)

for (i in 1:nrow(lst.param)) { #loop on each parameter combination, preferably run on a distant cluster each combination in parallel 
  
  for (f in fnorm) {
    if (transient) {
  norm.map <- terra::rast(here::here(paste0('data/derived-data/OmniscapeOutput/OmniscapeOutput_Transient_', lst.param[i, 2], '_GroupID_', lst.param[i, 3], '_TransfoCoef_', lst.param[i, 4],
                                            '_SuitThreshold_', lst.param[i, 5], '_DispDist_', lst.param[i, 6], '_', lst.param[i, 7], '_', lst.param[i, 8], '_', lst.param[i, 9], '/normalized_cum_currmap.tif')))
  flow.pot <- terra::rast(here::here(paste0('data/derived-data/OmniscapeOutput/OmniscapeOutput_Transient_', lst.param[i, 2], '_GroupID_', lst.param[i, 3], '_TransfoCoef_', lst.param[i, 4],
                                            '_SuitThreshold_', lst.param[i, 5], '_DispDist_', lst.param[i, 6], '_', lst.param[i, 7], '_', lst.param[i, 8], '_', lst.param[i, 9], '/flow_potential.tif')))
    } else {
      norm.map <- terra::rast(here::here(paste0('data/derived-data/OmniscapeOutput/OmniscapeOutput_', lst.param[i, 2], '_GroupID_', lst.param[i, 3], '_TransfoCoef_', lst.param[i, 4],
                                                '_SuitThreshold_', lst.param[i, 5], '_DispDist_', lst.param[i, 6], '_', lst.param[i, 7], '_', lst.param[i, 8], '_', lst.param[i, 9],  '/normalized_cum_currmap.tif')))
      flow.pot <- terra::rast(here::here(paste0('data/derived-data/OmniscapeOutput/OmniscapeOutput_', lst.param[i, 2], '_GroupID_', lst.param[i, 3], '_TransfoCoef_', lst.param[i, 4],
                                                '_SuitThreshold_', lst.param[i, 5], '_DispDist_', lst.param[i, 6], '_', lst.param[i, 7], '_', lst.param[i, 8], '_', lst.param[i, 9],  '/flow_potential.tif')))
    }
  # Double thresholding to identify EC delineation 
  vals <- na.omit(terra::values(flow.pot))
  vals <- vals[vals!= 0]
  vals <- quantile(vals, probs = 0.05)
  idx <- terra::values(flow.pot) < vals
  
  norm.map[idx] <- 0
  norm.map[norm.map < f] <- 0 
  norm.map[norm.map >= f] <- 1 
  
  # Polygon disaggregation and saving (shapefile and raster formats)
  poly <- terra::as.polygons(norm.map, dissolve = T)
  poly <- poly[poly$normalized_cum_currmap == 1]
  poly <- terra::disagg(poly)
  
  if (length(poly) > 0) {
    poly$corridorID <- c(1:length(poly))
    
    if (transient) {
      terra::writeVector(poly, here::here(paste0('outputs/EcologicalContinuities/Vector/EcologicalContinuities_Transient_', lst.param[i, 2], '_GroupID_', lst.param[i, 3], '_TransfoCoef_', lst.param[i, 4],
                                                 '_SuitThreshold_', lst.param[i, 5], '_DispDist_', lst.param[i, 6], 'km_NormFlowThreshold_', f, '_', lst.param[i, 7], '_', lst.param[i, 8], '_', lst.param[i, 9],  '.gpkg')), filetype = 'ESRI Shapefile', overwrite = T)
      
      terra::writeRaster(norm.map, here::here(paste0('outputs/EcologicalContinuities/Raster/EcologicalContinuities_Transient_', lst.param[i, 2], '_GroupID_', lst.param[i, 3], '_TransfoCoef_', lst.param[i, 4],
                                                     '_SuitThreshold_', lst.param[i, 5], '_DispDist_', lst.param[i, 6], 'km_NormFlowThreshold_', f, '_', lst.param[i, 7], '_', lst.param[i, 8], '_', lst.param[i, 9], '.tif')), overwrite = T)
      
    } else {
    
    terra::writeVector(poly, here::here(paste0('outputs/EcologicalContinuities/Vector/EcologicalContinuities_', lst.param[i, 2], '_GroupID_', lst.param[i, 3], '_TransfoCoef_', lst.param[i, 4],
                                               '_SuitThreshold_', lst.param[i, 5], '_DispDist_', lst.param[i, 6], 'km_NormFlowThreshold_', f, '_', lst.param[i, 7], '_', lst.param[i, 8], '_', lst.param[i, 9],  '.gpkg')), filetype = 'ESRI Shapefile', overwrite = T)
    
    terra::writeRaster(norm.map, here::here(paste0('outputs/EcologicalContinuities/Raster/EcologicalContinuities_', lst.param[i, 2], '_GroupID_', lst.param[i, 3], '_TransfoCoef_', lst.param[i, 4],
                                                   '_SuitThreshold_', lst.param[i, 5], '_DispDist_', lst.param[i, 6], 'km_NormFlowThreshold_', f, '_', lst.param[i, 7], '_', lst.param[i, 8], '_', lst.param[i, 9], '.tif')), overwrite = T)
  
    }
  }
  }
}
