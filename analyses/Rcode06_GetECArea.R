
# Transient state 
lst.param <- read.table(here::here('data/derived-data/BatchRun/list-of-params-for-batchrun-step2_transient.txt'))
lst.param <- paste0('EcologicalContinuities_Transient_', lst.param[, 2], '_GroupID_', lst.param[, 3], '_TransfoCoef_', 
                    lst.param[, 4], '_SuitThreshold_', lst.param[, 5], '_DispDist_', lst.param[, 6], 'km_NormFlowThreshold_', 
                    lst.param[, 7], '_' , lst.param[, 8], '_' , lst.param[, 9], '_', lst.param[, 10],'.tif')


library(doParallel)
registerDoParallel(cores=20)

all.area <- foreach::foreach(l=lst.param) %dopar% {
  rr <- terra::rast(here::here(paste0('outputs/EcologicalContinuities/Raster/', l)))
  area <- sum(na.omit(terra::values(rr)))
  return(data.frame(file = l, area.km2 = area))
}
all.area <- do.call(rbind, all.area)
all.area$file <- gsub('.tif', '', all.area$file, fixed = T)
all.area$file <- gsub('EcologicalContinuities_Transient_', '', all.area$file, fixed = T)
nme <- strsplit(all.area$file, '_')
nme <- lapply(nme, function(x){
  return(data.frame(Class = x[1], GroupID = x[3], TransfoCoef = x[5], SuitThreshold = x[7],
                    DispDist = x[9], NormFlowThreshold = x[11], Time = x[12], ssp = x[13], gcm = x[14]))
} )
nme <- do.call(rbind, nme)
all.area <- cbind(nme, all.area)
all.area.trans <-  all.area[, colnames(all.area) != 'file' ]

# Steady state
lst.param <- read.table(here::here('data/derived-data/BatchRun/list-of-params-for-batchrun-step2.txt'))
lst.param <- paste0('EcologicalContinuities_', lst.param[, 2], '_GroupID_', lst.param[, 3], '_TransfoCoef_', 
                    lst.param[, 4], '_SuitThreshold_', lst.param[, 5], '_DispDist_', lst.param[, 6], 'km_NormFlowThreshold_', 
                    lst.param[, 7], '_' , lst.param[, 8], '_' , lst.param[, 9], '_', lst.param[, 10],'.tif')

all.area <- foreach::foreach(l=lst.param) %dopar% {
  rr <- terra::rast(here::here(paste0('outputs/EcologicalContinuities/Raster/', l)))
  area <- sum(na.omit(terra::values(rr)))
  return(data.frame(file = l, area.km2 = area))
}

all.area <- do.call(rbind, all.area)
all.area$file <- gsub('.tif', '', all.area$file, fixed = T)
all.area$file <- gsub('EcologicalContinuities_', '', all.area$file, fixed = T)
nme <- strsplit(all.area$file, '_')
nme <- lapply(nme, function(x){
  return(data.frame(Class = x[1], GroupID = x[3], TransfoCoef = x[5], SuitThreshold = x[7],
                    DispDist = x[9], NormFlowThreshold = x[11], Time = x[12], ssp = x[13], gcm = x[14]))
} )
nme <- do.call(rbind, nme)
all.area <- cbind(nme, all.area)
all.area.steady <-  all.area[, colnames(all.area) != 'file' ]

all.area.trans$Time <- as.numeric(all.area.trans$Time) + 2.5
all.area.trans$TYPE <- 'Transient'
all.area.steady$TYPE <- 'Steady'
all.area <- rbind(all.area.steady, all.area.trans)
openxlsx::write.xlsx(all.area, file = here::here('outputs/Indicators/EcologicalContinuitiesArea.xlsx'))
