setwd('/bettik/primam/RFLC-SCP_FutureProjections')

library(doParallel)
# ----------------------------------------
# Read command-line arguments (The code is written for one combination of parameters)
# ----------------------------------------
args <- commandArgs(trailingOnly=TRUE) ## get group parameters
g <- as.character(args[1]) 
c <- as.numeric(args[2]) 
transfo.r <- as.numeric(args[3]) 
suit.p <- as.numeric(args[4]) 
dd <- as.numeric(args[5]) 
fnorm <- as.numeric(args[6])
time <- as.numeric(args[7])
ssp <- as.character(args[8])
gcm <- as.character(args[9])
transient <- as.logical(args[10]) # Ensure transient is treated as a boolean

all.ep <- sf::st_read(here::here('./data/raw-data/ProtectedAreas/AICHI_STRICT_NOTSTRICT_NO-OVERLAP.shp'))

# read corridor delimitation 
all.files <- list.files(here::here('outputs/EcologicalContinuities/Vector/'))

if (transient) {
ffile <- all.files[grep(paste0('EcologicalContinuities_Transient_', g, '_GroupID_', c, '_TransfoCoef_', transfo.r,
                               '_SuitThreshold_', suit.p, '_DispDist_', dd, 'km_NormFlowThreshold_', fnorm, '_', time, '_', ssp, '_', gcm, '.gpkg'), all.files)] 
} else {
ffile <- all.files[grep(paste0('EcologicalContinuities_', g, '_GroupID_', c, '_TransfoCoef_', transfo.r,
                                 '_SuitThreshold_', suit.p, '_DispDist_', dd, 'km_NormFlowThreshold_', fnorm, '_', time, '_', ssp, '_', gcm, '.gpkg'), all.files)] 
}

if (length(ffile) != 0) {
  
  corrid <- sf::st_read(here::here(paste0('outputs/EcologicalContinuities/Vector/', ffile ))) 
  corrid$corrid.area.km2 <- sf::st_area(corrid)/1000000
  area.corrid <- sum(corrid$corrid.area.km2)
  
  # Get corridor - PAs intersection
  if (sf::st_crs(all.ep) != sf::st_crs(corrid)) {
    all.ep <- sf::st_transform(all.ep, sf::st_crs(corrid))
  }
  
  indic.con <- foreach::foreach(type = unique(all.ep$TYPE)) %dopar% {
    subinter <- sf::st_intersection(all.ep[all.ep$TYPE %in% type,], corrid)
    if (nrow(subinter) > 0) {
    perc.over <- as.numeric(sum(sf::st_area(subinter)/1000000)/ area.corrid)*100
    } else {
      perc.over <- 0
    }
    return(data.frame(Type = type, Class = unique(all.ep$CLASS[all.ep$TYPE %in% type]), Perc.overlap.corrid = perc.over))
  }
 indic.con <- do.call(rbind, indic.con)

 } else {
  # Get corridor - PAs intersection 
  indic.con <- dplyr::distinct(sf::st_drop_geometry(all.ep[, c('TYPE', 'CLASS')]))
  colnames(indic.con) <- stringr::str_to_title(colnames(indic.con))
  indic.con$Perc.overlap.corrid <- 0 
}

# Save output
output_file <- paste0("outputs/Indicators/PercOverlap-EC-PAs_", ifelse(transient, "Transient_", ""), 
                      g, "_GroupID_", c, "_TransfoCoef_", transfo.r, "_SuitThreshold_", suit.p,
                      "_DispDist_", dd, "km_NormFlowThreshold_", fnorm, "_", time, "_", ssp, "_", gcm)

saveRDS(indic.con, here::here(output_file))
