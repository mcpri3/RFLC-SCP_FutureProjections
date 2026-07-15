############################################################################################################################################################
# This script 1. calculates resistance and source rasters 
############################################################################################################################################################

#####################################################################################################################
###################################### Prelim. Output folder creation  ##############################################
#####################################################################################################################
# Create the folder of source layers if does not exist 
if (!dir.exists(here::here('data/derived-data/SourceLayers/'))) {
  dir.create(here::here('data/derived-data/SourceLayers/'))
}
if (!dir.exists(here::here('data/derived-data/SourceLayers/100m'))) {
  dir.create(here::here('data/derived-data/SourceLayers/100m'))
}
# Create the folder of resistance layers data if does not exist 
if (!dir.exists(here::here('data/derived-data/ResistanceSurfaces/'))) {
  dir.create(here::here('data/derived-data/ResistanceSurfaces/'))
}
if (!dir.exists(here::here('data/derived-data/ResistanceSurfaces/100m'))) {
  dir.create(here::here('data/derived-data/ResistanceSurfaces/100m'))
}
# Create the folder of condition layers data if does not exist 
if (!dir.exists(here::here('data/derived-data/ConditionLayers/'))) {
  dir.create(here::here('data/derived-data/ConditionLayers/'))
}
if (!dir.exists(here::here('data/derived-data/ConditionLayers/100m'))) {
  dir.create(here::here('data/derived-data/ConditionLayers/100m'))
}
# #####################################################################################################################
# ############################## 1. Calculation of resistance and source maps per group ###############################
# #####################################################################################################################
# Set up parallel processing
plan(multisession, workers = parallel::detectCores() - 1)

# Read clustering scheme
clus.scheme <- read.xlsx(here('data/derived-data/FunctionalGroups/List-of-clustering-schemes.xlsx'))
timestep = 5

# Define scenario grid
scen <- expand.grid(
  time = seq(2020, 2050, by = timestep), 
  ssp = c('ssp1', 'ssp3'), 
  gcm = c('gfdl-esm4', 'mpi-esm1-2-hr', 'ukesm1-0-ll')
)

# Thresholds and transformation coefficients
p.threshold <- seq(0.5, 0.6, by = 0.1)
coef.c <- c(2, 4, 8)

# Initialize removal tracking
full.rm <- list()

# Parallelized loop over functional groups
full.rm <- future_lapply(seq_len(nrow(clus.scheme)), function(gpe) {
  cat("Processing group:", gpe, "\n")
  
  group_name <- clus.scheme$group[gpe]
  nclus <- clus.scheme$Nclus[gpe]
  
  # Read species list
  species_file <- here(sprintf(
    'data/derived-data/FunctionalGroups/Species-list_withTraits_%s_GroupID_K=%d.xlsx',
    group_name, nclus
  ))
  sp <- read.xlsx(species_file)
  
  # Fix species name typo
  sp$SPECIES_NAME_SYNONYM[sp$SPECIES_NAME_SYNONYM == 'Phylloscopus sibillatrix'] <- 'Phylloscopus sibilatrix'
  sp <- sp[sp$SPECIES_NAME_SYNONYM != "Pelophylax grafi",]
  
  group_rm <- list()
  
  for (i in unique(sp$cluster.id)) {
    sp.i <- sp$SPECIES_NAME_SYNONYM[sp$cluster.id == i]
    dd.min <- min(sp$DISPERSAL_KM[sp$cluster.id == i])
    
    # Iterate over scenarios
    for (row in seq_len(nrow(scen))) {
      scen.i <- scen[row, ]
      
      # Generate file name
      file_name <- sprintf('%s_%s_%d_%s.tif', scen.i$ssp, scen.i$gcm, scen.i$time, gsub(' ', '_', sp.i))
      sdm_path <- here(sprintf('data/derived-data/DistributionMaps/France/TemporallySmoothed_&_Masked/%s', file_name))
      
      # Load and compute median raster
      sdm <- median(rast(sdm_path))
      
      # Process source layers
      for (p in p.threshold) {
        srce <- sdm
        srce[srce < p] <- 0
        
        if (sum(na.omit(values(srce))) == 0) {
          group_rm <- append(group_rm, list(data.frame(taxa = group_name, gp = i, p.thre = p)))
        }
        
        # Write Source Map
        out_path <- here(sprintf(
          'data/derived-data/SourceLayers/SourceLayer_%s_GroupID_%d_SuitThreshold_%.1f_%d_%s_%s.tif',
          group_name, i, p, scen.i$time, scen.i$ssp, scen.i$gcm
        ))
        writeRaster(srce, out_path, overwrite = TRUE)
        
        # Disaggregate if necessary
        if (dd.min < 1) {
          writeRaster(disagg(srce, fact = 10), gsub('SourceLayers/', 'SourceLayers/100m/', out_path), overwrite = TRUE)
        }
      }
      
      # Process resistance layers
      for (c in coef.c) {
        res <- 100 - 99 * (1 - exp(-c * sdm)) / (1 - exp(-c))
        
        res_path <- here(sprintf(
          'data/derived-data/ResistanceSurfaces/ResistanceSurface_%s_GroupID_%d_TransfoCoef_%d_%d_%s_%s.tif',
          group_name, i, c, scen.i$time, scen.i$ssp, scen.i$gcm
        ))
        writeRaster(res, res_path, overwrite = TRUE)
        
        # Disaggregate if necessary
        if (dd.min < 1) {
          writeRaster(disagg(res, fact = 10), gsub('ResistanceSurfaces/', 'ResistanceSurfaces/100m/', res_path), overwrite = TRUE)
        }
      }
    }
  }
  
  # Return list of species to remove for this group
  do.call(rbind, group_rm)
})

# Combine removal lists
full.rm <- do.call(rbind, full.rm)

# Save removal list
write.xlsx(full.rm, here('data/derived-data/SourceLayers/GroupToRemove-noSource.xlsx'))

# #####################################################################################################################
# ############################## 2. For transient state, calculates mean resistance maps ###############################
# #####################################################################################################################
# Read clustering scheme
clus.scheme <- read.xlsx(here('data/derived-data/FunctionalGroups/List-of-clustering-schemes.xlsx'))
timestep = 5

# Define scenario grid
scen <- expand.grid(
  time = seq(2020, 2045, by = timestep), 
  ssp = c('ssp1', 'ssp3'), 
  gcm = c('gfdl-esm4', 'mpi-esm1-2-hr', 'ukesm1-0-ll')
)

# Thresholds and transformation coefficients
coef.c <- c(2, 4, 8)

for (gpe in 1:nrow(clus.scheme)) {
  print(gpe)
  group_name <- clus.scheme$group[gpe]
  nclus <- clus.scheme$Nclus[gpe]
  
  # Read species list
  species_file <- here(sprintf(
    'data/derived-data/FunctionalGroups/Species-list_withTraits_%s_GroupID_K=%d.xlsx',
    group_name, nclus
  ))
  sp <- read.xlsx(species_file)
  
  for (i in unique(sp$cluster.id)) {
    
    dd.min <- min(sp$DISPERSAL_KM[sp$cluster.id == i])
    
    for (row in seq_len(nrow(scen))) {
      scen.i <- scen[row, ]
      
      for (c in coef.c) {
        
        # Generate file name
        res_path <- here::here(paste0('data/derived-data/ResistanceSurfaces/ResistanceSurface_', group_name, '_GroupID_', i, '_TransfoCoef_', c, '_', scen.i$time, '_', scen.i$ssp, '_', scen.i$gcm,'.tif'))
        res_path.next <- here::here(paste0('data/derived-data/ResistanceSurfaces/ResistanceSurface_', group_name, '_GroupID_', i, '_TransfoCoef_', c, '_', scen.i$time + timestep, '_', scen.i$ssp, '_', scen.i$gcm,'.tif'))
        
        # Load and compute median raster
        res <- mean(rast(c(res_path, res_path.next)))
        terra::writeRaster(res, 
                           here::here(paste0('data/derived-data/ResistanceSurfaces/ResistanceSurface_', group_name, '_GroupID_', i, '_TransfoCoef_', c, '_', scen.i$time+timestep/2, '_', scen.i$ssp, '_', scen.i$gcm,'.tif')),
                           overwrite = TRUE)
        
        # Disaggregate if necessary
        if (dd.min < 1) {
          writeRaster(disagg(res, fact = 10),
                      here::here(paste0('data/derived-data/ResistanceSurfaces/100m/ResistanceSurface_', group_name, '_GroupID_', i, '_TransfoCoef_', c, '_', scen.i$time+timestep/2, '_', scen.i$ssp, '_', scen.i$gcm,'.tif')),
                      overwrite = TRUE)
        }
        
      }
    }
  }
}

# #####################################################################################################################
# ############################## 3. For transient state, calculates source and condition maps #########################
# #####################################################################################################################
# Read clustering scheme
clus.scheme <- read.xlsx(here('data/derived-data/FunctionalGroups/List-of-clustering-schemes.xlsx'))
timestep = 5

# Define scenario grid
scen <- expand.grid(
  time = seq(2020, 2045, by = timestep), 
  ssp = c('ssp1', 'ssp3'), 
  gcm = c('gfdl-esm4', 'mpi-esm1-2-hr', 'ukesm1-0-ll')
)

# Thresholds and transformation coefficients
p.threshold <- seq(0.5, 0.6, by = 0.1)

for (gpe in 1:nrow(clus.scheme)) {
  print(gpe)
  group_name <- clus.scheme$group[gpe]
  nclus <- clus.scheme$Nclus[gpe]
  
  # Read species list
  species_file <- here(sprintf(
    'data/derived-data/FunctionalGroups/Species-list_withTraits_%s_GroupID_K=%d.xlsx',
    group_name, nclus
  ))
  sp <- read.xlsx(species_file)
  
  
  for (i in unique(sp$cluster.id)) {
    
    dd.min <- min(sp$DISPERSAL_KM[sp$cluster.id == i])
    
    for (row in seq_len(nrow(scen))) {
      scen.i <- scen[row, ]
      
      for (p in p.threshold) {
        
        # Generate file name
        srce_path <- here::here(paste0('data/derived-data/SourceLayers/SourceLayer_', group_name, '_GroupID_', i, '_SuitThreshold_', p, '_', scen.i$time, '_', scen.i$ssp, '_', scen.i$gcm,'.tif'))
        srce_path.next <- here::here(paste0('data/derived-data/SourceLayers/SourceLayer_', group_name, '_GroupID_', i, '_SuitThreshold_', p, '_', scen.i$time + timestep, '_', scen.i$ssp, '_', scen.i$gcm,'.tif'))
        
        # Load and compute median raster
        srce <- max(rast(c(srce_path, srce_path.next)))
        condition.curr <- rast(srce_path)
        condition.fut <- rast(srce_path.next)
        
        idx.curr <- condition.curr > 0 & condition.fut == 0
        idx.both <- condition.curr > 0 & condition.fut > 0
        idx.fut <- condition.curr == 0 & condition.fut > 0
        
        condition.curr[idx.curr] <- 0
        condition.curr[idx.both] <- 0.5
        condition.curr[idx.fut] <- 1.5
        
        condition.fut[idx.curr] <- 0 
        condition.fut[idx.both] <- 1
        condition.fut[idx.fut] <- 1.5
        
        terra::writeRaster(srce,
                           here::here(paste0('data/derived-data/SourceLayers/SourceLayer_', group_name, '_GroupID_', i, '_SuitThreshold_', p, '_', scen.i$time + timestep/2, '_', scen.i$ssp, '_', scen.i$gcm,'.tif')),
                           overwrite = TRUE)
        terra::writeRaster(condition.curr, 
                           here::here(paste0('data/derived-data/ConditionLayers/ConditionLayerCurrent_', group_name, '_GroupID_', i, '_SuitThreshold_', p, '_', scen.i$time + timestep/2, '_', scen.i$ssp, '_', scen.i$gcm,'.tif')),
                           overwrite = TRUE)
        terra::writeRaster(condition.fut,
                           here::here(paste0('data/derived-data/ConditionLayers/ConditionLayerFuture_', group_name, '_GroupID_', i, '_SuitThreshold_', p, '_', scen.i$time + timestep/2, '_', scen.i$ssp, '_', scen.i$gcm,'.tif')),
                           overwrite = TRUE)

        # Disaggregate if necessary
        if (dd.min < 1) {
          
          writeRaster(disagg(srce, fact = 10),
                      here::here(paste0('data/derived-data/SourceLayers/100m/SourceLayer_', group_name, '_GroupID_', i, '_SuitThreshold_', p, '_', scen.i$time + timestep/2, '_', scen.i$ssp, '_', scen.i$gcm,'.tif')),
                      overwrite = TRUE)
          writeRaster(disagg(condition.curr, fact = 10),
                      here::here(paste0('data/derived-data/ConditionLayers/100m/ConditionLayerCurrent_', group_name, '_GroupID_', i, '_SuitThreshold_', p, '_', scen.i$time + timestep/2, '_', scen.i$ssp, '_', scen.i$gcm,'.tif')),
                      overwrite = TRUE)
          writeRaster(disagg(condition.fut, fact = 10),
                      here::here(paste0('data/derived-data/ConditionLayers/100m/ConditionLayerFuture_', group_name, '_GroupID_', i, '_SuitThreshold_', p, '_', scen.i$time + timestep/2, '_', scen.i$ssp, '_', scen.i$gcm,'.tif')),
                      overwrite = TRUE)
        }
        
      }
    }
  }
}
