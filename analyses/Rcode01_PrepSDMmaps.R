#####################################################################################################################
###################################### Prelim. Output folder creation  ##############################################
#####################################################################################################################
# To stock maps from Sara 
dir.create(here::here('data/raw-data/DistributionMaps'))
dir.create(here::here('data/raw-data/DistributionMaps/Current'))
dir.create(here::here('data/raw-data/DistributionMaps/Future/'))
dir.create(here::here('data/raw-data/DistributionMaps/Future/ssp1_gfdl-esm4'))
dir.create(here::here('data/raw-data/DistributionMaps/Future/ssp1_mpi-esm1-2-hr'))
dir.create(here::here('data/raw-data/DistributionMaps/Future/ssp1_ukesm1-0-ll'))
dir.create(here::here('data/raw-data/DistributionMaps/Future/ssp3_gfdl-esm4'))
dir.create(here::here('data/raw-data/DistributionMaps/Future/ssp3_mpi-esm1-2-hr'))
dir.create(here::here('data/raw-data/DistributionMaps/Future/ssp3_ukesm1-0-ll'))

# To stock modified maps 
dir.create(here::here('data/derived-data/DistributionMaps/'))
dir.create(here::here('data/derived-data/DistributionMaps/France'))
dir.create(here::here('data/derived-data/DistributionMaps/France/Current'))
dir.create(here::here('data/derived-data/DistributionMaps/France/Future/'))
dir.create(here::here('data/derived-data/DistributionMaps/France/Future/ssp1_gfdl-esm4'))
dir.create(here::here('data/derived-data/DistributionMaps/France/Future/ssp1_mpi-esm1-2-hr'))
dir.create(here::here('data/derived-data/DistributionMaps/France/Future/ssp1_ukesm1-0-ll'))
dir.create(here::here('data/derived-data/DistributionMaps/France/Future/ssp3_gfdl-esm4'))
dir.create(here::here('data/derived-data/DistributionMaps/France/Future/ssp3_mpi-esm1-2-hr'))
dir.create(here::here('data/derived-data/DistributionMaps/France/Future/ssp3_ukesm1-0-ll'))
dir.create(here::here('data/derived-data/DistributionMaps/France/TemporallySmoothed/'))
dir.create(here::here('data/derived-data/DistributionMaps/France/TemporallySmoothed_&_Masked/'))
dir.create(here::here('data/derived-data/DistributionMaps/France/TemporallySmoothed_&_Masked_PerGroup/'))
dir.create(here::here('data/derived-data/LUMM/'))

#######################################
# Copy species maps from Sara work 
#######################################
lst.vert <- read.csv('/Volumes/T5 EVO/Vertebrates/deliverables/vertebrates_checklist.csv')
mam <- openxlsx::read.xlsx('./data/raw-data/FunctionalGroups/VertebrateSpecies-list_withTraits_Mammalia_GroupID_K=11.xlsx')
ave <- openxlsx::read.xlsx('./data/raw-data/FunctionalGroups/VertebrateSpecies-list_withTraits_Aves_GroupID_K=19.xlsx')
amp <- openxlsx::read.xlsx('./data/raw-data/FunctionalGroups/VertebrateSpecies-list_withTraits_Amphibia_GroupID_K=4.xlsx')
rep <- openxlsx::read.xlsx('./data/raw-data/FunctionalGroups/VertebrateSpecies-list_withTraits_Reptilia_GroupID_K=3.xlsx')
mylst <- rbind(mam, ave, amp, rep)

sum(mylst$SPECIES_NAME %in% lst.vert$SpeciesName)
sum(mylst$SPECIES_NAME_SYNONYM %in% lst.vert$SpeciesName)
idx <- !mylst$SPECIES_NAME_SYNONYM %in% lst.vert$SpeciesName
mylst$SPECIES_NAME_SYNONYM[idx]
mylst$SPECIES_NAME_SYNONYM[mylst$SPECIES_NAME_SYNONYM == 'Phylloscopus sibillatrix'] <- 'Phylloscopus sibilatrix'
sp.lst <- mylst$SPECIES_NAME_SYNONYM[mylst$SPECIES_NAME_SYNONYM %in% lst.vert$SpeciesName] 

# Check for the methods of the paper how pseudo-absences were obtained 
method <- read.csv(here::here('data/raw-data/DistributionMaps/range_constraint_params.csv'))
method <- method[method$Name %in% mylst$SPECIES_NAME_SYNONYM,]

all.current <- list.files('/Volumes/T5 EVO/Vertebrates/deliverables/current/ca_2/', recursive = T) # CA unconstrained; see raw-data folder for CA constrained (zip file transmitted by Sara)

for (s in sp.lst) {
  s <- gsub(' ', '_', s) 
  ffile <- all.current[grep(s, all.current)]
  file.copy(paste0('/Volumes/T5 EVO/Vertebrates/deliverables/current/ca_2/', ffile), here::here('data/raw-data/DistributionMaps/Current/ca/')) 
  # file.copy(paste0('/Volumes/T5 EVO/Vertebrates/deliverables/ssp1_gfdl-esm4/ca_2/', ffile), here::here('data/raw-data/DistributionMaps/Future/ssp1_gfdl-esm4/'))
  # file.copy(paste0('/Volumes/T5 EVO/Vertebrates/deliverables/ssp1_mpi-esm1-2-hr/ca_2/', ffile), here::here('data/raw-data/DistributionMaps/Future/ssp1_mpi-esm1-2-hr/'))
  # file.copy(paste0('/Volumes/T5 EVO/Vertebrates/deliverables/ssp1_ukesm1-0-ll/ca_2/', ffile), here::here('data/raw-data/DistributionMaps/Future/ssp1_ukesm1-0-ll/'))
  # file.copy(paste0('/Volumes/T5 EVO/Vertebrates/deliverables/ssp3_gfdl-esm4/ca_2/', ffile), here::here('data/raw-data/DistributionMaps/Future/ssp3_gfdl-esm4/'))
  # file.copy(paste0('/Volumes/T5 EVO/Vertebrates/deliverables/ssp3_mpi-esm1-2-hr/ca_2/', ffile), here::here('data/raw-data/DistributionMaps/Future/ssp3_mpi-esm1-2-hr/'))
  # file.copy(paste0('/Volumes/T5 EVO/Vertebrates/deliverables/ssp3_ukesm1-0-ll/ca_2/', ffile), here::here('data/raw-data/DistributionMaps/Future/ssp3_ukesm1-0-ll/'))
}

###############################
# Change raster name for future
###############################
sp.code <- readr::read_csv("data/raw-data/HabitatPreference/nc_vertebrate_list.csv")
sp.lst <- list.files(here::here('data/raw-data/DistributionMaps/Current/ca_constrained/'))
sp.lst <- gsub('_raw', '', sp.lst)
sp.lst <- gsub('.tif', '', sp.lst)
sp.lst <- gsub('_', ' ', sp.lst)
sp.code <- sp.code[sp.code$SpeciesName %in% sp.lst,]
all.files <- list.files(here::here('data/raw-data/DistributionMaps/Future/'), recursive = T)
for (s in sp.code$Code) {
  files.s <- all.files[grep(s, all.files, fixed = T)]
  nme <- sp.code$SpeciesName[sp.code$Code %in% s]
  nme <- gsub(' ', '_', nme)
  file.rename(here::here(paste0('data/raw-data/DistributionMaps/Future/', files.s)), 
              here::here(paste0('data/raw-data/DistributionMaps/Future/', gsub(s, nme, files.s, fixed = T))))
}

##########################
# Crop species maps  
##########################
grid <- sf::st_read(here::here('data/raw-data/Grids/ReferenceGrid_France_bin_1000m.gpkg'))
grid.c <- sf::st_centroid(grid)
rr <- terra::rast(here::here('data/raw-data/Grids/ReferenceGrid_France_bin_1000m.tif'))

todo <- list.files(here::here('data/raw-data/DistributionMaps/'), recursive = T)
todo <- todo[grep('Current/ca/', todo, fixed = T)] #toupdate depending on what you want to crop 
# todo <- todo[-grep('Current', todo, fixed = T)]
# todo <- todo[-grep('REP', todo, fixed = T)]
# todo <- todo[-grep('AVE', todo, fixed = T)]
# todo <- todo[-grep('AMP', todo, fixed = T)]
# todo <- todo[-grep('MAM', todo, fixed = T)]
# todo <- todo[-grep('.xml', todo, fixed = T)]

foreach(f = todo) %dopar% { 
  map <- terra::rast(here::here(paste0('data/raw-data/DistributionMaps/', f)))
  grid.c <- sf::st_transform(grid.c, sf::st_crs(map))
  val <- terra::extract(map, grid.c)
  grid.c$val <- val[, 2]
  grid.c <- sf::st_transform(grid.c, sf::st_crs(rr))
  rr <- terra::rasterize(grid.c, rr, field = 'val')
  nme <- gsub('ca/', '', f, fixed = T)
  nme <- gsub('ca_constrained/', '', nme, fixed = T)
  nme <- gsub('_raw.tif', '', nme, fixed = T)
  nme <- gsub('_ca.tif', '', nme, fixed = T)
  terra::writeRaster(rr, here::here(paste0('data/derived-data/DistributionMaps/France/', nme)), overwrite = T)
} 

##########################
# Species maps ramping 
##########################
sp.lst <- list.files(here::here('data/derived-data/DistributionMaps/France/Current/ca'))
# past.ref <- 2005
# future.ref <- 2055
# ramping.ref <- seq(2020,2050, by = 5)
past.ref <- 0
future.ref <- 50
ramping.ref <- seq(15, 45, by = 5)

foreach(s = sp.lst) %dopar% {
  print(s)
  foreach(scen=c('ssp1_gfdl-esm4', 'ssp1_mpi-esm1-2-hr', 'ssp1_ukesm1-0-ll',
                   'ssp3_gfdl-esm4', 'ssp3_mpi-esm1-2-hr', 'ssp3_ukesm1-0-ll')) %dopar% {
  
  current <- terra::rast(here::here(paste0('data/derived-data/DistributionMaps/France/Current/ca/', s)))
  final <- terra::rast(here::here(paste0('data/derived-data/DistributionMaps/France/Future/',scen, '/', s)))
  
  # annual.rate <- (final - current)/(future.ref - past.ref) #linear trend
  # foreach(r = ramping.ref) %do% {
  #   ramped <- current + annual.rate*(r-past.ref)
  #   terra::writeRaster(ramped, here::here(paste0('data/derived-data/DistributionMaps/France/TemporallySmoothed/', scen, '_', r, '_', s)), overwrite = T)
  #  }

  # exponential trend 
  a = current
  a[current > final] <- -1 #expo decay
  a[current < final] <- 1 #expo increase 
  a[current == final] <- 0 #no change
  
  c = current - a
  b = log((current-c)*(final-c))/(past.ref + future.ref)

  foreach(r = ramping.ref) %do% {
    
    ramped <- a*exp(b*r) + c
    terra::writeRaster(ramped, here::here(paste0('data/derived-data/DistributionMaps/France/TemporallySmoothed/', scen, '_', r + 2005, '_', s)), overwrite = T)
  }
  }
}


##############################
# Preparing land system maps 
##############################
todo <- seq(5, 30, by = 5)
origin <- terra::rast(here::here('data/raw-data/LUMM/eu_lum_map_12_feb_clip_reclassified.tif'))
grid <- sf::st_read(here::here('data/raw-data/Grids/ReferenceGrid_France_bin_1000m.gpkg'))
grid <- sf::st_transform(grid, sf::st_crs(origin))
origin <- terra::crop(origin, grid)
terra::writeRaster(origin, here::here(paste0('data/derived-data/LUMM/ssp1_2020.tif')), overwrite = T)
terra::writeRaster(origin, here::here(paste0('data/derived-data/LUMM/ssp3_2020.tif')), overwrite = T)

foreach(t = todo) %do% {
  # SSP1
  toupdte <- terra::rast(here::here(paste0('data/raw-data/LUMM/SSP1/East/cov_all.', t, '.asc')))
  terra::crs(toupdte) <- "epsg:3035"
  toupdte.bis <- terra::rast(here::here(paste0('data/raw-data/LUMM/SSP1/North/cov_all.', t, '.asc')))
  terra::crs(toupdte.bis) <- "epsg:3035"
  toupdte <- terra::merge(toupdte, toupdte.bis)
  toupdte.bis <- terra::rast(here::here(paste0('data/raw-data/LUMM/SSP1/South/cov_all.', t, '.asc')))
  terra::crs(toupdte.bis) <- "epsg:3035"
  toupdte <- terra::merge(toupdte, toupdte.bis)
  toupdte.bis <- terra::rast(here::here(paste0('data/raw-data/LUMM/SSP1/West/cov_all.', t, '.asc')))
  terra::crs(toupdte.bis) <- "epsg:3035"
  toupdte <- terra::merge(toupdte, toupdte.bis)
  toupdte <- terra::crop(toupdte, origin)
  rast.bin <-  toupdte
  terra::values(rast.bin) <- ifelse(is.na(terra::values(rast.bin)), T, F)
  toupdte <- terra::ifel(rast.bin, origin, toupdte)
  terra::writeRaster(toupdte, here::here(paste0('data/derived-data/LUMM/ssp1_', t+2020, '.tif')), overwrite = T)
  
  # SSP3
  toupdte <- terra::rast(here::here(paste0('data/raw-data/LUMM/SSP3/East/cov_all.', t, '.asc')))
  terra::crs(toupdte) <- "epsg:3035"
  toupdte.bis <- terra::rast(here::here(paste0('data/raw-data/LUMM/SSP3/North/cov_all.', t, '.asc')))
  terra::crs(toupdte.bis) <- "epsg:3035"
  toupdte <- terra::merge(toupdte, toupdte.bis)
  toupdte.bis <- terra::rast(here::here(paste0('data/raw-data/LUMM/SSP3/South/cov_all.', t, '.asc')))
  terra::crs(toupdte.bis) <- "epsg:3035"
  toupdte <- terra::merge(toupdte, toupdte.bis)
  toupdte.bis <- terra::rast(here::here(paste0('data/raw-data/LUMM/SSP3/West/cov_all.', t, '.asc')))
  terra::crs(toupdte.bis) <- "epsg:3035"
  toupdte <- terra::merge(toupdte, toupdte.bis)
  toupdte <- terra::crop(toupdte, origin)
  rast.bin <-  toupdte
  terra::values(rast.bin) <- ifelse(is.na(terra::values(rast.bin)), T, F)
  toupdte <- terra::ifel(rast.bin, origin, toupdte)
  terra::writeRaster(toupdte, here::here(paste0('data/derived-data/LUMM/ssp3_', t+2020, '.tif')), overwrite = T)
}


##############################
# Masking SDM with LUMM
##############################
sp.lst <- list.files(here::here('data/derived-data/DistributionMaps/France/Current/ca/'))
sp.lst <- gsub('.tif', '', sp.lst)
all.files <- expand.grid(time = seq(2020, 2050, by = 5), ssp = c('ssp1', 'ssp3'), gcm = c('gfdl-esm4', 'mpi-esm1-2-hr', 'ukesm1-0-ll'), sp = sp.lst)
pref <- rjson::fromJSON(file = here::here('data/raw-data/HabitatPreference/species_habitat_prefs.json'))
codes.match <- openxlsx::read.xlsx(here::here('data/raw-data/LUMM/LUMM_CodesCorresp.xlsx'))
codes.match <- codes.match[, c('CodeSubclassC', 'CodeF')]

all.files$file <- paste0(all.files$ssp,'_', all.files$gcm, '_', all.files$time, '_', all.files$sp)
sp.code <- readr::read_csv("data/raw-data/HabitatPreference/nc_vertebrate_list.csv")

foreach(f = all.files$file) %do% {
  print(f)
  prob <- terra::rast(here::here(paste0('data/derived-data/DistributionMaps/France/TemporallySmoothed/', f, '.tif')))
  lumm <- terra::rast(here::here(paste0('data/derived-data/LUMM/', all.files$ssp[all.files$file == f] , '_', all.files$time[all.files$file == f], '.tif')))
  sp.nme <- all.files$sp[all.files$file == f]
  sp.cd <- sp.code$Code[sp.code$SpeciesName %in%  gsub('_', ' ', sp.nme)]
  cd.pref <- pref[names(pref) %in% sp.cd][[1]]$lumm
  cd.pref2 <- data.frame(orig.cd = cd.pref)
  cd.pref2 <- dplyr::left_join(cd.pref2, codes.match, by = c('orig.cd' = 'CodeSubclassC'))
  lumm.sp <- lumm
  idx <-  terra::values(lumm.sp) %in% cd.pref2$CodeF
  lumm.sp[idx] <- 1
  lumm.sp[!idx] <- 0
  prob <- prob * lumm.sp 
  terra::writeRaster(prob, here::here(paste0('data/derived-data/DistributionMaps/France/TemporallySmoothed_&_Masked/', f, '.tif')), overwrite = T)
}


