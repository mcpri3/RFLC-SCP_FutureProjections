# Set working directory
setwd('/bettik/primam/RFLC-SCP_FutureProjections')

# Load required libraries
library(dplyr)
library(foreach)
library(stringr, lib.loc = '/bettik/primam/local_lib')
library(igraph)
library(sf)
library(terra)
library(doParallel)
registerDoParallel(cores=8)

# Source required functions
source(here::here('R/get.conn.metrics.transient.R'))

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
transient <- args[10]
type <- 'EucliPath'
timestep <- 5 

# Read Protected Areas (PA) layer
all.ep <- sf::st_read(here::here('./data/raw-data/ProtectedAreas/AICHI_STRICT_NOTSTRICT_NO-OVERLAP.shp'))
all.ep$SITECODE <- paste(all.ep$SITECODE, all.ep$TYPE, sep="|")
all.ep$site.area.km2 <- sf::st_area(all.ep)/1000000 #calculate PA area

# Read suitable habitat for the group for t and t+1 
suit.hab <- terra::rast(here::here(paste0('data/derived-data/SourceLayers/SourceLayer_', g, '_GroupID_', c, '_SuitThreshold_', suit.p, '_', time, '_', ssp, '_', gcm ,'.tif')))
suit.hab <- terra::as.polygons(suit.hab)
names(suit.hab) <- "val"
suit.hab <- suit.hab[suit.hab$val == 1]
suit.hab <- sf::st_as_sf(suit.hab)
suit.hab <- sf::st_transform(suit.hab, sf::st_crs(all.ep))

suit.hab.t1 <- terra::rast(here::here(paste0('data/derived-data/SourceLayers/SourceLayer_', g, '_GroupID_', c, '_SuitThreshold_', suit.p, '_', time+timestep, '_', ssp, '_', gcm ,'.tif')))
suit.hab.t1 <- terra::as.polygons(suit.hab.t1)
names(suit.hab.t1) <- "val"
suit.hab.t1 <- suit.hab.t1[suit.hab.t1$val == 1]
suit.hab.t1 <- sf::st_as_sf(suit.hab.t1)
suit.hab.t1 <- sf::st_transform(suit.hab.t1, sf::st_crs(all.ep))

# Total area of suitable habitat
A <- as.numeric(sf::st_area(suit.hab) / 1e6)
At1 <- as.numeric(sf::st_area(suit.hab.t1) / 1e6)

# Read Ecological Continuities (EC)
ec_files <- list.files(here::here('outputs/EcologicalContinuities/Vector/'))
ec_pattern <- paste0(
  'EcologicalContinuities_', ifelse(transient, "Transient_", ""),
  g, '_GroupID_', c, '_TransfoCoef_', transfo.r, '_SuitThreshold_', suit.p, 
  '_DispDist_', dd, 'km_NormFlowThreshold_', fnorm, '_', time, '_', ssp, '_', gcm, '.gpkg'
)
ec_file <- ec_files[grep(ec_pattern, ec_files)]

avail.EC <- length(ec_file) != 0

if (avail.EC) {
  corrid <- st_read(here::here(paste0('outputs/EcologicalContinuities/Vector/', ec_file))) %>%
    st_transform(st_crs(all.ep))
}

# Read network
network_files <- list.files(here::here(paste0('outputs/Networks/', type, '/')))
network_pattern <- paste0(
  'PANetwork_', ifelse(transient, "Transient_", ""),
  g, '_GroupID_', c, '_TransfoCoef_', transfo.r, '_SuitThreshold_', suit.p, 
  '_DispDist_', dd, 'km_NormFlowThreshold_', fnorm, '_', time, '_', ssp, '_', gcm
)
network_file <- network_files[grep(network_pattern, network_files)]

avail.net <- length(network_file) != 0

if (avail.net) {
  net <- readRDS(here::here(paste0('outputs/Networks/', type, '/', network_file)))
  E(net)$weight <- 0  # Reset edge weights
  
  # edge_list_pattern <- paste0(
  #   'BinaryPAEdgeList_', ifelse(transient, "Transient_", ""),
  #   g, '_GroupID_', c, '_TransfoCoef_', transfo.r, '_SuitThreshold_', suit.p, 
  #   '_DispDist_', dd, 'km_NormFlowThreshold_', fnorm, '_', time, '_', ssp, '_', gcm
  # )
  # edgelst <- readRDS(here::here(paste0('outputs/EdgeLists/', type, '/', edge_list_pattern)))
}

if (avail.EC) {
  
  # Get area of suitable habitat in each PA at t and t+1
  inter.ep.in <- sf::st_intersection(all.ep, suit.hab)
  inter.ep.in <- sf::st_intersection(inter.ep.in, corrid) 
  inter.ep.in$SITECODE <- paste(inter.ep.in$SITECODE, inter.ep.in$corridorID, sep ='-') 
  inter.ep.in$area.suit <- as.numeric(sf::st_area(inter.ep.in)/1000000)
  inter.ep.in <- sf::st_drop_geometry(inter.ep.in[, c('SITECODE', 'area.suit')])
  
  inter.ep.in.t1 <- sf::st_intersection(all.ep, suit.hab.t1)
  inter.ep.in.t1 <- sf::st_intersection(inter.ep.in.t1, corrid) 
  inter.ep.in.t1$SITECODE <- paste(inter.ep.in.t1$SITECODE, inter.ep.in.t1$corridorID, sep ='-') 
  inter.ep.in.t1$area.suit.t1 <- as.numeric(sf::st_area(inter.ep.in.t1)/1000000)
  inter.ep.in.t1 <- sf::st_drop_geometry(inter.ep.in.t1[, c('SITECODE', 'area.suit.t1')])
  
  inter.ep.in <- dplyr::full_join(inter.ep.in, inter.ep.in.t1, by = 'SITECODE')
  inter.ep.in$area.suit[is.na(inter.ep.in$area.suit)] <- 0
  inter.ep.in$area.suit.t1[is.na(inter.ep.in$area.suit.t1)] <- 0
  
  if (avail.net) {
    
    vnme <- igraph::vertex_attr(net)$name
    tpe <- data.frame(vnme = vnme, SITECODE = unlist(lapply(strsplit(vnme, '-', fixed = T), function(x){return(x[1])})))
    tpe <- dplyr::left_join(tpe, sf::st_drop_geometry(all.ep[, c('SITECODE', 'CLASS', 'TYPE')]), by='SITECODE')
    
    #######################
    # BINARY NETWORK
    #######################
    # Get connectivity metrics for the network 
    mtrx.full <- get.conn.metrics.transient(nnet = net)
    colnames(mtrx.full$Betwness)[colnames(mtrx.full$Betwness) == 'btw'] <- 'Btw_i'
    
    indic.con <- list()
    indic.con$PC_i <- mtrx.full$PC_i
    indic.con$Betwness <- mtrx.full$Betwness
    indic.con$PCinter <- mtrx.full$PCinter
    indic.con$PCintra <- mtrx.full$PCintra
    
  } else { #no network, only PCintra is not null
    
    # Get local PC_intra 
    interm <- sf::st_drop_geometry(inter.ep.in)
    interm$PC_intra_i <- (interm$area.suit * interm$area.suit.t1)/(A*At1)
    interm$SITECODE <- unlist(lapply(strsplit(interm$SITECODE, '-'), function(x){return(x[1])}))
    ssum <- function(df) {return(data.frame(SITECODE = unique(df$SITECODE),PC_intra_i = sum(df$PC_intra_i)))}
    interm <- group_by(interm, SITECODE) %>% do(ssum(.)) %>% data.frame
    PC_intra_i <- data.frame(SITECODE = all.ep$SITECODE)
    PC_intra_i <- dplyr::left_join(PC_intra_i, interm, by = 'SITECODE')
    PC_intra_i$PC_intra_i[is.na(PC_intra_i$PC_intra_i)] <- 0
    PC_intra_i <- PC_intra_i[, c('SITECODE', 'PC_intra_i')]
    
    # Create data frame of local PCmetrics 
    PC_i <- PC_intra_i
    PC_i$PC_flux_i <- 0
    PC_i <- dplyr::left_join(PC_i, sf::st_drop_geometry(all.ep), by = 'SITECODE')
    PC_i$site.area.km2 <- as.numeric(PC_i$site.area.km2)
    
    BTW_i <- sf::st_drop_geometry(all.ep)
    BTW_i$Btw_i <- 0
    
    # Store and save all connectivity indexes 
    indic.con <- list()
    indic.con$PC_i <- PC_i
    indic.con$Betwness <- BTW_i
    indic.con$PCinter <- 0
    indic.con$PCintra <- sum(PC_i$PC_intra_i)
    
  }
}

if (!avail.EC) {
  
  # All PC metrics are equal to 0 
  PC_i <- data.frame(SITECODE = sf::st_drop_geometry(all.ep)$SITECODE, PC_intra_i = 0)
  PC_i$PC_flux_i <- 0
  PC_i <- dplyr::left_join(PC_i, sf::st_drop_geometry(all.ep), by = 'SITECODE')
  PC_i$site.area.km2 <- as.numeric(PC_i$site.area.km2)
  
  BTW_i <- sf::st_drop_geometry(all.ep)
  BTW_i$Btw_i <- 0
  
  # Store and save all connectivity indexes
  indic.con <- list()
  indic.con$PC_i <- PC_i
  indic.con$Betwness <- BTW_i
  indic.con$PCinter <- 0
  indic.con$PCintra <- 0
}

# Save results
output_path <- here::here(paste0(
  'outputs/Indicators/', type, '/BinaryNetwork_', ifelse(transient, "Transient_", ""), 'IndicCon_',
  g, '_GroupID_', c, '_TransfoCoef_', transfo.r, '_SuitThreshold_', suit.p, 
  '_DispDist_', dd, 'km_NormFlowThreshold_', fnorm, '_', time, '_', ssp, '_', gcm
))
saveRDS(indic.con, output_path)
