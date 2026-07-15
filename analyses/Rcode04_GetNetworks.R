# Set working directory
setwd('/bettik/primam/RFLC-SCP_FutureProjections')

# Load required libraries
library(dplyr)
library(gdistance, lib.loc = '/bettik/primam/local_lib') # also loads igraph
library(foreach)
library(stringr, lib.loc = '/bettik/primam/local_lib')

# ----------------------------------------
# Read command-line arguments (The code is written for one combination of parameters)
# ----------------------------------------
args <- commandArgs(trailingOnly = TRUE)  
g <- as.character(args[1]) 
c <- as.numeric(args[2]) 
transfo.r <- as.numeric(args[3]) 
suit.p <- as.numeric(args[4]) 
dd <- as.numeric(args[5]) 
fnorm <- as.numeric(args[6])
time <- as.numeric(args[7])
ssp <- as.character(args[8])
gcm <- as.character(args[9])
transient <- as.logical(args[10])

# ----------------------------------------
# Load required functions
# ----------------------------------------
source(here::here('R/get.dist.R'))
source(here::here('R/get.edgelst.R'))

# ----------------------------------------
# Load protected areas data
# ----------------------------------------
all.ep <- sf::st_read(here::here('./data/raw-data/ProtectedAreas/AICHI_STRICT_NOTSTRICT_NO-OVERLAP.shp')) 
all.ep$SITECODE <- paste(all.ep$SITECODE, all.ep$TYPE, sep="|")
all.ep$site.area.km2 <- sf::st_area(all.ep)/1000000 #calculate PA area

# Load Euclidean distance matrix between PAs
ep.dist <- readRDS(here::here('data/raw-data/ProtectedAreas/Distance_btw_Protections_No-overlap'))
colnames(ep.dist) <- all.ep$SITECODE
rownames(ep.dist) <- all.ep$SITECODE

# ----------------------------------------
# Locate and Load Ecological Continuities (EC)
# ----------------------------------------
ec_files <- list.files(here::here('outputs/EcologicalContinuities/Vector/'))
ec_pattern <- paste0(
  'EcologicalContinuities_', ifelse(transient, "Transient_", ""),
  g, '_GroupID_', c, '_TransfoCoef_', transfo.r, '_SuitThreshold_', suit.p, 
  '_DispDist_', dd, 'km_NormFlowThreshold_', fnorm, '_', time, '_', ssp, '_', gcm, '.gpkg'
)

ec_file <- ec_files[grep(ec_pattern, ec_files)]
avail.EC <- length(ec_file) != 0

if (avail.EC) {
  corrid <- sf::st_read(here::here(paste0('outputs/EcologicalContinuities/Vector/', ec_file))) %>%
    sf::st_transform(sf::st_crs(all.ep))
  
  # ----------------------------------------
  # Find intersection between corridors and PAs
  # ----------------------------------------
  inter <- sf::st_intersection(all.ep, corrid)
  
  if (nrow(inter) > 0) {  
    
    inter.final <- sf::st_drop_geometry(inter) 
    inter.final$SITECODE <- paste(inter.final$SITECODE, inter.final$corridorID, sep = '-') 
    inter.final <- inter.final[, c('SITECODE','SITENAME','corridorID')]
    colnames(inter.final)[colnames(inter.final) == 'corridorID'] <- 'inCorrid'
    
    # Generate edge list for intersecting sites
    edgelst.final <- group_by(inter.final, inCorrid) %>% do(get.edgelst(.)) %>% data.frame
    
    
    if (nrow(edgelst.final) > 0) {  # If inter-PA links exist (before filtering)
      
      # ----------------------------------------
      # Compute Euclidean distance between connected PAs
      # ----------------------------------------
      edgelst.final$rowid <- c(1:nrow(edgelst.final))
      dist <- group_by(sf::st_drop_geometry(edgelst.final), rowid) %>% do(get.dist(.)) %>% data.frame
      edgelst.final$dist.km2 <- dist$dist / 1000  # Convert to km
      edgelst.final <- edgelst.final[edgelst.final$dist.km2 <= dd,] #remove edges longer than the DD 
      
      if (nrow(edgelst.final) > 0) {  # If valid connections remain
        
        # ----------------------------------------
        # Generate and save network
        # ----------------------------------------
        net <- igraph::graph_from_edgelist(as.matrix(edgelst.final[, c('from', 'to')]), directed = FALSE)
        
        # Define file naming prefix based on transient flag
        file_prefix <- if (transient) "BinaryPANetwork_Transient_" else "BinaryPANetwork_"
        edge_prefix <- if (transient) "BinaryPAEdgeList_Transient_" else "BinaryPAEdgeList_"
        
        # Save network
        saveRDS(
          net, 
          here::here(paste0(
            'outputs/Networks/EucliPath/', file_prefix, g, '_GroupID_', c, '_TransfoCoef_', transfo.r,
            '_SuitThreshold_', suit.p, '_DispDist_', dd, 'km_NormFlowThreshold_', fnorm, '_', time, '_', ssp, '_', gcm
          ))
        )
        
        # Save edge list
        saveRDS(
          edgelst.final,
          here::here(paste0(
            'outputs/EdgeLists/EucliPath/', edge_prefix, g, '_GroupID_', c, '_TransfoCoef_', transfo.r, 
            '_SuitThreshold_', suit.p, '_DispDist_', dd, 'km_NormFlowThreshold_', fnorm, '_', time, '_', ssp, '_', gcm
          ))
        )
      }
    }
  }
}
