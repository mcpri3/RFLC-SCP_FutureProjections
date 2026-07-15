#' @title Get connectivity metrics for steady states 
#'
#' @description get.conn.metrics calculates connectivity metrics (probability of connectivity - PC, and betweenness) for a network. 
#' @param suit.con.PA shapefile of suitable habitat within ecological continuities within protected areas (i.e. suitable habitat connected and protected). 
#' @param nnet network of potential connections among protected areas.
#' @param shape.PA shapefile of protected areas including a SITECODE column.
#'
#' @return a list with local (i.e., for each network node) and global (i.e. for the whole network) PC metrics calculated together with node betweenness.  
#' @export
#'
#' @examples
get.conn.metrics <- function(suit.con.PA = inter.ep.in, nnet = net, shape.PA = all.ep) {
  
  # Get local PC_intra 
  interm <- sf::st_drop_geometry(suit.con.PA)
  interm$PC_intra_i <- interm$area.suit * interm$area.suit/A^2
  interm$SITECODE <- unlist(lapply(strsplit(interm$SITECODE, '-'), function(x){return(x[1])}))
  ssum <- function(df) {return(data.frame(SITECODE = unique(df$SITECODE),PC_intra_i = sum(df$PC_intra_i)))}
  interm <- group_by(interm, SITECODE) %>% do(ssum(.)) %>% data.frame
  PC_intra_i <- data.frame(SITECODE = shape.PA$SITECODE)
  PC_intra_i <- dplyr::left_join(PC_intra_i, interm, by = 'SITECODE')
  PC_intra_i$PC_intra_i[is.na(PC_intra_i$PC_intra_i)] <- 0
  PC_intra_i <- PC_intra_i[, c('SITECODE', 'PC_intra_i')]
  
  all.short.p <- foreach::foreach(p=names(V(nnet))) %dopar% { #run on each patch p 
    short.p <- igraph::shortest_paths(nnet, from = p)
    idx <- which(unlist(lapply(short.p$vpath, length))!= 0)
    vec.pto <- names(V(nnet))[idx]
    if (length(vec.pto) != 0) {
      return(data.table::data.table(from = p, to = vec.pto, p_ij_shortP = c(exp(-igraph::distances(nnet, v = p, to = vec.pto)))))
    }
  }  
  names(all.short.p) <- names(V(nnet))
  
  add.suit.conn.area <- function(df) {
    if (sum(class(df) %in% c('data.table'))>0) {
      # Add area of suitable habitat in each patch to the data.frame of undirect connections
      df <- left_join(df, suit.con.PA, by = c('from' = 'SITECODE'))
      colnames(df)[colnames(df) == 'area.suit'] <- "area.suit.from.km2"
      df$area.suit.from.km2[is.na(df$area.suit.from.km2)] <- 0
      df <- left_join(df, suit.con.PA, by = c('to' = 'SITECODE'))
      colnames(df)[colnames(df) == 'area.suit'] <- "area.suit.to.km2"
      df$area.suit.to.km2[is.na(df$area.suit.to.km2)] <- 0
    }
    return(df)
  }
  all.short.p <- lapply(all.short.p, add.suit.conn.area)
  
  # Get local PC_flux 
  PC_flux_i <- data.frame(SITECODE = shape.PA$SITECODE)
  PC_flux_i$PC_flux_i <- 0
  foreach::foreach(p=names(V(nnet))) %do% { # DO NOT DO DOPAR OTHERWISE DOES NOT WORK 
    short.p <- all.short.p[names(all.short.p) %in% p][[1]]
    short.p <- short.p[short.p$from != short.p$to,]
    if (!is.null(short.p)) {
      short.p$pc <- short.p$area.suit.from.km2 * short.p$area.suit.to.km2  * short.p$p_ij_shortP
      pc.flux <- sum(short.p$pc)
    } else {
      pc.flux <- 0 
    }
    
    # sum of the two
    pp <- unlist(strsplit(p, '-'))[1]
    PC_flux_i$PC_flux_i[PC_flux_i$SITECODE %in% pp] <- PC_flux_i$PC_flux_i[PC_flux_i$SITECODE %in% pp]  + 2*pc.flux/A^2 #multiplied by 2 here because path can go in two directions : i->j and j->i
  }
  
  # Put together all three local metrics 
  PC_i <- PC_intra_i
  PC_i <- dplyr::left_join(PC_i, PC_flux_i, by = 'SITECODE')
  PC_i <- dplyr::left_join(PC_i, sf::st_drop_geometry(shape.PA), by = 'SITECODE')
  PC_i$site.area.km2 <- as.numeric(PC_i$site.area.km2)
  
  # Get global PC metric = PC intra + PC inter
  PCinter <- sum(PC_i$PC_flux_i)/2 #divided by 2 because counted twice 
  PCintra <- sum(PC_i$PC_intra_i)
  
  # Add betweenness metric 
  nnet <- igraph::delete_edge_attr(nnet, 'weight')
  btw <- data.frame(nodeID = names(V(nnet)), btw = igraph::betweenness(nnet, directed = F, normalized = T))
  btw$SITECODE <- unlist(lapply(strsplit(btw$nodeID, '-', fixed = T), function(x){return(x[1])})) 
  ssum.b <- function(df) {return(data.frame(SITECODE = unique(df$SITECODE), btw = max(df$btw)))}
  btw <- group_by(btw, SITECODE) %>% do(ssum.b(.)) %>% data.frame
  btw.full <- sf::st_drop_geometry(shape.PA)
  btw.full <- left_join(btw.full, btw, by = 'SITECODE')
  btw.full$btw[is.na(btw.full$btw)] <- 0
  btw.full$site.area.km2 <- as.numeric(btw.full$site.area.km2)
  
  # Store and return all connectivity indexes 
  indic.con <- list()
  indic.con$PC_i <- PC_i
  indic.con$PCinter <- PCinter
  indic.con$PCintra <- PCintra
  indic.con$Betwness <- btw.full
  
  return(indic.con)
}
