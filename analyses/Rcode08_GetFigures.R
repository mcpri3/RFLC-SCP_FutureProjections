################################################################################################################################
# This script provides the code used to plot the different figures shown in the main text of the scientific paper
################################################################################################################################
# Figures are located in the figures/ folder

# General parameters 
tobind <- data.frame(GroupID = c(paste0('MAM', 1:11), paste0('AVE', 1:19), paste0('AMP', 1:4), paste0('REP', 1:3)), 
                     BigGroup = c(rep('Mammalia', 11), rep('Aves', 19), rep('Amphibia', 4), rep('Reptilia', 3)))
lev.biggp <- c('Mammalia', 'Aves', 'Amphibia', 'Reptilia')
lev.gp <- c(paste0('MAM', 1:11), paste0('AVE', 1:19), paste0('AMP', 1:4), paste0('REP', 1:3))
Ngroup <- data.frame(Class =  c('Mammalia', 'Aves', 'Amphibia', 'Reptilia'), N = c(11, 19, 4, 3))
NPA <- data.frame(PAtype =  c('S', 'NS'), Num = c(3, 6))

ep.type <- data.frame(TYPE = c("APB", "APG", "APHN", "CPN", "RNN", "RB" , "RNC", "RNR", "PPRNN", "RNCFS", "CEN", "CDL", "AAPN", "PPRNR", "ZPS","SIC","PNR"), 
                      TYPE_simple = c('Prefectural protection order', 'Prefectural protection order', 'Prefectural protection order', "National park", 'Natural or biological reserve', 'Natural or biological reserve', 
                                      'Natural or biological reserve', 'Natural or biological reserve', 'Natural or biological reserve (buffer zone)', 'National hunting and wildlife reserve', 'Conservatory site', 'Conservatory site', 'National park (buffer zone)', 
                                      'Natural or biological reserve (buffer zone)', 'Natura 2000 site', 'Natura 2000 site', 'Regional natural park'),
                      TYPE.col = c("#E41A1C","#E41A1C","#E41A1C", "#377EB8", "#4DAF4A", "#4DAF4A", "#4DAF4A", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#FFFF33", "#A65628","#984EA3",  "#F781BF", "#F781BF", "#01665E"))
ep.type$TYPE_simple <- factor(ep.type$TYPE_simple, levels = unique(c(ep.type$TYPE_simple)))

# General function
run.mods <- function(df, y.col = 'area.km2', PA = 0) { #PA = {0, 1, 2} level of details to keep for PA 
  
  if(length(unique(df[, y.col])) != 1) {
    
    df$Time <- as.numeric(df$Time)
    df$Time.sqr <- df$Time*df$Time
    
    mod0 <- lm(df[, y.col] ~ 1 , data = df)
    mod1 <- lm(df[, y.col] ~ Time , data = df)
    mod2 <- lm(df[, y.col] ~ Time + Time.sqr, data = df)
    mod3 <- chngpt::chngptm(formula.1 = df[, y.col] ~ 1 , formula.2 = ~Time, type = 'step', family = 'gaussian', data = df)
    AICc.df <- data.frame(mod = c('no change', 'linear', 'polynomial', 'abrupt'), 
                          AICc = c(MuMIn::AICc(mod0), MuMIn::AICc(mod1), MuMIn::AICc(mod2), MuMIn::AICc(mod3)))
    AICc.df$deltaAICc <- abs(AICc.df$AICc - AICc.df$AICc[which.min(AICc.df$AICc)])
    
    AICc.df$evol <- NA
    AICc.df$evol[AICc.df$mod == 'no change'] <- 'No change'
    AICc.df$evol[AICc.df$mod == 'linear'] <- ifelse(mod1$coefficients[2] > 0, "Increase", 'Decrease')
    AICc.df$evol[AICc.df$mod == 'polynomial'] <- ifelse(mod2$coefficients[3] > 0, "U-shape", 'Concave')
    AICc.df$evol[AICc.df$mod == 'abrupt'] <- ifelse(mod3$coefficients[2] > 0, "Abrupt increase", 'Abrupt decrease')
    
    AICc.df$chgpt <- NA
    AICc.df$chgpt[AICc.df$mod == 'abrupt'] <- mod3$coefficients[3]
    
    AICc.df$df <- c(insight::get_df(mod0), insight::get_df(mod1), insight::get_df(mod2), NA)
    
    AICc.df$p.val <- c(summary(mod0)$coefficients[4], summary(mod1)$coefficients[2, 4], summary(mod2)$coefficients[3, 4], summary(mod3)$coefficients[2,5])
    AICc.df$is.signif <- ifelse(AICc.df$p.val <= 0.05, T, F)
    
    final <- AICc.df[AICc.df$deltaAICc <= 2,]
    
    if (nrow(final) > 1) {
      final <- final[final$df %in% max(na.omit(final$df)),]
    }
    
  } else { #if only NULL values
    final <- data.frame(mod = 'no change', AICc = NA, deltaAICc = NA, evol = 'No change', chgpt = NA, df = NA, p.val = NA, is.signif = NA)
  }
  
  if (PA == 0) {
    final$Class <- unique(df$Class)
    final$GroupID <- unique(df$GroupID) 
  }
  
  if (PA == 1) {
    final$BigGroup <- unique(df$BigGroup)
    final$GroupID <- unique(df$GroupID) 
    final$TYPE_simple <- unique(df$TYPE_simple)
    final$Class <- unique(df$Class)
    
  } 
  
  if (PA == 2) {
    final$Class <- unique(df$Class)
    final$GroupID <- unique(df$GroupID) 
    final$TYPE <- unique(df$TYPE)
    final$SITECODE <- unique(df$SITECODE)
    final$CLASS <- unique(df$CLASS)
  } 
  
  return(final)
}

# Extraire uniquement la légende
g_legend <- function(a_gplot) {
  tmp <- ggplot_gtable(ggplot_build(a_gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

########################################################################################
################### Map ecological continuities ########################################
########################################################################################

# ==> ECgif 

########################################################################################
################### Trends of EC area ###############################################
########################################################################################

ec.area <- openxlsx::read.xlsx(here::here('outputs/Indicators/EcologicalContinuitiesArea.xlsx'))
ec.area$area.km2.log <- log(ec.area$area.km2)
ec.area$Time <- as.numeric(ec.area$Time)
# Supp Mat figure 
rmarkdown::render(here::here('figures/GetEvolECArea.Rmd'))

area.stat <- foreach::foreach(g = unique(paste(ec.area$Class, ec.area$GroupID))) %dopar% {
  sub.df <- ec.area[paste(ec.area$Class, ec.area$GroupID) %in% g, ]
  stats <- run.mods(df = sub.df)
}

stats <- do.call(rbind, area.stat)
stats$GroupID <- paste0(unlist(lapply(strsplit(stats$Class, ''), function(x){
  return(paste(toupper(x[1:3]), collapse = ''))
})), stats$GroupID)
sankey.dfplot <- stats 
colnames(sankey.dfplot) <- paste0(colnames(sankey.dfplot), '.ECarea')
sankey.dfplot2 <- sankey.dfplot

stats.tb <- stats |> 
  janitor::tabyl(Class, evol) 
df.barplot <- data.frame(Class = rep(stats.tb$Class, time = ncol(stats.tb)-1), 
                         Evolution = rep(colnames(stats.tb)[-1], each = length(stats.tb$Class)),
                         val = as.numeric(unlist(stats.tb)[5:length(unlist(stats.tb))]))

df.barplot$GeneralEvol <- ifelse(df.barplot$Evolution %in% c('Abrupt increase', 'Increase'), 'Increase', 
                                 ifelse(df.barplot$Evolution %in% c('Abrupt decrease', 'Decrease'), 'Decrease', 
                                        ifelse(df.barplot$Evolution %in% c('Concave', 'U-shape'), 'Fluctuations', 'No change')))
df.barplot <- dplyr::left_join(df.barplot, Ngroup, by = 'Class')
df.barplot$val.perc <- df.barplot$val/df.barplot$N*100
df.barplot$GeneralEvol <- factor(df.barplot$GeneralEvol, levels = c('No change','Increase', 'Decrease', 'Fluctuations'))
df.barplot$Evolution <- factor(df.barplot$Evolution, levels = c('No change','Abrupt increase', 'Increase', "Abrupt decrease", 'Decrease', 'Concave', 'U-shape'))
df.barplot$Class <- factor(df.barplot$Class, levels = c('Mammalia', 'Aves', 'Amphibia', 'Reptilia'))

p1 <- ggplot(data = df.barplot, aes(x = Class, y = val.perc, fill = Evolution)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(name = 'Ecological continuity \narea', values =  c("#01665E", "#377EB8", "darkblue", "#E41A1C", "darkred", "gray10","darkgrey"),
                    labels = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape'), 
                    breaks = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape')) +
  xlab('') +
  ylab('Percentage of groups') +
  theme_light() +
  theme(text = element_text(size = 20))
p1
ggsave(p1, filename = here::here('figures/EvolutionECarea.png'), width = 9)

by(df.barplot$val, df.barplot$Evolution, sum)/sum(df.barplot$val)*100

##########################################################################################################################
################### Percentage of overlap between ecological continuities and protected areas ############################
##########################################################################################################################

#Transient 
ec.area <- openxlsx::read.xlsx(here::here('outputs/Indicators/EcologicalContinuitiesArea.xlsx'))
ec.area <- ec.area[ec.area$TYPE %in% 'Transient',]

all.files.full <- list.files(here::here(paste0('outputs/Indicators/')))
all.files.full <- all.files.full[grep('PercOverlap', all.files.full, fixed = T)]
all.files <- all.files.full[grep('_Transient_', all.files.full, fixed = T)]

val <- foreach::foreach(f = all.files) %dopar% {
  
  ffile <- readRDS(here::here(paste0('outputs/Indicators/', f)))
  
  lb <- data.table::rbindlist(lapply(strsplit(f, '_'), function(x) {
    return(data.frame(class = x[3], group = x[5], res = x[7], suit = x[9], dd = x[11], fnorm = x[13], time = as.numeric(x[14]) + 2.5, ssp = x[15], gcm = x[16]))
  }))
  
  ec.area.f <- ec.area$area.km2[ec.area$Class == lb$class & ec.area$GroupID == lb$group & ec.area$TransfoCoef == lb$res & ec.area$SuitThreshold == lb$suit &
                                  ec.area$DispDist == lb$dd & ec.area$NormFlowThreshold == lb$fnorm & ec.area$Time == lb$time & 
                                  ec.area$ssp == lb$ssp & ec.area$gcm == lb$gcm]
  
  val <- cbind(data.frame(BigGroup = lb$class, 
                          Group = lb$group,
                          Rest.c = lb$res, 
                          Suit.t = lb$suit, 
                          DD =  gsub('km', '', lb$dd), 
                          Fnorm = lb$fnorm, 
                          Time = lb$time, 
                          SSP = lb$ssp, 
                          GCM = lb$gcm, EC.area.km2 = ec.area.f), ffile)
  return(val)
}
val.transient <- do.call(rbind, val)

#Steady
ec.area <- openxlsx::read.xlsx(here::here('outputs/Indicators/EcologicalContinuitiesArea.xlsx'))
ec.area <- ec.area[ec.area$TYPE %in% 'Steady',]

all.files.full <- list.files(here::here(paste0('outputs/Indicators/')))
all.files.full <- all.files.full[grep('PercOverlap', all.files.full, fixed = T)]
all.files <- all.files.full[-grep('_Transient_', all.files.full, fixed = T)]

val <- foreach::foreach(f = all.files) %dopar% {
  
  ffile <- readRDS(here::here(paste0('outputs/Indicators/', f)))
  
  lb <- data.table::rbindlist(lapply(strsplit(f, '_'), function(x) {
    return(data.frame(class = x[2], group = x[4], res = x[6], suit = x[8], dd = x[10], fnorm = x[12], time = x[13], ssp = x[14], gcm = x[15]))
  }))
  
  ec.area.f <- ec.area$area.km2[ec.area$Class == lb$class & ec.area$GroupID == lb$group & ec.area$TransfoCoef == lb$res & ec.area$SuitThreshold == lb$suit &
                                  ec.area$DispDist == lb$dd & ec.area$NormFlowThreshold == lb$fnorm & ec.area$Time == lb$time & 
                                  ec.area$ssp == lb$ssp & ec.area$gcm == lb$gcm]
  
  val <- cbind(data.frame(BigGroup = lb$class, 
                          Group = lb$group,
                          Rest.c = lb$res, 
                          Suit.t = lb$suit, 
                          DD =  gsub('km', '', lb$dd), 
                          Fnorm = lb$fnorm, 
                          Time = lb$time, 
                          SSP = lb$ssp, 
                          GCM = lb$gcm, EC.area.km2 = ec.area.f), ffile)
  return(val)
}

val <- do.call(rbind, val)
val <- rbind(val, val.transient)
val$GroupID <- paste0( unlist(lapply(strsplit(val$BigGroup, ''), function(x){
  return(paste(toupper(x[1:3]), collapse = ''))
})), val$Group)
val <- dplyr::left_join(val, ep.type, by = c('Type'='TYPE'))
val$ratio.perc.area <- val$Perc.overlap.corrid/val$EC.area.km2 
saveRDS(val, here::here('outputs/Indicators/StandardizedEC-PAOverlap'))

# Supp Mat figures 
rmarkdown::render(here::here('figures/GetStandardizedEC-PAOverlaps.Rmd'))

#############
# Test stat 
#############
val <- readRDS(here::here('outputs/Indicators/StandardizedEC-PAOverlap'))

# run individual mods
overlap.stat <- foreach::foreach(g = unique(paste(val$GroupID, val$TYPE_simple))) %dopar% {
  sub.df <- val[paste(val$GroupID, val$TYPE_simple) %in% g, ]
  stats <- run.mods(df = sub.df, y.col = 'ratio.perc.area', PA = 1)
}
# rbind outputs
stats <- do.call(rbind, overlap.stat)
table(stats$evol)/nrow(stats)*100 #general stats 
table(stats$evol[stats$Class == 'S'])/nrow(stats[stats$Class == 'S',])*100
table(stats$evol[stats$Class == 'NS'])/nrow(stats[stats$Class == 'NS',])*100

stats.san <- stats
colnames(stats.san) <- paste0(colnames(stats.san), '.PAoverlap')
sankey.dfplot <- dplyr::left_join(stats.san, sankey.dfplot, by = c('GroupID.PAoverlap'= 'GroupID.ECarea'))

# Summarise results for St vs NSt
df.barplot <- expand.grid(Class = lev.biggp, 
                          Evolution = c('No change', 'Abrupt increase', 'Increase','Abrupt decrease', 'Decrease', 'Concave', 'U-shape'), 
                          PAtype = c('S', 'NS'))
df.barplot$val <- NA 
for (i in 1:nrow(df.barplot)) {
  substat <- stats[(stats$BigGroup == df.barplot$Class[i]) & (stats$evol == df.barplot$Evolution[i]) & (stats$Class == df.barplot$PAtype[i]),]
  df.barplot$val[i] <- nrow(substat)
}

df.barplot <- dplyr::left_join(df.barplot, Ngroup, by = 'Class')
df.barplot <- dplyr::left_join(df.barplot, NPA, by = 'PAtype')

df.barplot$val.perc <- df.barplot$val/(df.barplot$N*df.barplot$Num)*100

df.barplot$Evolution <- factor(df.barplot$Evolution, levels = c('No change','Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape'))
df.barplot$Class <- factor(df.barplot$Class, levels = c('Mammalia', 'Aves', 'Amphibia', 'Reptilia'))

p2 <- ggplot(data = df.barplot[df.barplot$PAtype == 'S',], aes(x = Class, y = val.perc, fill = Evolution)) + 
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c("#01665E", "#377EB8", "darkblue", "#E41A1C", "darkred","gray10","darkgrey"), 
                    labels = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape')) +
  xlab('') +
  ylab('Percentage of models') +
  theme_light() +
  ggtitle('A. Strict protections (all types)') +
  theme(text = element_text(size = 20), legend.position = 'none')
p2
p2bis <- ggplot(data = df.barplot[df.barplot$PAtype == 'S',], aes(x = Class, y = val.perc, fill = Evolution)) + 
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c("#01665E", "#377EB8", "darkblue", "#E41A1C", "darkred","gray10","darkgrey"), 
                    labels = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape')) +
  xlab('') +
  ylab('Percentage of models') +
  theme_light() +
  ggtitle('A. Strict protections') +
  theme(text = element_text(size = 20), legend.position = 'none')

p3 <- ggplot(data = df.barplot[df.barplot$PAtype == 'NS',], aes(x = Class, y = val.perc, fill = Evolution)) + 
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c("#01665E", "#377EB8", "darkblue", "#E41A1C", "darkred", "gray10","darkgrey"), 
                    labels = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape')) +
  xlab('') +
  ylab('Percentage of models') +
  theme_light() +
  ggtitle('A. Non-strict protections (all types)') +
  theme(text = element_text(size = 20), legend.position = 'none')
p3
p3bis <- ggplot(data = df.barplot[df.barplot$PAtype == 'NS',], aes(x = Class, y = val.perc, fill = Evolution)) + 
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c("#01665E", "#377EB8", "darkblue", "#E41A1C", "darkred", "gray10","darkgrey"), 
                    labels = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape')) +
  xlab('') +
  ylab('') +
  theme_light() +
  ggtitle('B. Non-strict protections') +
  theme(text = element_text(size = 20), legend.position = 'none')

# Legend extraction 
pleg <- ggplot(data = df.barplot[df.barplot$PAtype == 'S',], aes(x = Class, y = val.perc/6, fill = Evolution)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(name = 'Standardized EC-PA overlap', values = c("#01665E", "#377EB8", "darkblue", "#E41A1C", "darkred", "gray10","darkgrey"), 
                    breaks = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape'),
                    labels = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape')) +
  theme_light() +
  theme(text = element_text(size = 20))
leg <- g_legend(pleg)

pdf(here::here('figures/StandardizedEC-PAoverlap_St-NStPAs.pdf'), height =8, width = 14)
gridExtra::grid.arrange(grobs = list(p2bis, p3bis, leg), ncol = 3, nrow = 1)
dev.off()

# Summarise results per PA type 
df.barplot <- expand.grid(Class = lev.biggp, 
                          Evolution = c('No change', 'Abrupt increase', 'Increase','Abrupt decrease', 'Decrease', 'Concave', 'U-shape'), 
                          PAtype = unique(ep.type$TYPE_simple))
df.barplot$val <- NA 
for (i in 1:nrow(df.barplot)) {
  substat <- stats[(stats$BigGroup == df.barplot$Class[i]) & (stats$evol == df.barplot$Evolution[i]) & (stats$TYPE_simple == df.barplot$PAtype[i]),]
  df.barplot$val[i] <- nrow(substat)
}

df.barplot <- dplyr::left_join(df.barplot, Ngroup, by = 'Class')
df.barplot$val.perc <- df.barplot$val/df.barplot$N*100

df.barplot$Evolution <- factor(df.barplot$Evolution, levels = c('No change','Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape'))
df.barplot$Class <- factor(df.barplot$Class, levels = c('Mammalia', 'Aves', 'Amphibia', 'Reptilia'))

######################
# Plot for St-PAs
######################
full.px <- list()
full.px[[1]] <- p2

for (p in unique(stats$TYPE_simple[stats$Class == 'S'])[1:2]) {
  px <- ggplot(data = df.barplot[df.barplot$PAtype == p,], aes(x = Class, y = val.perc, fill = Evolution)) +
    geom_bar(stat = 'identity') +
    scale_fill_manual(values = c("#01665E", "#377EB8", "darkblue", "#E41A1C", "darkred", "gray10","darkgrey"), 
                      breaks = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape')) +
    xlab('') +
    ylab('') +
    theme_light() +
    ggtitle(paste0(toupper(letters)[which(p == unique(stats$TYPE_simple[stats$Class == 'S'])) + 1], '. ', p)) +
    theme(text = element_text(size = 20), legend.position = 'none')
  
  full.px[[p]] <- px
}

p = unique(stats$TYPE_simple[stats$Class == 'S'])[3]
px <- ggplot(data = df.barplot[df.barplot$PAtype == p,], aes(x = Class, y = val.perc, fill = Evolution)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c("#01665E", "#377EB8", "darkblue", "#E41A1C", "darkred", "gray10","darkgrey"), 
                    breaks = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape')) +
  xlab('') +
  ylab('Percentage of models') +
  theme_light() +
  ggtitle(paste0(toupper(letters)[which(p == unique(stats$TYPE_simple[stats$Class == 'S'])) + 1], '. ', p)) +
  theme(text = element_text(size = 20), legend.position = 'none')

full.px[[length(full.px)+1]] <- px
full.px[[length(full.px)+1]] <- leg
pfinal1 <- gridExtra::grid.arrange(grobs = full.px, ncol = 3, nrow = 2)

pdf(here::here('figures/StandardizedEC-PAoverlap_StPAs.pdf'), height = 13, width = 20)
gridExtra::grid.arrange(grobs = full.px, ncol = 3, nrow = 2)
dev.off()

######################
# Plot for NSt-PAs
######################
full.px <- list()
full.px[[1]] <- p3

for (p in unique(stats$TYPE_simple[stats$Class == 'NS'])[1:3]) {
  px <- ggplot(data = df.barplot[df.barplot$PAtype == p,], aes(x = Class, y = val.perc, fill = Evolution)) +
    geom_bar(stat = 'identity') +
    scale_fill_manual(values = c("#01665E", "#377EB8", "darkblue", "#E41A1C", "darkred", "gray10","darkgrey"), 
                      breaks = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape')) +
    xlab('') +
    ylab('') +
    theme_light() +
    ggtitle(paste0(toupper(letters)[which(p == unique(stats$TYPE_simple[stats$Class == 'NS'])) + 1], '. ', p)) +
    theme(text = element_text(size = 20), legend.position = 'none')
  
  full.px[[p]] <- px
}
p <- unique(stats$TYPE_simple[stats$Class == 'NS'])[4]
px <- ggplot(data = df.barplot[df.barplot$PAtype == p,], aes(x = Class, y = val.perc, fill = Evolution)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c("#01665E", "#377EB8", "darkblue", "#E41A1C", "darkred", "gray10","darkgrey"), 
                    breaks = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape')) +
  xlab('') +
  ylab('Percentage of models') +
  theme_light() +
  ggtitle(paste0(toupper(letters)[which(p == unique(stats$TYPE_simple[stats$Class == 'NS'])) + 1], '. ', p)) +
  theme(text = element_text(size = 20), legend.position = 'none')
full.px[[length(full.px)+1]] <- px

for (p in unique(stats$TYPE_simple[stats$Class == 'NS'])[5:6]) {
  px <- ggplot(data = df.barplot[df.barplot$PAtype == p,], aes(x = Class, y = val.perc, fill = Evolution)) +
    geom_bar(stat = 'identity') +
    scale_fill_manual(values = c("#01665E", "#377EB8", "darkblue", "#E41A1C", "darkred", "gray10","darkgrey"), 
                      breaks = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape')) +
    xlab('') +
    ylab('') +
    theme_light() +
    ggtitle(paste0(toupper(letters)[which(p == unique(stats$TYPE_simple[stats$Class == 'NS'])) + 1], '. ', p)) +
    theme(text = element_text(size = 20), legend.position = 'none')
  
  full.px[[p]] <- px
}
full.px[[length(full.px)+1]] <- leg
pfinal2 <- gridExtra::grid.arrange(grobs = full.px, ncol = 4, nrow = 2)

pdf(here::here('figures/StandardizedEC-PAoverlap_NStPAs.pdf'), height = 13, width = 30)
gridExtra::grid.arrange(grobs = full.px, ncol = 4, nrow = 2)
dev.off()

# Sankey chart 
library(ggalluvial)
psankey <- ggplot(data = sankey.dfplot,
       aes(axis1 = evol.ECarea, axis2 = evol.PAoverlap, y = after_stat(count))) +
  geom_alluvium(aes(), curve_type = "sigmoid") +
  geom_stratum() +
  #geom_text(stat = "stratum", aes(label = after_stat(stratum), y = after_stat(y)), size = 3) +
  scale_x_discrete(limits = c("EC area", "Standardized EC-PA \noverlap"),
                   expand = c(0.15, 0.05))+
  labs(x = "Relationship between estimated trends", y = "Number of fitted models") +
  theme_minimal(base_size = 20)
psankey
ggsave(plot = psankey, here::here('figures/Sankey_ECArea-StandardECPAOverlapNoText.svg'), device = 'svg', width = 10)

psankey <- ggplot(data = sankey.dfplot,
                  aes(axis1 = evol.ECarea, axis2 = evol.PAoverlap, y = after_stat(count))) +
  geom_alluvium(aes(), curve_type = "sigmoid") +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum), y = after_stat(y)), size = 3) +
  scale_x_discrete(limits = c("EC area", "Standardized EC-PA \noverlap"),
                   expand = c(0.15, 0.05))+
  labs(x = "Relationship between estimated trends", y = "Number of fitted models") +
  theme_minimal(base_size = 20)
ggsave(plot = psankey, here::here('figures/Sankey_ECArea-StandardECPAOverlapText.svg'), device = 'svg', width = 10)

#######################################################################################
################################### Global PC metrics #################################
#######################################################################################
# List all files 
type <- 'EucliPath'
all.files.full <- list.files(here::here(paste0('outputs/Indicators/', type, '/')))

# Transient
all.files <- all.files.full[grep('BinaryNetwork_Transient_IndicCon', all.files.full)]
ec.area <- openxlsx::read.xlsx(here::here('outputs/Indicators/EcologicalContinuitiesArea.xlsx'))
ec.area <- ec.area[ec.area$TYPE %in% 'Transient',]

# Summarize values per group
PC.df <- foreach::foreach(f=all.files) %dopar% {
  
  ffile <- readRDS(here::here(paste0('outputs/Indicators/', type, '/',f)))
  
  lb <- data.table::rbindlist(lapply(strsplit(f, '_'), function(x) {
    return(data.frame(class = x[4], group = x[6], res = x[8], suit = x[10], dd = x[12], fnorm = x[14], time = as.numeric(x[15])+2.5, ssp = x[16], gcm = x[17]))
  }))
  
  ec.area.f <- ec.area$area.km2[ec.area$Class == lb$class & ec.area$GroupID == lb$group & ec.area$TransfoCoef == lb$res & ec.area$SuitThreshold == lb$suit &
                                  ec.area$DispDist == lb$dd & ec.area$NormFlowThreshold == lb$fnorm & ec.area$Time == lb$time & 
                                  ec.area$ssp == lb$ssp & ec.area$gcm == lb$gcm]
  
  PC.df <- cbind(data.frame(Class = lb$class, 
                            Group = lb$group,
                            Rest.c = lb$res, 
                            Suit.t = lb$suit, 
                            DD =  gsub('km', '', lb$dd), 
                            Fnorm = lb$fnorm, 
                            Time = lb$time, 
                            SSP = lb$ssp, 
                            GCM = lb$gcm), data.frame(PCinter = ffile$PCinter), data.frame(PCintra = ffile$PCintra), 
                 data.frame(EC.area.km2 = ec.area.f))
  return(PC.df)
}
PC.df <- do.call(rbind, PC.df)
PC.df.transient <- PC.df 

# Steady 
all.files <- all.files.full[-grep('BinaryNetwork_Transient_IndicCon', all.files.full)]
ec.area <- openxlsx::read.xlsx(here::here('outputs/Indicators/EcologicalContinuitiesArea.xlsx'))
ec.area <- ec.area[ec.area$TYPE %in% 'Steady',]

# Summarize values per group
PC.df <- foreach::foreach(f=all.files) %dopar% {
  
  ffile <- readRDS(here::here(paste0('outputs/Indicators/', type, '/',f)))
  
  lb <- data.table::rbindlist(lapply(strsplit(f, '_'), function(x) {
    return(data.frame(class = x[3], group = x[5], res = x[7], suit = x[9], dd = x[11], fnorm = x[13], time = x[14], ssp = x[15], gcm = x[16]))
  }))
  
  ec.area.f <- ec.area$area.km2[ec.area$Class == lb$class & ec.area$GroupID == lb$group & ec.area$TransfoCoef == lb$res & ec.area$SuitThreshold == lb$suit &
                                  ec.area$DispDist == lb$dd & ec.area$NormFlowThreshold == lb$fnorm & ec.area$Time == lb$time & 
                                  ec.area$ssp == lb$ssp & ec.area$gcm == lb$gcm]
  
  PC.df <- cbind(data.frame(Class = lb$class, 
                            Group = lb$group,
                            Rest.c = lb$res, 
                            Suit.t = lb$suit, 
                            DD =  gsub('km', '', lb$dd), 
                            Fnorm = lb$fnorm, 
                            Time = lb$time, 
                            SSP = lb$ssp, 
                            GCM = lb$gcm), data.frame(PCinter = ffile$PCinter), data.frame(PCintra = ffile$PCintra), 
                 data.frame(EC.area.km2 = ec.area.f))
  return(PC.df)
}
PC.df <- do.call(rbind, PC.df)
PC.df$Time <- as.numeric(PC.df$Time)
PC.df <- rbind(PC.df.transient, PC.df)
PC.df$GroupID <- paste0(unlist(lapply(strsplit(PC.df$Class, ''), function(x){
  return(paste(toupper(x[1:3]), collapse = ''))
})), PC.df$Group)
PC.df$PCinter <- as.numeric(PC.df$PCinter)
PC.df$StandardPCinter <- PC.df$PCinter/PC.df$EC.area.km2
openxlsx::write.xlsx(PC.df, here::here('outputs/Indicators/GlobalPCmetric.xlsx'))

# Stat. model 
PC.df <- openxlsx::read.xlsx(here::here('outputs/Indicators/GlobalPCmetric.xlsx'))
# by(PC.df$PCinter, PC.df$GroupID, summary)
by(PC.df$PCinter, PC.df$Time, summary)
summary(PC.df$PCinter)*100

# run individual mods
globalPC.stat <- foreach::foreach(g = unique(PC.df$GroupID)) %dopar% {
  sub.df <- PC.df[PC.df$GroupID %in% g,]
  stats <- run.mods(df = sub.df, y.col = 'PCinter', PA = 0)
}
# rbind outputs
stats <- do.call(rbind, globalPC.stat)
table(stats$evol)/nrow(stats)*100

# Plot 
stats.tb <- stats |> 
  janitor::tabyl(Class, evol) 
df.barplot <- data.frame(Class = rep(stats.tb$Class, time = ncol(stats.tb)-1), 
                         Evolution = rep(colnames(stats.tb)[-1], each = length(stats.tb$Class)),
                         val = as.numeric(unlist(stats.tb)[5:length(unlist(stats.tb))]))
df.barplot <- dplyr::left_join(df.barplot, Ngroup, by = 'Class')
df.barplot$val.perc <- df.barplot$val/df.barplot$N*100
df.barplot$Evolution <- factor(df.barplot$Evolution, levels = c('No change','Abrupt increase', 'Increase', "Abrupt decrease", 'Decrease', 'Concave', 'U-shape'))
df.barplot$Class <- factor(df.barplot$Class, levels = c('Mammalia', 'Aves', 'Amphibia', 'Reptilia'))

p2 <- ggplot(data = df.barplot, aes(x = Class, y = val.perc, fill = Evolution)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(name = 'Global probability of \nconnectivity', values =  c("#01665E", "#377EB8", "darkblue", "#E41A1C", "darkred", "gray10","darkgrey"),
                    labels = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape'), 
                    breaks = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape')) +
  xlab('') +
  ylab('Percentage of groups') +
  theme_light() +
  theme(text = element_text(size = 20))
p2
ggsave(p2, filename = here::here('figures/EvolutionGlobalPCmetric.png'), width = 10)

# Supp Mat figures 
rmarkdown::render(here::here('figures/GetGlobalPCmetric.Rmd'))

# Sankey plot
stats.san <- stats
colnames(stats.san) <- paste0(colnames(stats.san), '.GlobalPC')
sankey.dfplot <- dplyr::left_join(stats.san, sankey.dfplot2, by = c('GroupID.GlobalPC'= 'GroupID.ECarea'))
sankey.dfplot <- table(sankey.dfplot$evol.ECarea,sankey.dfplot$evol.GlobalPC)
nodes = data.frame("name" = c(rownames(sankey.dfplot), colnames(sankey.dfplot)))
links = expand.grid(source = c(0:(nrow(sankey.dfplot)-1)), target = c((nrow(sankey.dfplot)):(nrow(sankey.dfplot) + ncol(sankey.dfplot) -1)))
links$value <- c(sankey.dfplot)

nodes_colors <- data.frame(lab = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape'), col = rep("gray10", 7))
nodes_colors=left_join(nodes,nodes_colors,by=c("name"="lab"))
cols=paste(shQuote(nodes_colors$col), collapse=", ")
colorScale=paste0('d3.scaleOrdinal() .range([',cols,'])')

p <- networkD3::sankeyNetwork(Links = links, Nodes = nodes,
                              Source = "source", Target = "target",
                              Value = "value", NodeID = "name", fontSize = 24,  colourScale = colorScale)

p <- htmlwidgets::onRender(p, '
  function(el) { 
    var cols_x = this.sankey.nodes().map(d => d.x).filter((v, i, a) => a.indexOf(v) === i).sort(function(a, b){return a - b});
    var labels = ["EC area", "Global PC metric"];
    cols_x.forEach((d, i) => {
      d3.select(el).select("svg")
        .append("text")
        .attr("x", d)
        .attr("y", 12)
        .text(labels[i]);
    })
  }
')
p

######################################################################################
################################### Local PC metrics #################################
######################################################################################
# List all files 
type <- 'EucliPath'
all.files.full <- list.files(here::here(paste0('outputs/Indicators/', type, '/')))

# Transient
all.files <- all.files.full[grep('BinaryNetwork_Transient_IndicCon', all.files.full)]
ec.area <- openxlsx::read.xlsx(here::here('outputs/Indicators/EcologicalContinuitiesArea.xlsx'))
ec.area <- ec.area[ec.area$TYPE %in% 'Transient',]

# Summarize values per group
PC.df <-foreach::foreach(f=all.files) %dopar% {
  
  ffile <- readRDS(here::here(paste0('outputs/Indicators/', type, '/',f)))
  
  lb <- data.table::rbindlist(lapply(strsplit(f, '_'), function(x) {
    return(data.frame(class = x[4], group = x[6], res = x[8], suit = x[10], dd = x[12], fnorm = x[14], time = as.numeric(x[15])+2.5, ssp = x[16], gcm = x[17]))
  }))
  
  ec.area.f <- ec.area$area.km2[ec.area$Class == lb$class & ec.area$GroupID == lb$group & ec.area$TransfoCoef == lb$res & ec.area$SuitThreshold == lb$suit &
                                  ec.area$DispDist == lb$dd & ec.area$NormFlowThreshold == lb$fnorm & ec.area$Time == lb$time & 
                                  ec.area$ssp == lb$ssp & ec.area$gcm == lb$gcm]
  
  PC.df <- cbind(data.frame(Class = lb$class, 
                            Group = lb$group,
                            Rest.c = lb$res, 
                            Suit.t = lb$suit, 
                            DD =  gsub('km', '', lb$dd), 
                            Fnorm = lb$fnorm, 
                            Time = lb$time, 
                            SSP = lb$ssp, 
                            GCM = lb$gcm, EC.area.km2 = ec.area.f), ffile$PC_i)
  return(PC.df)
}
PC.df <- do.call(rbind, PC.df)
PC.df.transient <- PC.df 
remove(PC.df)

# Steady 
all.files <- all.files.full[-grep('BinaryNetwork_Transient_IndicCon', all.files.full)]
ec.area <- openxlsx::read.xlsx(here::here('outputs/Indicators/EcologicalContinuitiesArea.xlsx'))
ec.area <- ec.area[ec.area$TYPE %in% 'Steady',]

# Summarize values per group
PC.df <- foreach::foreach(f=all.files) %dopar% {
  
  ffile <- readRDS(here::here(paste0('outputs/Indicators/', type, '/',f)))
  
  lb <- data.table::rbindlist(lapply(strsplit(f, '_'), function(x) {
    return(data.frame(class = x[3], group = x[5], res = x[7], suit = x[9], dd = x[11], fnorm = x[13], time = x[14], ssp = x[15], gcm = x[16]))
  }))
  
  ec.area.f <- ec.area$area.km2[ec.area$Class == lb$class & ec.area$GroupID == lb$group & ec.area$TransfoCoef == lb$res & ec.area$SuitThreshold == lb$suit &
                                  ec.area$DispDist == lb$dd & ec.area$NormFlowThreshold == lb$fnorm & ec.area$Time == lb$time & 
                                  ec.area$ssp == lb$ssp & ec.area$gcm == lb$gcm]
  
  PC.df <- cbind(data.frame(Class = lb$class, 
                            Group = lb$group,
                            Rest.c = lb$res, 
                            Suit.t = lb$suit, 
                            DD =  gsub('km', '', lb$dd), 
                            Fnorm = lb$fnorm, 
                            Time = lb$time, 
                            SSP = lb$ssp, 
                            GCM = lb$gcm, EC.area.km2 = ec.area.f), ffile$PC_i)
  return(PC.df)
}
PC.df <- do.call(rbind, PC.df)
PC.df <- rbind(PC.df.transient, PC.df)
PC.df$GroupID <- paste0(unlist(lapply(strsplit(PC.df$Class, ''), function(x){
  return(paste(toupper(x[1:3]), collapse = ''))
})), PC.df$Group)
PC.df$StandardPC_flux_i  <- PC.df$PC_flux_i/PC.df$EC.area.km2
saveRDS(PC.df, here::here('outputs/Indicators/LocalPCmetric'))

# Split the dataset into several smaller, one for each PA
all.ep <- sf::st_read(here::here('./data/raw-data/ProtectedAreas/AICHI_STRICT_NOTSTRICT_NO-OVERLAP.shp'), quiet = T)
all.ep$SITECODE <- paste(all.ep$SITECODE, all.ep$TYPE, sep="|")
PC.df <- readRDS(here::here('outputs/Indicators/LocalPCmetric'))
for (p in all.ep$SITECODE) {
  subdf <- PC.df[PC.df$SITECODE %in% p,]
  saveRDS(subdf, here::here(paste0('outputs/Indicators/LocalPCmetric_', p)))
}

############################################
# Stat. model 
# run individual mods
all.ep <- sf::st_read(here::here('./data/raw-data/ProtectedAreas/AICHI_STRICT_NOTSTRICT_NO-OVERLAP.shp'), quiet = T)
all.ep$SITECODE <- paste(all.ep$SITECODE, all.ep$TYPE, sep="|")

# Local PC metric 
localPC.stat <- foreach::foreach(p = all.ep$SITECODE) %dopar% {
  PC.df <- readRDS(here::here(paste0('outputs/Indicators/LocalPCmetric_', p)))
  full.stats <- data.frame()
  for (g in unique(PC.df$GroupID)) {
    sub.df <- PC.df[PC.df$GroupID %in% g,]
    stats <- run.mods(df = sub.df, y.col = 'PC_flux_i', PA = 2)
    if (length(unique(sub.df$PC_flux_i)) == 1) {
      if (unique(sub.df$PC_flux_i) == 0) {
        stats$evol = 'Not connected'
      } 
    }
    full.stats <- rbind(full.stats, stats)
  }
  return(full.stats)
}
# rbind outputs
stats <- do.call(rbind, localPC.stat)
openxlsx::write.xlsx(stats, here::here('outputs/Indicators/Trends_LocalPCmetric.xlsx'))

############################################
# Make the maps 
stats <- openxlsx::read.xlsx(here::here('outputs/Indicators/Trends_LocalPCmetric.xlsx'))
# Prep df for the plot 
get.mode <- function(df) {
  distrib <- table(df$evol)
  whichmode <- names(which(distrib == max(distrib))) 
  if(length(whichmode) > 1) {whichmode <- 'No consensus across groups'}
  return(data.frame(SITECODE = unique(df$SITECODE), TYPE = unique(df$TYPE), CLASS = unique(df$CLASS), TREND_ACROSS_GRP = whichmode))
}

full.df.plot <- data.frame()
for (g in unique(stats$Class)) {
  stats.g <- stats[stats$Class %in% g,]
  df.plot <- group_by(stats.g, SITECODE) %>% do(get.mode(.)) %>% data.frame
  df.plot$Class <- g
  full.df.plot <- rbind(full.df.plot, df.plot)
}
full.df.plot$Class <- factor(full.df.plot$Class, levels = lev.biggp)
full.df.plot$TREND_ACROSS_GRP <- factor(full.df.plot$TREND_ACROSS_GRP, 
                                        levels = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape', 'No consensus across groups', 'Not connected'))

# General mode 
df.plot <- group_by(stats, SITECODE) %>% do(get.mode(.)) %>% data.frame
df.plot$TREND_ACROSS_GRP <- factor(df.plot$TREND_ACROSS_GRP, 
                                        levels = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape', 'No consensus across groups', 'Not connected'))

# Plot 
all.ep <- sf::st_read(here::here('./data/raw-data/ProtectedAreas/AICHI_STRICT_NOTSTRICT_NO-OVERLAP.shp'), quiet = T)
all.ep$SITECODE <- paste(all.ep$SITECODE, all.ep$TYPE, sep="|")
all.ep <- dplyr::left_join(all.ep, ep.type, by = 'TYPE')
all.ep$TYPE_simple <- paste0(all.ep$TYPE_simple, ' (', all.ep$CLASS, ')')
spdf_france <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf", country = 'France')
spdf_france <- sf::st_transform(spdf_france, sf::st_crs(all.ep))
spdf_france <- sf::st_crop(spdf_france, all.ep)

# Per taxonomic group 
for (g in lev.biggp) {
  
  all.ep.c <- sf::st_centroid(all.ep)
  all.ep.c <- dplyr::left_join(all.ep.c, full.df.plot[full.df.plot$Class == g, c(1, 4:5)], by = 'SITECODE')
  all.ep.c.con <- all.ep.c[!(all.ep.c$TREND_ACROSS_GRP %in% 'Not connected'),]
  all.ep.c.notcon <- all.ep.c[all.ep.c$TREND_ACROSS_GRP %in% 'Not connected' ,]
  
  pleg <- ggplot(data = spdf_france) +
    geom_sf() +
    geom_sf(data = all.ep.c.notcon, aes(shape = TREND_ACROSS_GRP), size = 2) +
    geom_sf(data = all.ep.c.con, aes(fill = TREND_ACROSS_GRP, colour = TREND_ACROSS_GRP), shape = 21, size = 4) +
    scale_fill_manual(name = c('Local PC metric'), values = c("#01665E", "#377EB8", "darkblue", "#E41A1C", "darkred", "gray10","darkgrey", "orange"), 
                      labels = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape', 'No consensus across groups'),
                      breaks = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape', 'No consensus across groups'))+
    scale_colour_manual(name = c('Local PC metric'), values = c("#01665E", "#377EB8", "darkblue", "#E41A1C", "darkred", "gray10","darkgrey", "orange"), 
                      labels = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape', 'No consensus across groups'),
                      breaks = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape', 'No consensus across groups'))+
    scale_shape_manual(name = '', values = 4) + 
    cowplot::theme_cowplot(font_size = 24)
  leg <- g_legend(pleg)

  full.px <- list()
  for (p in unique(all.ep.c$TYPE_simple)) {

    all.ep.c.p <- all.ep.c[all.ep.c$TYPE_simple %in% p ,]
    all.ep.c.p.con <- all.ep.c.p[!(all.ep.c.p$TREND_ACROSS_GRP %in% 'Not connected'),]
    all.ep.c.p.notcon <- all.ep.c.p[all.ep.c.p$TREND_ACROSS_GRP %in% 'Not connected' ,]
    
    p.lab <- ifelse(p == 'Natural or biological reserve (buffer zone) (NS)',
                   'Natural or biological reserve \n(buffer zone) (NS)', ifelse(p == 'National hunting and wildlife reserve (NS)', 'National hunting and wildlife reserve \n(NS)', p))

    px <- ggplot(data = spdf_france) +
      geom_sf() +
      geom_sf(data = all.ep.c.p.notcon, aes(shape = TREND_ACROSS_GRP), size = 1) +
      geom_sf(data = all.ep.c.p.con, aes(fill = TREND_ACROSS_GRP, colour = TREND_ACROSS_GRP), shape = 21, size = 3) +
      ggtitle(paste0(toupper(letters)[which(p == unique(all.ep.c$TYPE_simple))], '. ', p.lab)) +
      scale_fill_manual(values = c("#01665E", "#377EB8", "darkblue", "#E41A1C", "darkred", "gray10","darkgrey", "orange"),
                        breaks = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape', 'No consensus across groups'))+
      scale_colour_manual(values = c("#01665E", "#377EB8", "darkblue", "#E41A1C", "darkred", "gray10","darkgrey", "orange"),
                        breaks = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape', 'No consensus across groups'))+
      scale_shape_manual(name = '', values = 4) + 
      cowplot::theme_cowplot(font_size = 20) +
      theme(legend.position = 'none')
    
    full.px[[p]] <- px
  }
  full.px[[length(full.px)+1]] <- leg
  
  pdf(here::here(paste0('figures/GeneralTrendLocalPCmetric_', g, '.pdf')), height = 24, width = 14) #horiz: 16 / 27 
  gridExtra::grid.arrange(grobs = full.px, ncol = 2, nrow = 5, 
                          top = grid::textGrob(paste0(which(g == lev.biggp), '. ', g), gp=grid::gpar(fontsize=30, fontface = 'bold')))
  
  dev.off()
}

# Supp Mat figures 
stats$evol <- factor(stats$evol, levels = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape', 'Not connected'))

for (g in lev.gp) {
  
  sub.df <- stats[stats$GroupID %in% g,]
  all.ep.c <- sf::st_centroid(all.ep)
  all.ep.c <- dplyr::left_join(all.ep.c, sub.df[, c(4, 10, 12)], by = 'SITECODE')
  all.ep.c.con <- all.ep.c[!(all.ep.c$evol %in% 'Not connected'),]
  all.ep.c.notcon <- all.ep.c[all.ep.c$evol %in% 'Not connected' ,]

  pleg <- ggplot(data = spdf_france) +
    geom_sf() +
    geom_sf(data = all.ep.c.notcon, aes(shape = evol), size = 2) +
    geom_sf(data = all.ep.c.con, aes(fill = evol, colour = evol), shape = 21, size = 4) +
    scale_fill_manual(name = c('Local PC metric'), values = c("#01665E", "#377EB8", "darkblue", "#E41A1C", "darkred", "gray10","darkgrey"), 
                      labels = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape'),
                      breaks = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape'))+
    scale_colour_manual(name = c('Local PC metric'), values = c("#01665E", "#377EB8", "darkblue", "#E41A1C", "darkred", "gray10","darkgrey"), 
                        labels = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape'),
                        breaks = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape'))+
    scale_shape_manual(name = '', values = 4) + 
    cowplot::theme_cowplot(font_size = 24)
  leg <- g_legend(pleg)
  
  full.px <- list()
  for (p in unique(all.ep.c$TYPE_simple)) {
    
    all.ep.c.p <- all.ep.c[all.ep.c$TYPE_simple %in% p ,]
    all.ep.c.p.con <- all.ep.c.p[!(all.ep.c.p$evol %in% 'Not connected'),]
    all.ep.c.p.notcon <- all.ep.c.p[all.ep.c.p$evol %in% 'Not connected' ,]
    
    p.lab <- ifelse(p == 'Natural or biological reserve (buffer zone) (NS)',
                    'Natural or biological reserve \n(buffer zone) (NS)', ifelse(p == 'National hunting and wildlife reserve (NS)', 'National hunting and wildlife reserve \n(NS)', p))
    
    px <- ggplot(data = spdf_france) +
      geom_sf() +
      geom_sf(data = all.ep.c.p.notcon, aes(shape = evol), size = 1) +
      geom_sf(data = all.ep.c.p.con, aes(fill = evol, colour = evol), shape = 21, size = 3) +
      ggtitle(paste0(toupper(letters)[which(p == unique(all.ep.c$TYPE_simple))], '. ', p.lab)) + 
      scale_fill_manual(values = c("#01665E", "#377EB8", "darkblue", "#E41A1C", "darkred", "gray10","darkgrey"), 
                        breaks = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape'))+
      scale_colour_manual(values = c("#01665E", "#377EB8", "darkblue", "#E41A1C", "darkred", "gray10","darkgrey"), 
                          breaks = c('No change', 'Abrupt increase', 'Increase', 'Abrupt decrease', 'Decrease', 'Concave', 'U-shape'))+
      scale_shape_manual(name = '', values = 4) + 
      cowplot::theme_cowplot(font_size = 20) +
      theme(legend.position = 'none')
    
    full.px[[p]] <- px
  }
  full.px[[length(full.px)+1]] <- leg
  
  lb <- strsplit(g, '')[[1]]
  lb <- paste(lb[4:length(lb)], collapse = '')
  pdf(here::here(paste0('figures/TrendLocalPCmetric_', g,'.pdf')), height = 16, width = 27, onefile = F)
  gridExtra::grid.arrange(grobs = full.px, ncol = 4, nrow = 3,
                          top = grid::textGrob(paste(unique(sub.df$Class), lb), gp=grid::gpar(fontsize=30, fontface = 'bold')))
  dev.off()
  
}

