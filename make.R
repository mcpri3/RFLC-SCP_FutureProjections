#' RFLC-SCP_FutureProjections
#' 
#' @description 
#'This project provides all the data, codes and outputs used to produce the results presented in:
#' 
#' Prima, M.-C., Si-Moussi, S., Garcia, N., Scherpenhuijzen, N., Verburg, P., Rouveyrol, P., Suarez, L., & Thuiller, W. (under review). 
#' Global and local future connectivity trends reveal potential mixed contribution of protected areas to species range shift. 
#' Global Change Biology. 
#' 
#' The workflow follows Prima et al. (2024) https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.14444, including:   
#' 
#' Step 0. Cluster species in functional groups having similar traits and environmental niches (not included here as it was already performed in Prima et al., 2024 and Prima et al., 2025: https://conbio.onlinelibrary.wiley.com/doi/full/10.1111/conl.13148).
#' Step 1. Generate future habitat suitability maps (analyses/Rcode01_PrepSDMmaps.R)
#' Step 2. Generate future resistance and source maps for each group based on the habitat suitability maps, and generate layers used for the conditional arguments of Omniscape  (analyses/Rcode2_GetResisSuitMaps.R).
#' Step 3. Generate future steady and transient ecological continuities per group (analyses/Rcode3_GetEcologicalContinuities.R, also requires Julia scripts analyses/JuliaScript_OmniscapeRun.jl and analyses/JuliaScript_OmniscapeRun_Transient.jl).
#' Step 4. Generate future steady and transient networks of protected areas (analyses/Rcode04_GetNetworks.R)
#' Step 5. Calculate future steady and transient multi-scale network metrics (analyses/Rcode05_GetConnMetrics.R and analyses/Rcode05bis_GetConnMetricsTransient.R).
#' Step 6. Calculate other connectivity metrics (i.e., ecological continuity areas analyses/Rcode06_GetECArea.R) and ecological continuity-protected area overlap (analyses/Rcode07_GetEC-PAsOverlap.R) 
#' Step 7. Do statistical analyses and plot figures (analyses/Rcode8_GetFigures.R).
#' 
#' 
#' First run this make.R file before any run of R scripts.
#' R functions are located in the R/ folder and are automatically loaded when the make.R file is run.
#' Function help can be found by running the classic linecode help('function_name'). 
#' 
#' Initial and intermediate datasets are located is the data/ folder  
#' Generated outputs are located in the outputs/ folder 
#' Figures of the paper are located in the figures/ folder   
#' 
#' @author Marie-Caroline Prima \email{marie-caroline.prima@univ-grenoble-alpes.fr}
#' 
#' @date 2025/01/23



## Install Dependencies (listed in DESCRIPTION) ----

devtools::install_deps(upgrade = "never")


## Load Project Addins (R Functions and Packages) ----

devtools::load_all(here::here())
library(dplyr)
library(foreach)
library(ggplot2)
library(openxlsx)
library(here)
library(terra)
library(future.apply)
library(lmerTest)
library(lme4)
library(doParallel)
registerDoParallel(cores=8)

## Global Variables ----

# You can list global variables here (or in a separate R script)


## Run Project ----

# List all R scripts in a sequential order and using the following form:
# source(here::here("analyses", "script_X.R"))
