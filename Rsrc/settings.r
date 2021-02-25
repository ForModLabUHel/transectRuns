# ABOUT THIS SCRIPT
# Loads R-packages/libraries
# Installs a package from GitHub which contains the PREBAS model
# Creates a folder for plots exported from R

library(lubridate)
library(data.table)
library(sf)
library(tidyverse)
library(mapview)
library(rgeos)
library(ggplot2)
library(ggpubr)
library(devtools)
library(plyr)# added (not loaded due to dependency removal)
library(abind)# added (not loaded due to dependency removal)

multiLayer=TRUE

vPREBAS <- "master"   #### choose PREBAS verson to run the model  "master" , "v0.2.x"
devtools::install_github("ForModLabUHel/Rprebasso", ref=vPREBAS)

# create plots directory if that doesn't exist
if(!dir.exists(paste0(getwd(), "/plots"))) {
  dir.create(paste0(getwd(), "/plots"), recursive = TRUE)
}

library(Rprebasso) 