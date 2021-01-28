library(lubridate)
library(data.table)
library(sf)
library(tidyverse)
library(mapview)
library(rgeos)
library(ggplot2)
library(ggpubr)
library(devtools)

multiLayer=TRUE

vPREBAS <- "master"   #### choose PREBAS verson to run the model  "master" , "v0.2.x"
devtools::install_github("ForModLabUHel/Rprebasso", ref=vPREBAS)

library(Rprebasso) 
