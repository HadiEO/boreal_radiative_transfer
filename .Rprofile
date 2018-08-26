# Set path to installed libraries
.libPaths("C:/Program Files/R/R-3.3.1/library")

# Import GIS and image processing packages
library(RStoolbox)
library(rasterVis)
library(shapefiles)
library(rgdal)
library(rgeos)
library(sp)
library(maptools)
library(raster)

# Import packages for "spectra"
library(hyperSpec)
library(prospectr)
library(caret)
library(chemometrics)
library(Rprospect)
library(hsdar)

# Import data science packages
library(abind)
library(plyr)
require(reshape2)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(lubridate)
library(modelr)
library(magrittr)

# Import self-written functions
source("munge/rep_col.R")
source("munge/my_spectralResampling.R")
source("tests/my_ggplot_funs.R")
source("tests/multiplot.R")
source("munge/RMSE.R")

# Import common data ?
wav.S2 <- read_csv2("data/image_data/wav_S2.csv")
wav.L8 <- read_csv2("data/image_data/wav_L8.csv")

# Plotting settings
pts.cols <- c("magenta", "chartreuse4", "dodgerblue", "dark blue", "brown", "dark orange")
pts.pchs <- c(1,7,5,6,4,2)
# mycolor <- c('cyan', 'darkgreen', 'yellow', 'burlywood',
#              'darkred', 'darkgray', 'blue', 'lightgreen')

# Disable scientific notation printing
options(scipen=999)

