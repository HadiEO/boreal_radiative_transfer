# Import packages
library(rgdal); library(rgeos); library(raster)
# getGDALDriverNames()$name
library(sp);library(maptools);library(RStoolbox);library(ggplot2);
library(rasterVis);library(shapefiles)

# Import packages for "spectra"
library(hyperSpec);library(prospectr);library(caret);library(chemometrics)

# Disable scientific notation printing
options(scipen=999)

#######################################################################################################################
# Import BOA
#######################################################################################################################
folder <- "Z:/PHD_data/COAUTHOR_STUDY/Data_to_run_spectra_extract/image/BOA/S2_Sen2Cor/without_BRDFcorr/clipped"
rasterlist <-  list.files(folder, pattern = "\\.tif$", full.names=FALSE) 
setwd(folder)

Suo2015.S2.st.all <- list()
for(i in rasterlist) { assign(unlist(strsplit(i, "[.]"))[1], Suo2015.S2.st.all[[i]] <- raster(i)) } 
Suo2015.S2.st.all <- stack(Suo2015.S2.st.all)

# Add b1 and b8 from TOA
setwd("Z:/PHD_data/COAUTHOR_STUDY/Data_to_run_spectra_extract/image/TOA_single/S2")
S2.b1 <- raster("S2subset_toa_B1_443_60mto20m.tif") 
S2.b8 <- raster("S2subset_toa_B8_842.tif") 
Suo2015.S2.st.all <- addLayer(Suo2015.S2.st.all, S2.b8)
# cannot stack b1 has different extent cause it's resampled from 60m. Needs to extract SPDF separately

# Reorder the bands
Suo2015.S2.st.all <- subset(Suo2015.S2.st.all, c(3:8,10,9,1,2)) 
names(Suo2015.S2.st.all) <- 
  c("B2_490", "B3_560", "B4_665", "B5_705", "B6_740", "B7_783", "B8_842", "B8a_865", 
    "B11_1610", "B12_2190")
names(S2.b1) <- "B1_443"

# Even better, save the image stack as .rds file
setwd("Z:/PHD_data/COAUTHOR_STUDY/Data_to_run_spectra_extract/imgst")
saveRDS(Suo2015.S2.st.all, "S2_imgst_noB1.rds")
saveRDS(S2.b1, "S2_img_B1.rds")


#######################################################################################################################
# (1) Create one spatial polygons (20x20m) to store the bands 
#######################################################################################################################
Suo2015.S2.all.SP <- as(Suo2015.S2.st.all$B2_490, 'SpatialPolygons') # can be any layer
Suo2015.S2.b1.SP <- as(S2.b1, 'SpatialPolygons')
# setwd("Z:/PHD_data/COAUTHOR_STUDY/Data_to_run_spectra_extract/SP")
# saveRDS(Suo2015.S2.all.SP, "S2_SP_noB1.rds")
# saveRDS(Suo2015.S2.b1.SP, "S2_SP_B1.rds")
