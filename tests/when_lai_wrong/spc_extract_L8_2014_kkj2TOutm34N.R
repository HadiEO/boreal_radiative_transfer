
# Import packages
source("cache/libPaths.R")
library(rgdal); library(rgeos); library(raster)
library(sp);library(maptools);library(RStoolbox);library(ggplot2);
library(rasterVis);library(shapefiles)

# Import packages for "spectra"
library(hyperSpec);library(prospectr);library(caret);library(chemometrics)

# Disable scientific notation printing
options(scipen=999)


# Import TOA images -----------------------------------------------------

folder.L8 <- "data/image_data/L8_BOA_subset_2014"                         
files <-  list.files(folder.L8, pattern = "\\.tif$", full.names=TRUE)
files.refl <- files[9:14]                                                                 # select reflective bands 2 - 7
temp.ls <- list()
for(i in files.refl) { assign(unlist(strsplit(i, "[.]"))[1], temp.ls[[i]] <- raster(i)) } 
img.st <- stack(temp.ls)
names(img.st) <- c("BOA_B2_blue", "BOA_B3_green", "BOA_B4_red", "BOA_B5_nir", "BOA_B6_swir1", "BOA_B7_swir2")

# Rescale sr
img.st <- img.st * 0.0001

L8.boa.st <- img.st


# Import field plot points shapefile (UTM35N for Landsat 8) -----------------------------

shp <- readOGR(dsn="data/gis", layer="LAI_2015_coords_kkj2TOutm34N_qgis")                          # Careful utm35N for L8, utm34N for S2 image!
sppts <- SpatialPoints(shp@coords, proj4string=crs(L8.boa.st), bbox = NULL)
sppolyg.w12.5 <- rgeos::gBuffer(sppts, width = 12.5,                            # Make square 25 x 25 meter spatial polygon for each point
                                quadsegs = 1, capStyle = "SQUARE", byid = T)

# Extract spectra = mean over square 25 x 25 m ---------------------------------------------------------

spc.w12.5 <- raster::extract(L8.boa.st, sppolyg.w12.5, small = TRUE, fun = mean, na.rm = TRUE, 
                             df = TRUE, weights = TRUE, normalizeWeights = TRUE)

spc.w12.5 <- spc.w12.5[,-1] %>% mutate(ID = gaps[["ID"]]) %>% dplyr::select(ID, everything())

write.csv2(spc.w12.5, "results/BOA_L8_sq25m_2014.csv")

