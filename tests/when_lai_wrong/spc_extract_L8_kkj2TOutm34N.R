# Not yet modified! 2017.06.04



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

folder.L8 <- "data/image_data/L8_TOA_subset"                         
files <-  list.files(folder.L8, pattern = "\\.tif$", full.names=TRUE)
files.refl <- files[4:9]                                                                 # select reflective bands 2 - 7
temp.ls <- list()
for(i in files.refl) { assign(unlist(strsplit(i, "[.]"))[1], temp.ls[[i]] <- raster(i)) } 
img.st <- stack(temp.ls)
names(img.st) <- c("TOA_B2_blue", "TOA_B3_green", "TOA_B4_red", "TOA_B5_nir", "TOA_B6_swir1", "TOA_B7_swir2")

# The bands are digital number. Apply reflectance mult. and add. scaling constants in the metadata
# The scaling constants are the same for all reflective bands
img.st <- img.st * 2.0000E-05 + -0.100000

L8.toa.st <- img.st


# Import field plot points shapefile (UTM35N for Landsat 8) -----------------------------

shp <- readOGR(dsn="data/gis", layer="LAI_2015_utm35N")                          # Careful utm35N for L8, utm34N for S2 image!
sppts <- SpatialPoints(shp@coords, proj4string=crs(L8.toa.st), bbox = NULL)
sppolyg.w12.5 <- rgeos::gBuffer(sppts, width = 12.5,                            # Make square 25 x 25 meter spatial polygon for each point
                                quadsegs = 1, capStyle = "SQUARE", byid = T)

sppolyg.w15 <- rgeos::gBuffer(sppts, width = 15,                                # 30 x 30 meter 
                              quadsegs = 1, capStyle = "SQUARE", byid = T)

sppolyg.w30 <- rgeos::gBuffer(sppts, width = 30,                                # 60 x 60 meter 
                              quadsegs = 1, capStyle = "SQUARE", byid = T)

# Extract spectra = mean over square 25 x 25 m ---------------------------------------------------------

spc.w12.5 <- raster::extract(L8.toa.st, sppolyg.w12.5, small = TRUE, fun = mean, na.rm = TRUE, 
                             df = TRUE, weights = TRUE, normalizeWeights = TRUE)

spc.w12.5 <- spc.w12.5[,-1] %>% mutate(ID = gaps[["ID"]]) %>% dplyr::select(ID, everything())

write.csv2(spc.w12.5, "results/TOA_L8_sq25m.csv")

# Extract spectra = mean over square 30 x 30 m ---------------------------------------------------------

spc.w15 <- raster::extract(L8.toa.st, sppolyg.w15, small = TRUE, fun = mean, na.rm = TRUE, 
                           df = TRUE, weights = TRUE, normalizeWeights = TRUE)


spc.w15 <- spc.w15[,-1] %>% mutate(ID = gaps[["ID"]]) %>% dplyr::select(ID, everything())

write.csv2(spc.w15, "results/TOA_L8_sq30m.csv")


# Extract spectra = mean over square 60 x 60 m ---------------------------------------------------------

spc.w30 <- raster::extract(L8.toa.st, sppolyg.w30, small = TRUE, fun = mean, na.rm = TRUE, 
                           df = TRUE, weights = TRUE, normalizeWeights = TRUE)

spc.w30 <- spc.w30[,-1] %>% mutate(ID = gaps[["ID"]]) %>% dplyr::select(ID, everything())

write.csv2(spc.w30, "results/TOA_L8_sq60m.csv")


