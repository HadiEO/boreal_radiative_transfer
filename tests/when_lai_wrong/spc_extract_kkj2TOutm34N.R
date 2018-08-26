# This script extracts Sentinel 2 image spectra using simple buffer 12.5 meters

# Import packages
source("cache/libPaths.R")
library(rgdal); library(rgeos); library(raster)
library(sp);library(maptools);library(RStoolbox);library(ggplot2);
library(rasterVis);library(shapefiles)

# Import packages for "spectra"
library(hyperSpec);library(prospectr);library(caret);library(chemometrics)

# Disable scientific notation printing
options(scipen=999)


# Import BOA images: (1) 10 m bands. (2) 20 m bands -----------------------

folder.10m <- "data/image_data/S2_BOA_subset/10m"                         # (1) 10 m bands
temp <-  list.files(folder.10m, pattern = "\\.tif$", full.names=TRUE)  
img.10m <- stack(temp[1])                                                 # layer names not read
names(img.10m) <- c("B2_490", "B3_560", "B4_665", "B8_842")

folder.20m <- "data/image_data/S2_BOA_subset/20m"                         # (2) 20 m bands  
temp <-  list.files(folder.20m, pattern = "\\.tif$", full.names=TRUE) 
img.20m <- stack(temp[1])                                                 # layer names not read
names(img.20m) <- c("B5_705", "B6_740", "B7_783", "B8a_865", 
                    "B11_1610", "B12_2190")

# Apply scale factor 
img.10m <- img.10m * 0.0001
img.20m <- img.20m * 0.0001



# Import TOA images: (1) 10 m bands. (2) 20 m bands -----------------------                        # !!!! Change here TOA or BOA !!!!! ****

folder.10m <- "data/image_data/S2_TOA_subset/10m"                         # (1) 10 m bands
temp <-  list.files(folder.10m, pattern = "\\.tif$", full.names=TRUE)  
img.10m <- stack(temp[1])                                                 # layer names not read
names(img.10m) <- c("B2_490", "B3_560", "B4_665", "B8_842")

folder.20m <- "data/image_data/S2_TOA_subset/20m"                         # (2) 20 m bands  
temp <-  list.files(folder.20m, pattern = "\\.tif$", full.names=TRUE) 
img.20m <- stack(temp[1])                                                 # layer names not read
names(img.20m) <- c("B5_705", "B6_740", "B7_783", "B8a_865", 
                    "B11_1610", "B12_2190")

# Apply scale factor 
img.10m <- img.10m * 0.0001
img.20m <- img.20m * 0.0001



# Import field plot points shapefile (UTM34N) -----------------------------

# shp_wrong <- readOGR(dsn="data/gis", layer="LAI_2015_utm34N") 
shp <- readOGR(dsn="data/gis", layer="LAI_2015_coords_kkj2TOutm34N_qgis")                         # Change shapefile here !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
sppts <- SpatialPoints(shp@coords, proj4string=crs(img.20m), bbox = NULL)
sppolyg.w12.5 <- rgeos::gBuffer(sppts, width = 12.5,                            # Make square 25 x 25 meter spatial polygon for each point
                                quadsegs = 1, capStyle = "SQUARE", byid = T)

x11()
plot(img.10m$B8_842, main = "BOA B8 842nm")
plot(shp, add = TRUE)
plot(shp_wrong, add = TRUE, col = "red")


# Extract spectra = mean over square 25 x 25 m ---------------------------------------------------------

spc.10m.w12.5 <- raster::extract(img.10m, sppolyg.w12.5, small = TRUE, fun = mean, na.rm = TRUE, 
                                 df = TRUE, weights = TRUE, normalizeWeights = TRUE)

spc.20m.w12.5 <- raster::extract(img.20m, sppolyg.w12.5, small = TRUE, fun = mean, na.rm = TRUE, 
                                 df = TRUE, weights = TRUE, normalizeWeights = TRUE)

identical(as.character(shp@data$ID), gaps$ID)   # The order of rows (plot ID) in spc df is the same as rows in gaps df
spc.w12.5 <- bind_cols(spc.10m.w12.5[,-1], spc.20m.w12.5[,-1]) %>% mutate(ID = gaps[["ID"]]) %>% dplyr::select(ID, everything())

# write.csv2(spc.w12.5, "results/kkj2toutm34N_BOA_S2_sq25m.csv")
write.csv2(spc.w12.5, "results/kkj2toutm34N_TOA_S2_sq25m.csv")
