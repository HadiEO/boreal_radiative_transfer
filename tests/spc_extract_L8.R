
# Import BOA images: 30 m  -----------------------
# Pixel_qa has been checked all field plots are OK

folder.L8.BOA <- "data/image_data/L8_BOA_subset/"
temp.str <- "clipped_LC08_L1TP_189017_20150820_20170406_01_T1_"  
B2.482 <- raster(str_c(folder.L8.BOA, temp.str, "sr_band2.tif")); names(B2.482) <- "B2.482"
B3.561 <- raster(str_c(folder.L8.BOA, temp.str, "sr_band3.tif")); names(B3.561) <- "B3.561"
B4.655 <- raster(str_c(folder.L8.BOA, temp.str, "sr_band4.tif")); names(B4.655) <- "B4.655"
B5.865 <- raster(str_c(folder.L8.BOA, temp.str, "sr_band5.tif")); names(B5.865) <- "B5.865"
B6.1609 <- raster(str_c(folder.L8.BOA, temp.str, "sr_band6.tif")); names(B6.1609) <- "B6.1609"
B7.2201 <- raster(str_c(folder.L8.BOA, temp.str, "sr_band7.tif")); names(B7.2201) <- "B7.2201"


# Stack the bands
L8.BOA.st <- raster::stack(B2.482,
                           B3.561,
                           B4.655,
                           B5.865,
                           B6.1609,
                           B7.2201)

# Apply scale factor 
L8.BOA.st <- calc(L8.BOA.st, fun = function(z) z * 0.0001) 


# Import field plot points (UTM34N) -----------------------------
# --> Use shp
shp <- readOGR(dsn="data/gis", layer="Hyytiala_LAI_2015_UTM35N")
sppts <- SpatialPoints(shp@coords, proj4string=crs(L8.BOA.st), bbox = NULL)   


# Make field plot square area -----------------------------------------

sppolyg.w12.5 <- rgeos::gBuffer(sppts, width = 12.5,                            # Make square 25 x 25 meter spatial polygon for each point
                                quadsegs = 1, capStyle = "SQUARE", byid = T)

# Check if sppolyg.w12.5 coords are in the same row order as sppts coords
temp <- sapply(slot(sppolyg.w12.5, "polygons"), function(x) slot(x, "labpt"))
temp <- tibble(coords.x1 = temp[1,], coords.x2 = temp[2,])
identical(temp, as_tibble(sppts@coords))   # FALSE, but
temp - as_tibble(sppts@coords)             # The coordinate differences are minuscule, so OK :)



sppolyg.w15 <- rgeos::gBuffer(sppts, width = 15,                                # 30 x 30 meter 
                              quadsegs = 1, capStyle = "SQUARE", byid = T)

sppolyg.w30 <- rgeos::gBuffer(sppts, width = 30,                                # 60 x 60 meter 
                              quadsegs = 1, capStyle = "SQUARE", byid = T)



# Extract spectra = mean over square 25 x 25 m ---------------------------------------------------------

L8.BOA.w12.5 <- raster::extract(L8.BOA.st, sppolyg.w12.5, small = TRUE, fun = mean, na.rm = TRUE, 
                                df = TRUE, weights = TRUE, normalizeWeights = TRUE)

L8.BOA.w12.5 <- L8.BOA.w12.5 %>% mutate(ID = fieldXY[["ID"]]) 

write.csv2(L8.BOA.w12.5, "data/image_extracted_spectra/L8_BOA_w12_5.csv")

# Extract spectra = mean over square 30 x 30 m ---------------------------------------------------------

L8.BOA.w15 <- raster::extract(L8.BOA.st, sppolyg.w15, small = TRUE, fun = mean, na.rm = TRUE, 
                              df = TRUE, weights = TRUE, normalizeWeights = TRUE)

L8.BOA.w15 <- L8.BOA.w15 %>% mutate(ID = fieldXY[["ID"]]) 

write.csv2(L8.BOA.w15, "data/image_extracted_spectra/L8_BOA_w15.csv")

# Extract spectra = mean over square 60 x 60 m ---------------------------------------------------------

L8.BOA.w30 <- raster::extract(L8.BOA.st, sppolyg.w30, small = TRUE, fun = mean, na.rm = TRUE, 
                              df = TRUE, weights = TRUE, normalizeWeights = TRUE)

L8.BOA.w30 <- L8.BOA.w30 %>% mutate(ID = fieldXY[["ID"]]) 

write.csv2(L8.BOA.w30, "data/image_extracted_spectra/L8_BOA_w30.csv")

