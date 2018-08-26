
# Import BOA images: common 20 m  -----------------------

folder.S2.BOA <- "data/image_data/S2_BOA_subset/"   
B2.490 <- raster(str_c(folder.S2.BOA, "S2A_BOA_20m_singles_B2_490.tif")); names(B2.490) <- "B2.490"
B3.560 <- raster(str_c(folder.S2.BOA, "S2A_BOA_20m_singles_B3_560.tif")); names(B3.560) <- "B3.560"
B4.665 <- raster(str_c(folder.S2.BOA, "S2A_BOA_20m_singles_B4_665.tif")); names(B4.665) <- "B4.665"
B5.705 <- raster(str_c(folder.S2.BOA, "S2A_BOA_20m_singles_B5_705.tif")); names(B5.705) <- "B5.705"
B6.740 <- raster(str_c(folder.S2.BOA, "S2A_BOA_20m_singles_B6_740.tif")); names(B6.740) <- "B6.740"
B7.783 <- raster(str_c(folder.S2.BOA, "S2A_BOA_20m_singles_B7_783.tif")); names(B7.783) <- "B7.783"
B8A.865 <- raster(str_c(folder.S2.BOA, "S2A_BOA_20m_singles_B8A_865.tif")); names(B8A.865) <- "B8A.865"
B11.1610 <- raster(str_c(folder.S2.BOA, "S2A_BOA_20m_singles_B11_1610.tif")); names(B11.1610) <- "B11.1610"
B12.2190 <- raster(str_c(folder.S2.BOA, "S2A_BOA_20m_singles_B12_2190.tif")); names(B12.2190) <- "B12.2190"

# Stack the bands
S2.BOA.st <- raster::stack(B2.490, 
                  B3.560,
                  B4.665,
                  B5.705,
                  B6.740,
                  B7.783,
                  B8A.865,
                  B11.1610,
                  B12.2190)

# Apply scale factor 
S2.BOA.st <- calc(S2.BOA.st, fun = function(z) z * 0.0001) 


# Import field plot points (UTM34N) -----------------------------
# --> Use shp
# shp <- readOGR(dsn="data/gis", layer="Hyytiala_LAI_2015_UTM34N")
# sppts <- SpatialPoints(shp@coords, proj4string=crs(img.20m), bbox = NULL)   # images loaded in next code section
# -- >or use csv
fieldXY <- read_csv2("data/forest/Hyytiala_LAI_2015_LauriFix.csv")    # Use coordinates fixed by Lauri 
sppts <- fieldXY %>% select(UTM34_E, UTM34_N) %>% SpatialPoints(proj4string = crs(S2.BOA.st), bbox = NULL)
# Check if fieldXY coords are in the same row order as sppts coords
identical(fieldXY[,c("UTM34_E", "UTM34_N")], as_tibble(sppts@coords))   # TRUE


# Make field plot square area -----------------------------------------

sppolyg.w12.5 <- rgeos::gBuffer(sppts, width = 12.5,                            # Make square 25 x 25 meter spatial polygon for each point
                                quadsegs = 1, capStyle = "SQUARE", byid = T)

# Check if sppolyg.w12.5 coords are in the same row order as sppts coords
temp <- sapply(slot(sppolyg.w12.5, "polygons"), function(x) slot(x, "labpt"))
temp <- tibble(UTM34_E = temp[1,], UTM34_N = temp[2,])
identical(temp, as_tibble(sppts@coords))   # FALSE, but
temp - as_tibble(sppts@coords)             # There's one plot with minuscule difference, so OK :)



sppolyg.w15 <- rgeos::gBuffer(sppts, width = 15,                                # 30 x 30 meter 
                              quadsegs = 1, capStyle = "SQUARE", byid = T)

sppolyg.w30 <- rgeos::gBuffer(sppts, width = 30,                                # 60 x 60 meter 
                              quadsegs = 1, capStyle = "SQUARE", byid = T)



# Extract spectra = mean over square 25 x 25 m ---------------------------------------------------------

S2.BOA.w12.5 <- raster::extract(S2.BOA.st, sppolyg.w12.5, small = TRUE, fun = mean, na.rm = TRUE, 
                           df = TRUE, weights = TRUE, normalizeWeights = TRUE)

S2.BOA.w12.5 <- S2.BOA.w12.5 %>% mutate(ID = fieldXY[["ID"]]) 

write.csv2(S2.BOA.w12.5, "data/image_extracted_spectra/S2_BOA_w12_5.csv")

# Extract spectra = mean over square 30 x 30 m ---------------------------------------------------------

S2.BOA.w15 <- raster::extract(S2.BOA.st, sppolyg.w15, small = TRUE, fun = mean, na.rm = TRUE, 
                                df = TRUE, weights = TRUE, normalizeWeights = TRUE)

S2.BOA.w15 <- S2.BOA.w15 %>% mutate(ID = fieldXY[["ID"]]) 

write.csv2(S2.BOA.w15, "data/image_extracted_spectra/S2_BOA_w15.csv")

# Extract spectra = mean over square 60 x 60 m ---------------------------------------------------------

S2.BOA.w30 <- raster::extract(S2.BOA.st, sppolyg.w30, small = TRUE, fun = mean, na.rm = TRUE, 
                              df = TRUE, weights = TRUE, normalizeWeights = TRUE)

S2.BOA.w30 <- S2.BOA.w30 %>% mutate(ID = fieldXY[["ID"]]) 

write.csv2(S2.BOA.w30, "data/image_extracted_spectra/S2_BOA_w30.csv")



