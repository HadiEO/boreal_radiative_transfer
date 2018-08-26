# Export GTiff from SNAP is in multi-band format
myRaster <- brick("data/image_data/S2_BOA_subset/S2A_BOA_20m.tif")
# GTiff doesn't carry band names
# Check min,max to match band 
summary(myRaster, maxsamp = ncell(myRaster))  
# Ok, below names after matching with band stats in SNAP
names(myRaster) <- c("B2_490", "B3_560", "B4_665", "B5_705", "B6_740", "B7_783", 
                     "B8A_865", "B11_1610", "B12_2190")
# Write single bands (this takes some time)
writeRaster(myRaster, "data/image_data/S2_BOA_subset/S2A_BOA_20m_singles.tif", 
            bylayer = TRUE, suffix = names(myRaster))