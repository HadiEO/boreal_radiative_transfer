# Read data
otaniemi2016.conifer <- read_csv2("data/leaf/from_Aarne/coniferous_mean_albedo.csv")
otaniemi2016.broadleaf <- read_csv2("data/leaf/from_Aarne/broadleaved_mean_albedo_addNA.csv")

# Make into speclib
otaniemi2016.conifer.speclib <- otaniemi2016.conifer %>% 
  dplyr::select(-Wavelength) %>% t() %>% 
  hsdar::speclib(., otaniemi2016.conifer$Wavelength, 
                 SI = data.frame(species.name = as.character(colnames(otaniemi2016.conifer)[-1]),
                                 leaf.type = "coniferous")) 

otaniemi2016.broadleaf.speclib <- otaniemi2016.broadleaf %>% 
  dplyr::select(-Wavelength) %>% t() %>% 
  hsdar::speclib(., otaniemi2016.broadleaf$Wavelength,
                 SI = data.frame(species.name = as.character(colnames(otaniemi2016.broadleaf)[-1]),
                                 leaf.type = "broadleaved")) 


# Write to disk as speclib object (.rds)
write_rds(otaniemi2016.conifer.speclib, "data/leaf/from_Aarne/otaniemi2016_conifer.rds")
write_rds(otaniemi2016.broadleaf.speclib, "data/leaf/from_Aarne/otaniemi2016_broadleaf.rds")



# Prepare for GSA
# Resample to S2A
otaniemi2016.conifer.speclib.S2A <- 
  my_spectralResampling_speclib(spectra = otaniemi2016.conifer.speclib,
                                sensor = "Sentinel2A")

otaniemi2016.broadleaf.speclib.S2A <- 
  my_spectralResampling_speclib(spectra = otaniemi2016.broadleaf.speclib,
                                sensor = "Sentinel2A")

# Conifer without larx
otaniemi2016.coniferNoLarx.speclib.S2A <- 
  my_spectralResampling_speclib(
    spectra = subset(otaniemi2016.conifer.speclib, !species.name %in% c("Larix_gmelinii_albedo", "Larix_laricina_albedo", "Larix_sibirica_albedo")),
    sensor = "Sentinel2A")


# Get min and max
otaniemi2016.conifer.forGSA <- spectra(otaniemi2016.conifer.speclib.S2A) %>% as_tibble %>% 
  set_colnames(c("Blue", "Green", "Red", "RE1", "RE2", "RE3", "NIRnarrow", "SWIR1", "SWIR2")) %>% 
  summarize_all(funs(min, max))

otaniemi2016.broadleaf.forGSA <- spectra(otaniemi2016.broadleaf.speclib.S2A) %>% as_tibble %>% 
  set_colnames(c("Blue", "Green", "Red", "RE1", "RE2", "RE3", "NIRnarrow", "SWIR1", "SWIR2")) %>% 
  summarize_all(funs(min, max))

write.csv2(otaniemi2016.conifer.forGSA, "data/leaf/from_Aarne/otaniemi2016_conifer_forGSA.csv")
write.csv2(otaniemi2016.broadleaf.forGSA, "data/leaf/from_Aarne/otaniemi2016_broadleaf_forGSA.csv")


# Conifer without larx
otaniemi2016.coniferNoLarx.forGSA <- spectra(otaniemi2016.coniferNoLarx.speclib.S2A) %>% as_tibble %>% 
  set_colnames(c("Blue", "Green", "Red", "RE1", "RE2", "RE3", "NIRnarrow", "SWIR1", "SWIR2")) %>% 
  summarize_all(funs(min, max))
write.csv2(otaniemi2016.coniferNoLarx.forGSA, "data/leaf/from_Aarne/otaniemi2016_coniferNoLarx_forGSA.csv")


# Merge the two speclib (coniferous and broadleaved), need to make wavelength range same
range(wavelength(otaniemi2016.conifer.speclib))
range(wavelength(otaniemi2016.broadleaf.speclib))   # beyond 400-2300 nm are NA, so can't model satellite SWIR2

# mask(otaniemi2016.conifer.speclib) <- c(350, 399, 2301, 2500)   # mask() keeps wavelength range and replace values with NA
# range(wavelength(otaniemi2016.conifer.speclib))
# NOT DONE, there's no built-in method to subset by wavelength


# Plot mean, min, max spectra
par(mfrow = c(1,2))
hsdar::plot(otaniemi2016.conifer.speclib, FUN = "mean", lty = 2, ylim = c(0,1), main = "Coniferous")
hsdar::plot(otaniemi2016.conifer.speclib, new = FALSE, FUN = "min", lty = 2)
hsdar::plot(otaniemi2016.conifer.speclib, new = FALSE, FUN = "max", lty = 2)

hsdar::plot(otaniemi2016.broadleaf.speclib, FUN = "mean", lty = 2, ylim = c(0,1), main = "Broadleaved")
hsdar::plot(otaniemi2016.broadleaf.speclib, new = FALSE, FUN = "min", lty = 2)
hsdar::plot(otaniemi2016.broadleaf.speclib, new = FALSE, FUN = "max", lty = 2)

# Plot spectra by species name
SI(otaniemi2016.broadleaf.speclib)
plot(subset(otaniemi2016.broadleaf.speclib, species.name == "Betula_pendula_albedo"))
plot(subset(otaniemi2016.broadleaf.speclib, species.name == "Populus_tremuloides_albedo"), new = FALSE)

# Plot all species
plot(subset(otaniemi2016.conifer.speclib, species.name == "Abies_balsamea_albedo"), ylim = c(0,1))
plot(subset(otaniemi2016.conifer.speclib, species.name == "Abies_sibirica_albedo"), new = FALSE)
plot(subset(otaniemi2016.conifer.speclib, species.name == "Larix_gmelinii_albedo"), new = FALSE, col = "red")
plot(subset(otaniemi2016.conifer.speclib, species.name == "Larix_laricina_albedo"), new = FALSE, col = "red")
plot(subset(otaniemi2016.conifer.speclib, species.name == "Larix_sibirica_albedo"), new = FALSE, col = "red")
plot(subset(otaniemi2016.conifer.speclib, species.name == "Picea_abies_albedo"), new = FALSE)
plot(subset(otaniemi2016.conifer.speclib, species.name == "Picea_glauca_albedo"), new = FALSE)
plot(subset(otaniemi2016.conifer.speclib, species.name == "Picea_mariana_albedo"), new = FALSE)
plot(subset(otaniemi2016.conifer.speclib, species.name == "Pinus_banksiana_albedo"), new = FALSE)
plot(subset(otaniemi2016.conifer.speclib, species.name == "Pinus_contorta_albedo"), new = FALSE)
plot(subset(otaniemi2016.conifer.speclib, species.name == "Pinus_sylvestris_albedo"), new = FALSE)
plot(subset(otaniemi2016.conifer.speclib, species.name == "Pseudotsuga_menziesii_albedo"), new = FALSE)



