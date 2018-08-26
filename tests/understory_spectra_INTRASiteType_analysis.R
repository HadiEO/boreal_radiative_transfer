# Read understory data -----------------------------------------
ustory.R <- read_csv2("data/understory/understory_Refl.csv")
# ! ustory.R differ for each plot

# Read understory type ----------------------------------------------------
ustory.type <- read_csv2("data/understory/understory_transects.csv")    
ustory.sites <- read_csv2("data/understory/understory_sites.csv")


# Resample spectra --------------------------------------------------------
ustory.Wav <- ustory.R$Wl

ustory.R.S2 <- ustory.R %>% dplyr::select(-Wl) %>% 
  map(my_spectralResampling, wavelength = ustory.Wav, sensor = "Sentinel2A")


# Add the site fertility  -----------------------------------------------
ustory.R.S2.df <- ldply(ustory.R.S2) %>% 
                  left_join(ustory.type, by = c(".id" = "Transect_id")) 


# Plot to check -----------------------------------------------------------
S2.Wav <- read_csv2("data/image_data/wav_S2.csv")
S2.Wav <- S2.Wav %>% dplyr::filter(!band %in% c("b1", "b8", "b9", "b10"))

ustory.speclib <- ustory.R.S2.df %>% dplyr::select(Blue:SWIR2) %>% as.matrix() %>% 
  speclib(wavelength = S2.Wav$wav_center)


# Save to disk ------------------------------------------------------------
write.csv2(ustory.R.S2.df, "data/understory/understory_Refl_S2.csv")

