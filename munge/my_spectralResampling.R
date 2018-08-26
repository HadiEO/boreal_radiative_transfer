
# Response can call the ones implemented internally, or provided by user for custom sensor


my_spectralResampling <- function(spectra, wavelength, sensor, response = NULL) {
  if(sensor == "Landsat8") {
    response <- read_csv2("munge/Landsat8_response.csv", col_types = cols())
  } else if(sensor == "Sentinel2A_old") {   # Previous relative spectral response before v.3. (19.12.2017)
    response <- read_csv2("munge/Sentinel2A_response_old.csv", col_types = cols())
    names(response) <- c("Wav", "Aerosol", "Blue", "Green", "Red", "RE1", "RE2", "RE3", 
                         "NIRwide", "NIRnarrow", "WV", "Cirrus", "SWIR1", "SWIR2")  # Rename to EM region
    response <- response %>% dplyr::filter(Wav <= 2500, Wav >= 400)  # Subset wavelength to PROSPECT spectra (ASD is from 350, but response are 0), the omitted wavelengths have response = 0
  } else if(sensor == "Sentinel2A_new") {
    response <- read_csv2("munge/Sentinel2A_response.csv", col_types = cols())
    names(response) <- c("Wav", "Aerosol", "Blue", "Green", "Red", "RE1", "RE2", "RE3", 
                         "NIRwide", "NIRnarrow", "WV", "Cirrus", "SWIR1", "SWIR2")  # Rename to EM region
    response <- response %>% dplyr::filter(Wav <= 2500, Wav >= 400)  # Subset wavelength to PROSPECT spectra (ASD is from 350, but response are 0), the omitted wavelengths have response = 0
  } else {
    response <- response  # Todo: user-provided response. If response empty and sensor not the two implemented, return error                       
  }


  names(response)[1] <- "Wav"
  wav.sensor <- response$Wav
  spectra.sensor <- spectra[which(wavelength %in% wav.sensor)]          # Strip wavelength to sensor wavelength range

  # Replace NA with 0 (caution!)
  spectra.sensor[is.na(spectra.sensor)] <- 0
  
  spectra.resampled <- response %>% dplyr::select(-Wav) %>% 
    map(function(z) sum(spectra.sensor * z) / sum(z)) %>% unlist
  
  if(sensor == "Sentinel2A") {
    spectra.resampled <- spectra.resampled[attr(spectra.resampled, "names") %in% 
                                             c("Blue", "Green", "Red", "RE1", "RE2", "RE3", "NIRnarrow", "SWIR1", "SWIR2")]
  } else if(sensor == "Landsat8") {
    spectra.resampled <- spectra.resampled[attr(spectra.resampled, "names") %in% 
                                             c("Blue", "Green", "Red", "NIR", "SWIR1", "SWIR2")]
  }

  
  return(spectra.resampled)            # Return a named vector. Names based on bands' names in response
}


# Debug
# spectra <- BRF
# wavelength <- attr(BRF, "Wav)
# sum(spectra.sensor * response$NIR) / sum(response$NIR)   # OK
# response = "Sentinel2A"



# If input speclib object -------------------------------------------------

my_spectralResampling_speclib <- function(spectra, sensor) {
  if(class(spectra) != "Speclib") stop("Input spectra must be a hsdar::speclib object")
  spc <- hsdar::spectra(spectra)
  spc <- t(spc)
  wav <- wavelength(spectra)
  
  spc.resampled <- adply(spc, 2, my_spectralResampling, wavelength = wav, sensor = sensor)
  if(sensor == "Sentinel2A") wav.resampled <- c(490,560,665,705,740,783,865,1610,2190)
  if(sensor == "Landsat8") wav.resampled <- c(482,561,655,865,1609,2201)
  
  spc.resampled.speclib <- hsdar::speclib(as.matrix(spc.resampled[,-1]),            # Remove first column is row index
                                wav.resampled)
  SI(spc.resampled.speclib) <- SI(spectra)

  return(spc.resampled.speclib)
}


# Debug
# spectra = otaniemi2016.broadleaf.speclib
# spectra = wL.copula.BIRCH.speclib
# test <- my_spectralResampling_speclib(spectra = otaniemi2016.broadleaf.speclib,
#                               sensor = "Landsat8")
# plot(test, new = FALSE, col = "red")
# sensor = "Sentinel2A"






