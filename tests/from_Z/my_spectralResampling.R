# Landsat_7_response <- read.csv2("R_GSA/script/hsdar/data/Landsat_7_response.csv")
# Landsat_7_response <- apply(Landsat_7_response, 2, as.numeric)
# Landsat_7_wav <- seq(434, 2401, 1)


my_spectralResampling <- function(wL.hypers, wav.hypers, response = Landsat_7_response, wav = Landsat_7_wav) {
  
  # Landsat_7_response ===== [1:1968, 1:6]
  # Landsat_7_wav ====== seq(434,2401,1)
  # wL.hypers ========== [1:2101]
  # wav.hypers ========= seq(400,2500,1)
  
  Landsat_7_response <- read.csv2("R_GSA/script/hsdar/data/Landsat_7_response.csv")
  Landsat_7_response <- apply(Landsat_7_response, 2, as.numeric)
  Landsat_7_wav <- seq(434, 2401, 1)
  
  wL.hypers.L7 <- wL.hypers[which(wav.hypers %in% wav)]          # Strip the PROSPECT wavelength to Landsat 7 wavelength range
  
  out <- list(
    b1 = sum(wL.hypers.L7 * response[,1]) / sum(response[,1]),
    b2 = sum(wL.hypers.L7 * response[,2]) / sum(response[,2]),
    b3 = sum(wL.hypers.L7 * response[,3]) / sum(response[,3]),
    b4 = sum(wL.hypers.L7 * response[,4]) / sum(response[,4]),
    b5 = sum(wL.hypers.L7 * response[,5]) / sum(response[,5]),
    b7 = sum(wL.hypers.L7 * response[,6]) / sum(response[,6])
    )
  
  return(out)
}
