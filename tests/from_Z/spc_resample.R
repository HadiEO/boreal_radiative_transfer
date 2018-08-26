# source("cache/libPaths.R")
require(hsdar)


spc_resample <- function(spc_hypers, spc, wav, sensor = "Landsat7") {
  spectra <- spc_hypers %>% collect() %>% .[[spc]]
  wav <- spc_hypers %>%  collect() %>% .[[wav]]
  spclib <- speclib(spectra, wav)
  spclib_L7 <- spectral.resampling(spclib, sensor)
  spc_sensor <- spclib_L7@spectra
  spc_sensor <- data.frame(b1=spc_sensor[1,1], b2=spc_sensor[1,2], b3=spc_sensor[1,3], b4=spc_sensor[1,4], 
                               b5=spc_sensor[1,5], b7=spc_sensor[1,6])
  return(spc_sensor)
}

