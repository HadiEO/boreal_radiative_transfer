# get.sensor.characteristics(0) # Cool, there's Sentinel2

spc_resample_S2 <- function(spectra, wav) {

  spclib <- hsdar::speclib(spectra, wav)
  spclib_S2 <- hsdar::spectral.resampling(spclib, sensor = "Sentinel2")
  spc_sensor <- spectra(spclib_S2)
  spc_sensor <- data.frame(b1=spc_sensor[1,1], b2=spc_sensor[1,2], b3=spc_sensor[1,3], b4=spc_sensor[1,4], 
                           b5=spc_sensor[1,5], b6=spc_sensor[1,6], b7=spc_sensor[1,7], b8=spc_sensor[1,8],
                           b8a=spc_sensor[1,9], b9=spc_sensor[1,10], b10=spc_sensor[1,11], 
                           b11=spc_sensor[1,12], b12=spc_sensor[1,13])
  return(spc_sensor)
}

# test (to delete after)
# spectra <- wL.pine
# sensor = "Sentinel2"

spc_resample_L8 <- function(spectra, wav) {

  spclib <- hsdar::speclib(spectra, wav)
  spclib_S2 <- hsdar::spectral.resampling(spclib, sensor = "Landsat8")
  spc_sensor <- spectra(spclib_S2)
  spc_sensor <- data.frame(b1=spc_sensor[1,1], b2=spc_sensor[1,2], b3=spc_sensor[1,3], b4=spc_sensor[1,4], 
                           b5=spc_sensor[1,5], b6=spc_sensor[1,6], b7=spc_sensor[1,7], b8=spc_sensor[1,8],
                           b8a=spc_sensor[1,9], b9=spc_sensor[1,10], b10=spc_sensor[1,11], 
                           b11=spc_sensor[1,12], b12=spc_sensor[1,13])
  return(spc_sensor)
}