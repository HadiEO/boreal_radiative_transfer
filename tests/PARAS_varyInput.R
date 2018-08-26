# Current implementation allows to vary spectral inputs only; wL and Rg, one at a time
# This way, I can assign appropariate wL samples and Rg samples based on 
# species and site type of each LAI plot
# Yay, decided 20171108 11:43am
# Need to drop SWIR2 cause broadleaf spectra used from Otaniemi2016 campaign is up to 2300nm (satellite SWIR2 is up to 2350nm)

# NOT DONE 20171110

# Step 1: change wL (INPUT AS SPECLIB OBJECT)
# (1) wL variation among means of species

# Step 2: change Rg (INPUT AS SPECLIB OBJECT)
# (2) Rg variation among means of site type

# Coding recipe
# Make speclib object simply hsdar::speclib(spectra, wavelength); spectra stored as one row = one sample, column = wavelength
# nspectra()




# Debug -------------------------------------------------------------------
# wL <-  read_rds("data/leaf/from_Aarne/otaniemi2016_broadleaf.rds")
# i <- 8
# gaps.Nilson <- gaps.Nilson %>% mutate(LAIeff = PAI_estimate)
# gaps1 <- gaps.Nilson$gaps1[i]
# gaps2 <- gaps.Nilson$gaps2[i]
# gaps3 <- gaps.Nilson$gaps3[i]
# gaps4 <- gaps.Nilson$gaps4[i]
# gaps5 <- gaps.Nilson$gaps5[i]
# LAIeff <- gaps.Nilson$LAIeff[i]
# VZA <- 0
# SZA <- 41.33
# Prop_pine <- gaps.Nilson$Prop_pine[i]
# Prop_spruce <- gaps.Nilson$Prop_spruce[i]
# Prop_birch <- gaps.Nilson$Prop_birch[i]
# Site_type_lab <- gaps.Nilson$Site_type_lab[i]
# default.finnish.species = TRUE
# default.finnish.understory = TRUE
# resample = TRUE
# sensor = "Sentinel2A"
# ID = gaps.Nilson$ID[i]
# var = "BRF"
# output.as.df = TRUE
# output.intermediate = TRUE



# if default.finnish.species = FALSE, the STAR values will not be used, and user needs to supply CI
# if default.finnish.species = TRUE, and wL = NULL, mean spectra of pine, spruce, and birch in database will be used, and weight by basal area proportion,
# otherwise if wL != NULL, user-supplied wL will be used



# PARAS function adapted for varyInput ------------------------------------

PARAS.varyInput <- function(var = NULL, ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff, CI = NULL,                             
                             VZA, SZA, wL = NULL, 
                            default.finnish.species = TRUE, Prop_pine = NULL, Prop_spruce = NULL, Prop_birch = NULL, 
                            default.finnish.understory = TRUE, Site_type_lab = NULL, Rg = NULL,
                             resample = FALSE, sensor = NULL,
                             output.as.df = TRUE, output.intermediate = FALSE) {
  
  # 1. Input A: Sensor-view geometry ***************************************************************
  # Fit 2nd order polynomial 
  cgfs <- c(gaps1, gaps2, gaps3, gaps4, gaps5)
  vzas <- c(7, 23, 38, 53, 68) # Rautiainen & Stenberg (2015)   
  cgf.mod <- lm(cgfs ~ vzas + I(vzas^2))
  
  # cgf.view <- predict(cgf.mod, tibble(vzas = VZA))        # Interpolated, not simply first ring for nadir view
  cgf.view <- gaps1
  cgf.sun <- predict(cgf.mod, tibble(vzas = SZA))
  t0.sun <- cgf.sun
  i0.sun <- 1 - t0.sun
  
  # 2. Input B: Canopy structure  ***************************************************************
  DIFN <- 0.066*gaps1 + 0.189*gaps2 + 0.247*gaps3 + 0.249*gaps4 + 0.249*gaps5  
  iD <- 1 - DIFN
  
  if(default.finnish.species == TRUE) { 
    STAR.pine <- 0.147 # see ref in Majasalmi et al. (2016)
    STAR.spruce <- 0.161 # see ref in Majasalmi et al. (2016)
    Beta.pine <- 4*STAR.pine
    Beta.spruce <- 4*STAR.spruce
    Beta.birch <- 1
    CI <- Beta.pine * Prop_pine + Beta.spruce * Prop_spruce + Beta.birch * Prop_birch
    # Todo: apply crown-level clumping
  } else {
    CI <- CI
  }
  
  LAItrue <- LAIeff / CI                # LAI corrected for shoot level clumping
  p <- 1 - (iD / LAItrue)               # analytical formula based on Stenberg (2007)
  
  
  # 3. Input C: leaf albedo wL   ***************************************************************
  if(is.null(wL)) {                       # If user does not supply wL (speclib object)   **************************************
    # Base case use species mean
    wL.finnish <- read_csv2("data/leaf/wL_bySpecies.csv", col_types = cols())  # Equal weights sunlit vs shaded, current vs older spruce needle
    
    # Weight wL by species      
    wL <- wL.finnish$Pine * Prop_pine + wL.finnish$Spruce * Prop_spruce + wL.finnish$Birch * Prop_birch   
    attr(wL, "Wav") <- wL.finnish$Wav           # A vector, with wavelength attribute
    wL.wav <-  attr(wL, "Wav")
  } else {
    wL <- wL                             # User-supplied wL (speclib object)
    # Extract spectra matrix and wavelength vector from speclib object wL
    
    
    wL.spc <- hsdar::spectra(wL)  # Each row = one spectra
    wL.spc.t <- t(wL.spc)         # Transpose so each col = one spectra
    wL.wav <- hsdar::wavelength(wL)  
  }
  

  # 4. Input C: understory reflectance  ***************************************************************
  if(default.finnish.understory == TRUE) {
    Rg.finnish <- read_csv2("data/understory/understory_Refl_3class.csv", col_types = cols())         # Read Rg of finnish species 
    Rg <- Rg.finnish[[Site_type_lab]]           # A vector, with wavelength attribute
    attr(Rg, "Wav") <- Rg.finnish$Wl
    Rg.wav <- attr(Rg, "Wav")
  } else if(!is.null(Rg)) {
    Rg <- Rg                                         # If user supplies Rg (speclib object)  **************************************
    # Extract spectra matrix and wavelength vector from speclib object wL
    Rg.spc <- hsdar::spectra(Rg)  # Each row = one spectra
    Rg.spc.t <- t(Rg.spc)         # Transpose so each col = one spectra
    Rg.wav <- hsdar::wavelength(Rg)  
    }
  
  
  # 5,6,7. The rest of the calculaton made into a separate function to be called when looping over wL or Rg
  
  # Make sure wL and Rg has the same wavelength range (Rg cut to wL)
  # idx <- which(Rg.wav %in% wL.wav)
  # Rg <- Rg[idx]
  # attr(Rg, "Wav") <- Rg.wav[idx]
  # Rg.wav <- attr(Rg, "Wav")                                                # Update Rg.wav!
  

  # Loop over wL or Rg **************************************
  source("tests/calc_PARAS.R")
  calc.PARAS.out <- as_tibble(wL.spc.t) %>% 
    map_df(.f = calc.PARAS, Rg = Rg,                                 # Define wL or Rg as additional argument to tell they are fixed input
           wL.wav = wL.wav, Rg.wav = Rg.wav,                         # The rest inputs to calc.PARAS below are global variables within PARAS.varyInput function call
           p = p, LAItrue = LAItrue, 
           var = var, cgf.view = cgf.view,
           i0.sun = i0.sun, t0.sun = t0.sun, iD = iD, DIFN = DIFN, CI = CI,
           resample = resample, sensor = sensor, ID = ID,
           output.as.df = output.as.df, output.intermediate = output.intermediate)       
                                           
  
  return(calc.PARAS.out)         # If output.as.df = TRUE, this is a one-row tibble; otherwise a named vector
                                # Thus, for varied input case, the tibble columns storing BRFs are lists of length
                                # = number of inputs e.g. no. of wL or Rg
}

