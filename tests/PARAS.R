
# Variable names must be modified to match input data read in paras_main.R !
# 20170517 function modified to accept readily resampled spectra to desired sensor
# 20170525 function modified to calculate BRF for one forest plot at a time
# 20171026 function modified for better generalization: inputs are atomic
# see https://github.com/hectornieto/pyPro4Sail/blob/master/src/FourSAIL.py and
# https://github.com/hectornieto/pyPro4Sail/blob/master/src/pyPro4SAIL.py
# 20171105 function computes both BRF and spectral albedo



# Consideration of generalizing the function ------------------------------
# - Different optical sensors
# - Input leaf constituents (to simulate with prospect) or leaf albedo
# - Allows to give different weights to sun-exposed vs. shaded, current year vs older needles for spruce
# - Leaf albedo and soil spectra can be in different wavelength range and bandwidth,
# thus output spectral bands should adjust. RESAMPLE INPUT OR OUTPUT? OUTPUT. It's ok
# if soil spectra has to be resampled, later aggregated to broad bands. For now,
# resample to the spectra with shorter wavelength range
# - Make sure functions can run with varying leaf albedo and soil spectra,
# or any inputs actually (e.g. varying CI)
# - Include woody element
# if(!is.na(wood_to_plant_ratio)) {
#   wL <- (1 - wood_to_plant_ratio) * wL +                                   # if include woody element optical properties
#     wood_to_plant_ratio * R_wood.L7 }                                  # R_wood.L7 resampled already yo ETM bands, ought to be a named data frame       
# - Include understory diffuse component

# NOT DONE

# Description of inputs ---------------------------------------------------
# If inputs are multiple species, use a list
# cgf.view    single numeric
# cgf.sun     single numeric
# wL          vector numeric
# Rg          vector numeric
# Prop        [0,1]
# var         c("BRF", "albedo")
# scale.Albedo  (gaps1 / DIFN) * albedo
# BAI2LAI     currently implemented only for Finnish species
# NOT DONE


# Debug -------------------------------------------------------------------
# i <- 10
# gaps1 <- gaps$gaps1[i]
# gaps2 <- gaps$gaps2[i]
# gaps3 <- gaps$gaps3[i]
# gaps4 <- gaps$gaps4[i]
# gaps5 <- gaps$gaps5[i]
# LAIeff <- gaps$LAIeff[i]
# VZA <- 0
# SZA <- 20
# Prop_pine <- gaps$Prop_pine[i]
# Prop_spruce <- gaps$Prop_spruce[i]
# Prop_birch <- gaps$Prop_birch[i]
# Site_type_lab <- gaps$Site_type_lab[i]
# default.finnish.species = TRUE
# wL.by.PROSPECT = FALSE
# wL = NULL
# only.sunlit.wL = FALSE
# shaded.wL.weight = NULL
# spruce.currentYear = NULL
# Rg = NULL
# default.finnish.understory = TRUE
# resample = TRUE
# sensor = "Landsat8"
# ID <- gaps$ID[i]
# var = "BRF"
# scale.Albedo = FALSE
# spruce.currentYear = 1
# BAI2LAI <- list(Pine = 0.18, Spruce = 0.18, Birch = 0.15)
# output.as.df = TRUE
# output.intermediate = FALSE

# plot(vzas, cgfs, ylim = c(0,1), xlim = c(0,90))
# new <- data.frame(vzas = seq(0,90,by=10))
# lines(new$vzas, predict.lm(cgf.mod, newdata = new))



# Function ----------------------------------------------------------------

PARAS <- function(var = NULL, scale.Albedo = FALSE, ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff, CI = NULL,                             
                      VZA, SZA, wL = NULL, default.finnish.species = TRUE,
                      Prop_pine = NULL, Prop_spruce = NULL, Prop_birch = NULL, only.sunlit.wL = FALSE,
                      shaded.wL.weight = NULL, spruce.currentYear = NULL,
                      wL.by.PROSPECT = FALSE, N = NULL, Cab = NULL, Cw = NULL, Cm = NULL, Car = NULL, 
                      default.finnish.understory = TRUE, Site_type_lab = NULL, Rg = NULL,
                      BAI2LAI = NULL,
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
  # If user-provided wL
  if(!is.null(wL)) {
    wL <- wL                            # A vector, with wavelength attribute
  } 
  
  # If input leaf constituents to model leaf spectra
  if(wL.by.PROSPECT == TRUE) {
    # If Car used, use PROSPECT-5
    if(!is.null(Car))  {
      LRT <- Rprospect::prospect5(N = N, Cab = Cab, Car = Car, Cw = Cw, Cm = Cm) 
      }
    LRT <- Rprospect::prospect4(N = N, Cab = Cab, Cw = Cw, Cm = Cm)
    
    wL <- LRT$Reflectance + LRT$Transmittance
    attr(wL, "Wav") <- LRT$Wavelength                      # A vector, with wavelength attribute
  }
  
 
  # If input leaf albedo
  if(default.finnish.species == TRUE) {
    # Base case use species mean
    wL.finnish <- read_csv2("data/leaf/wL_bySpecies.csv", col_types = cols())  # Equal weights sunlit vs shaded, current vs older spruce needle
    
    if(only.sunlit.wL == TRUE) {
      wL.finnish <- read_csv2("data/leaf/wL_bySpecies_onlySunlit.csv", col_types = cols()) 
    }
    if(!is.null(shaded.wL.weight)) {
      wL.finnish.sunlit <- read_csv2("data/leaf/wL_bySpecies_onlySunlit.csv", col_types = cols()) 
      wL.finnish.shaded <- read_csv2("data/leaf/wL_bySpecies_onlyShaded.csv", col_types = cols()) 
      wL.finnish <- wL.finnish.shaded * shaded.wL.weight + wL.finnish.sunlit * (1 - shaded.wL.weight)
    } 
    
    if(!is.null(spruce.currentYear)) {  # If spruce wL not 50/50 current year & mature
      wL.finnish.sunlit <- read_csv2("data/leaf/wL_bySpecies_onlySunlit.csv", col_types = cols()) 
      wL.finnish.shaded <- read_csv2("data/leaf/wL_bySpecies_onlyShaded.csv", col_types = cols()) 
      temp <- read_csv2("data/leaf/wL_spruce_E0_E1.csv", col_types = cols())
      wL.finnish.sunlit$Spruce <- spruce.currentYear * temp$E0 + (1 - spruce.currentYear) * temp$E1
      wL.finnish <- wL.finnish.shaded * 0.5 + wL.finnish.sunlit * 0.5
    }
  
    if(!is.null(BAI2LAI)) {                         # Currently only implemented for Finnish species 
      Rstem <- read_csv2("data/stem/stem_refl_estonia.csv", col_types = cols())
      LAI2PAI <- map(BAI2LAI, function(z) 1 / (1 + z))
      BAI2PAI <- map(BAI2LAI, function(z) z * (1 / (1 + z)))
      # Here need to cut wL and Rg wavelength, ideally done when preparing them as inputs
      wL.finnish <- wL.finnish %>%  dplyr::filter(Wav >= 400, Wav <= 2400)                    
      # Weight in stem contribution
      wL.finnish$Pine <- (LAI2PAI$Pine * wL.finnish$Pine) + (BAI2PAI$Pine * Rstem$Pine)
      wL.finnish$Spruce <- (LAI2PAI$Spruce * wL.finnish$Spruce) + (BAI2PAI$Spruce * Rstem$Spruce)
      wL.finnish$Birch <- (LAI2PAI$Birch * wL.finnish$Birch) + (BAI2PAI$Birch * Rstem$Birch)
    }
    
    # Weight final wL by species      
    wL <- wL.finnish$Pine * Prop_pine + wL.finnish$Spruce * Prop_spruce + wL.finnish$Birch * Prop_birch   
    attr(wL, "Wav") <- wL.finnish$Wav           # A vector, with wavelength attribute
  }
  

  # 4. Input C: understory reflectance  ***************************************************************
  if(!is.null(Rg)) {                           # User-supplied Rg
    Rg <- Rg                                    
  } 
  
  if(default.finnish.understory == TRUE) {
    Rg.finnish <- read_csv2("data/understory/understory_Refl_3class.csv", col_types = cols())         # Read Rg of finnish species 
    # If using stem spectra (currently implements only Finnish species), need to cut wavelength
    if(!is.null(BAI2LAI)) Rg.finnish <- Rg.finnish %>% dplyr::filter(Wl >= 400, Wl <= 2400)   
    Rg <- Rg.finnish[[Site_type_lab]]           # A vector, with wavelength attribute
    attr(Rg, "Wav") <- Rg.finnish$Wl
  } 
      
  # Check if need to resample soil spectra to leaf albedo wavelength, or vice versa
  wL.wav <- attr(wL, "Wav")
  Rg.wav <- attr(Rg, "Wav")
  # if(!identical(wL.wav, Rg.wav)) stop("Soil spectra and leaf spectra doesn't have the same wavelengths")
  # NOT YET IMPLEMENTED
  # wL.wav.range <- range(wL.wav)[2]-range(wL.wav)[1]
  # Rg.wav.range <- range(Rg.wav)[2]-range(Rg.wav)[1]
  # if(!identical(wL.wav, Rg.wav)){
  #   if(Rg.wav .range <  wL.wav.range) {
  #     # Need function to resample spectra based on wavelength of the target spectra
  #   } else {
  #     
  #   }
  # }


  # 5. Calculation A: canopy scattering coefficient   *******************************************************
  wC <-  (wL - p * wL) / (1 - p * wL)
  attr(wC, "Wav") <- attr(wL, "Wav")
  alpha.C <- 1 - wC                             # canopy absorption coefficient
  attr(alpha.C, "Wav") <- attr(wL, "Wav")

  # 6. Calculation B: fraction of backward scatter Q based on Mottus & Stenberg (2008)   **********************
  q <- 1 - exp(-0.1684 * LAItrue)                                    
  Q <- 0.5 + (0.5 * q) * ((1 - (p * wL)) / (1 - (p * q * wL)))
    

  # 7. Main calculation: BRF or albedo
  if(var == "BRF") {
    BRF <- (cgf.view * Rg * (1 - i0.sun)) + (Q * i0.sun * wC) 
    attr(BRF, "Wav") <- attr(wL, "Wav")
    out <- BRF
  } else if(var == "albedo") {
    t.bs <- t0.sun + i0.sun * (1 - Q) * wC                      # Total transmittance
    
    Ac.bs <- i0.sun * alpha.C
    Ac.s <- (t.bs * Rg * iD * alpha.C) / (1 - Q * wC * iD * Rg) # Sum of geometric series = a / (1 - b); where a is scale factor, b is common ratio
    Ac <- Ac.bs + Ac.s
    
    Ag.bs <-  t.bs * (1 - Rg)
    Ag.s <- (t.bs * Rg * iD * Q * wC * (1 -  Rg)) / (1 - Q * wC * iD * Rg)        # same common ratio as Ac.s
    Ag <- Ag.bs + Ag.s
    
    albedo <- 1 - Ac - Ag
    attr(albedo, "Wav") <- attr(wL, "Wav")
    
    # if(scale.Albedo == TRUE) {
    #   albedo <- (0.066*gaps1 / DIFN) * albedo
    # }
    
    out <- albedo
  } else {
    stop("var should be 'BRF' or 'albedo'")
  }
  
  
  
  if(resample == TRUE) {
    out.resampled <- my_spectralResampling(out, attr(out, "Wav"), sensor = sensor)
  }
  
  if(output.as.df == TRUE) {
    out.resampled <- as_tibble(as.list(out.resampled))
    out.resampled$ID <- ID

    if(output.intermediate == TRUE) {
      out.resampled <- out.resampled %>% mutate(cgf.view = cgf.view, i0.sun = i0.sun, DIFN = DIFN, CI = CI,
                            LAItrue = LAItrue, p = p)   
    }
  }
  
  return(out.resampled)                               # If output.as.df = TRUE, this is a one-row tibble; otherwise a named vector
                                                      
}



#***********************************************************************
# Add output --------------------------------------------------------------
#***********************************************************************
PARAS.addOutput <- function(var = NULL, scale.Albedo = FALSE, ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff, CI = NULL,                             
                  VZA, SZA, wL = NULL, default.finnish.species = TRUE,
                  Prop_pine = NULL, Prop_spruce = NULL, Prop_birch = NULL, only.sunlit.wL = FALSE,
                  shaded.wL.weight = NULL, spruce.currentYear = NULL,
                  wL.by.PROSPECT = FALSE, N = NULL, Cab = NULL, Cw = NULL, Cm = NULL, Car = NULL, 
                  default.finnish.understory = TRUE, Site_type_lab = NULL, Rg = NULL,
                  BAI2LAI = NULL,
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
  # If user-provided wL
  if(!is.null(wL)) {
    wL <- wL                            # A vector, with wavelength attribute
  } 
  
  # If input leaf constituents to model leaf spectra
  if(wL.by.PROSPECT == TRUE) {
    # If Car used, use PROSPECT-5
    if(!is.null(Car))  {
      LRT <- Rprospect::prospect5(N = N, Cab = Cab, Car = Car, Cw = Cw, Cm = Cm) 
    }
    LRT <- Rprospect::prospect4(N = N, Cab = Cab, Cw = Cw, Cm = Cm)
    
    wL <- LRT$Reflectance + LRT$Transmittance
    attr(wL, "Wav") <- LRT$Wavelength                      # A vector, with wavelength attribute
  }
  
  
  # If input leaf albedo
  if(default.finnish.species == TRUE) {
    # Base case use species mean
    wL.finnish <- read_csv2("data/leaf/wL_bySpecies.csv", col_types = cols())  # Equal weights sunlit vs shaded, current vs older spruce needle
    
    if(only.sunlit.wL == TRUE) {
      wL.finnish <- read_csv2("data/leaf/wL_bySpecies_onlySunlit.csv", col_types = cols()) 
    }
    if(!is.null(shaded.wL.weight)) {
      wL.finnish.sunlit <- read_csv2("data/leaf/wL_bySpecies_onlySunlit.csv", col_types = cols()) 
      wL.finnish.shaded <- read_csv2("data/leaf/wL_bySpecies_onlyShaded.csv", col_types = cols()) 
      wL.finnish <- wL.finnish.shaded * shaded.wL.weight + wL.finnish.sunlit * (1 - shaded.wL.weight)
    } 
    
    if(!is.null(spruce.currentYear)) {  # If spruce wL not 50/50 current year & mature
      wL.finnish.sunlit <- read_csv2("data/leaf/wL_bySpecies_onlySunlit.csv", col_types = cols()) 
      wL.finnish.shaded <- read_csv2("data/leaf/wL_bySpecies_onlyShaded.csv", col_types = cols()) 
      temp <- read_csv2("data/leaf/wL_spruce_E0_E1.csv", col_types = cols())
      wL.finnish.sunlit$Spruce <- spruce.currentYear * temp$E0 + (1 - spruce.currentYear) * temp$E1
      wL.finnish <- wL.finnish.shaded * 0.5 + wL.finnish.sunlit * 0.5
    }
    
    if(!is.null(BAI2LAI)) {                         # Currently only implemented for Finnish species 
      Rstem <- read_csv2("data/stem/stem_refl_estonia.csv", col_types = cols())
      LAI2PAI <- map(BAI2LAI, function(z) 1 / (1 + z))
      BAI2PAI <- map(BAI2LAI, function(z) z * (1 / (1 + z)))
      # Here need to cut wL and Rg wavelength, ideally done when preparing them as inputs
      wL.finnish <- wL.finnish %>%  dplyr::filter(Wav >= 400, Wav <= 2400)                    
      # Weight in stem contribution
      wL.finnish$Pine <- (LAI2PAI$Pine * wL.finnish$Pine) + (BAI2PAI$Pine * Rstem$Pine)
      wL.finnish$Spruce <- (LAI2PAI$Spruce * wL.finnish$Spruce) + (BAI2PAI$Spruce * Rstem$Spruce)
      wL.finnish$Birch <- (LAI2PAI$Birch * wL.finnish$Birch) + (BAI2PAI$Birch * Rstem$Birch)
    }
    
    # Weight final wL by species      
    wL <- wL.finnish$Pine * Prop_pine + wL.finnish$Spruce * Prop_spruce + wL.finnish$Birch * Prop_birch   
    attr(wL, "Wav") <- wL.finnish$Wav           # A vector, with wavelength attribute
  }
  
  
  # 4. Input C: understory reflectance  ***************************************************************
  if(!is.null(Rg)) {                           # User-supplied Rg
    Rg <- Rg                                    
  } 
  
  if(default.finnish.understory == TRUE) {
    Rg.finnish <- read_csv2("data/understory/understory_Refl_3class.csv", col_types = cols())         # Read Rg of finnish species 
    # If using stem spectra (currently implements only Finnish species), need to cut wavelength
    if(!is.null(BAI2LAI)) Rg.finnish <- Rg.finnish %>% dplyr::filter(Wl >= 400, Wl <= 2400)   
    Rg <- Rg.finnish[[Site_type_lab]]           # A vector, with wavelength attribute
    attr(Rg, "Wav") <- Rg.finnish$Wl
  } 
  
  # Check if need to resample soil spectra to leaf albedo wavelength, or vice versa
  wL.wav <- attr(wL, "Wav")
  Rg.wav <- attr(Rg, "Wav")
  # if(!identical(wL.wav, Rg.wav)) stop("Soil spectra and leaf spectra doesn't have the same wavelengths")
  # NOT YET IMPLEMENTED
  # wL.wav.range <- range(wL.wav)[2]-range(wL.wav)[1]
  # Rg.wav.range <- range(Rg.wav)[2]-range(Rg.wav)[1]
  # if(!identical(wL.wav, Rg.wav)){
  #   if(Rg.wav .range <  wL.wav.range) {
  #     # Need function to resample spectra based on wavelength of the target spectra
  #   } else {
  #     
  #   }
  # }
  
  
  # 5. Calculation A: canopy scattering coefficient   *******************************************************
  wC <-  (wL - p * wL) / (1 - p * wL)
  attr(wC, "Wav") <- attr(wL, "Wav")
  alpha.C <- 1 - wC                             # canopy absorption coefficient
  attr(alpha.C, "Wav") <- attr(wL, "Wav")
  
  # 6. Calculation B: fraction of backward scatter Q based on Mottus & Stenberg (2008)   **********************
  q <- 1 - exp(-0.1684 * LAItrue)                                    
  Q <- 0.5 + (0.5 * q) * ((1 - (p * wL)) / (1 - (p * q * wL)))
  
  # Added 2018.05.19
  # Canopy contribution
  canopy.contribution <- Q * i0.sun * wC
  attr(canopy.contribution, "Wav") <- attr(wL, "Wav")
  
  
  # 7. Main calculation: BRF or albedo
  if(var == "BRF") {
    BRF <- (cgf.view * Rg * (1 - i0.sun)) + (Q * i0.sun * wC) 
    attr(BRF, "Wav") <- attr(wL, "Wav")
    out <- BRF
  } else if(var == "albedo") {
    t.bs <- t0.sun + i0.sun * (1 - Q) * wC                      # Total transmittance
    
    Ac.bs <- i0.sun * alpha.C
    Ac.s <- (t.bs * Rg * iD * alpha.C) / (1 - Q * wC * iD * Rg) # Sum of geometric series = a / (1 - b); where a is scale factor, b is common ratio
    Ac <- Ac.bs + Ac.s
    
    Ag.bs <-  t.bs * (1 - Rg)
    Ag.s <- (t.bs * Rg * iD * Q * wC * (1 -  Rg)) / (1 - Q * wC * iD * Rg)        # same common ratio as Ac.s
    Ag <- Ag.bs + Ag.s
    
    albedo <- 1 - Ac - Ag
    attr(albedo, "Wav") <- attr(wL, "Wav")
    
    # if(scale.Albedo == TRUE) {
    #   albedo <- (0.066*gaps1 / DIFN) * albedo
    # }
    
    out <- albedo
  } else {
    stop("var should be 'BRF' or 'albedo'")
  }
  
  
  
  if(resample == TRUE) {
    out.resampled <- my_spectralResampling(out, attr(out, "Wav"), sensor = sensor)
    wC.resampled <- my_spectralResampling(wC, attr(out, "Wav"), sensor = sensor)   # Added 2018.05.19
    canopy.contribution.resampled <- my_spectralResampling(canopy.contribution, attr(out, "Wav"), sensor = sensor)   # Added 2018.05.19
  }
  
  if(output.as.df == TRUE) {
    out.resampled <- as_tibble(as.list(out.resampled))
    out.resampled$ID <- ID
    
    wC.resampled <- as_tibble(as.list(wC.resampled))    # Added wC 2018.05.19
    canopy.contribution.resampled <- as_tibble(as.list(canopy.contribution.resampled))    # Added 2018.05.19
    
    if(sensor == "Landsat8") {
      names(wC.resampled) <- c("wC.Blue", "wC.Green", "wC.Red", "wC.NIR", "wC.SWIR1", "wC.SWIR2")
      names(canopy.contribution.resampled) <- c("can.Blue", "can.Green", "can.Red", "can.NIR", "can.SWIR1", "can.SWIR2")
    } else {
      names(wC.resampled) <- c("wC.Blue", "wC.Green", "wC.Red", "wC.RE1", "wC.RE2", "wC.RE3", "wC.NIRnarrow", "wC.SWIR1", "wC.SWIR2")
      names(canopy.contribution.resampled) <- c("can.Blue", "can.Green", "can.Red", "can.RE1", "can.RE2", "can.RE3", "can.NIRnarrow", "can.SWIR1", "can.SWIR2")
    }
    
    out.resampled <- bind_cols(out.resampled, wC.resampled, canopy.contribution.resampled)
    
    if(output.intermediate == TRUE) {
      out.resampled <- out.resampled %>% mutate(cgf.view = cgf.view, i0.sun = i0.sun, DIFN = DIFN, CI = CI,
                                                LAItrue = LAItrue, p = p)   
    }
  }
  
  return(out.resampled)                               # If output.as.df = TRUE, this is a one-row tibble; otherwise a named vector
  
}