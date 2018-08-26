# Current implementation allows to vary spectral inputs only; wL and Rg
# This way, I can assign appropariate wL samples and Rg samples based on 
# species and site type of each LAI plot
# Yay, decided 20171108 11:43am

# NOT DONE 20171110

# Step 1: change wL
# (1) wL variation among means of species

# Step 2: change Rg
# (2) Rg variation among means of site type


# Debug -------------------------------------------------------------------




# PARAS function adapted for varyInput ------------------------------------

PARAS.simplified <- function(var = NULL, ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff, CI = NULL,                             
                  VZA, SZA, wL = NULL, default.finnish.species = TRUE,
                  Prop_pine = NULL, Prop_spruce = NULL, Prop_birch = NULL,
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
  # Must be user-provided wL

  
  # 4. Input C: understory reflectance  ***************************************************************
  # Must be user-supplied Rg

  

  
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
  
  return(out.resampled)                                                   # This is a vector
  
}








# Function calling PARAS.simplified above  -------------------------------------------

# Fun receives 1 input
PARAS.varyInputs <- function(forest.data, wL.samples, R_gr_hypers) {
              
  
  store.runs <- array(999, c(nrow(forest.data), ncol(wL.samples$Albedo)))
  
  fun.out <- list(input = forest.data,              # change here, PARAS() new code outputs 
                  b1 = list(s = store.runs),
                  b2 = list(s = store.runs),
                  b3 = list(s = store.runs),
                  b4 = list(s = store.runs),
                  b5 = list(s = store.runs),
                  b7 = list(s = store.runs))
  
  for (i in 1:ncol(wL.samples$Albedo)) {
    wL.sample <- tibble(Wavelength = wL.samples$Wavelength,      # 
                        Albedo = wL.samples$Albedo[,i])
    
    paras.out <-  PARAS(forest.data, wL_hypers = wL.sample, R_gr_hypers = R_gr_hypers, sensor = "Landsat7")
    
   fun.out$b1$s[,i] <- paras.out$BRF_b1_s
   fun.out$b2$s[,i] <- paras.out$BRF_b2_s
   fun.out$b3$s[,i] <- paras.out$BRF_b3_s
   fun.out$b4$s[,i] <- paras.out$BRF_b4_s
   fun.out$b5$s[,i] <- paras.out$BRF_b5_s
   fun.out$b7$s[,i] <- paras.out$BRF_b7_s
    
  }
  
  return(fun.out)
  
}



