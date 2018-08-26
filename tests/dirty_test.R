# Change so can arbitrarily assign wL, Rg, Q as input
# wL and Rg in broadbands
# wL and Rg should have attribute wavelength

# 1st. test : universal wL and Rg
# 2nd. test : species-specific wL, and site fertility-specific Rg




# Debug                                    * To comment! ******************
# i = 15
# var = "BRF"
# ID = gaps.Nilson$ID[i]
# gaps1 = gaps.Nilson$gaps1[i]
# gaps2 = gaps.Nilson$gaps2[i]
# gaps3 = gaps.Nilson$gaps3[i]
# gaps4 = gaps.Nilson$gaps4[i]
# gaps5 = gaps.Nilson$gaps5[i]
# LAIeff =  gaps.Nilson$PAI_estimate[i]
# CI = NULL
# VZA = 0
# SZA = 20
# # wL = custom.wL
# wL = gaps.Nilson$wL.plot.S2A.sel[i]
# default.finnish.species = TRUE
# Prop_pine = gaps.Nilson$Prop_pine[i]
# Prop_spruce = gaps.Nilson$Prop_spruce[i]
# Prop_birch = gaps.Nilson$Prop_birch[i]
# Rg = custom.Rg



# Write the function ----------------------------------------------------------------

PARAS.arbitrary <- function(var = NULL, ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff, CI = NULL,                             
                  VZA, SZA, wL = NULL, 
                  default.finnish.species = TRUE, Prop_pine = NULL, Prop_spruce = NULL, Prop_birch = NULL,   # This is needed to calculate CI
                  Rg = NULL) {
  
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

  } else {
    CI <- CI                              # Arbitrary CI
  }
  
  LAItrue <- LAIeff / CI                # LAI corrected for shoot level clumping
  p <- 1 - (iD / LAItrue)               # analytical formula based on Stenberg (2007)
  
  
  # 3. Input C: leaf albedo wL   ***************************************************************
   wL <- wL                              # User-provided
   if(is.list(wL)) {
     wL <- unlist(wL)
     attr(wL, "Wav") <- names(wL)
   } 
   
  # 4. Input C: understory reflectance  ***************************************************************
   Rg <- Rg                              # User-provided
   if(is.list(Rg)) {
     Rg <- unlist(Rg)
     attr(Rg, "Wav") <- names(Rg)
   } 
  
  
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
    out <- BRF                                                  # Output
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
    out <- albedo                                              # Output
  } else {
    stop("var should be 'BRF' or 'albedo'")
  }
  
  
  # Prepare output to return
  out.ls <- as.list(out)
  names(out.ls) <- attr(out, "Wav")
  out.ls$ID <- ID
  out.ls <- as_tibble(out.ls)
  
  out.ls <- out.ls %>% mutate(cgf.view = cgf.view, i0.sun = i0.sun, DIFN = DIFN, CI = CI,
                                                LAItrue = LAItrue, p = p)
  
  return(out.ls)                                                   # This is a vector
  
}




