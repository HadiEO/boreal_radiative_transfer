calc.PARAS <- function(wL = NULL, Rg = NULL,
                       wL.wav, Rg.wav, 
                       p, LAItrue, 
                       var, cgf.view,
                       i0.sun, t0.sun, iD, DIFN, CI, 
                       resample, sensor, ID,
                       output.as.df, output.intermediate) {   
  
   attr(wL, "Wav") <- wL.wav
   attr(Rg, "Wav") <- Rg.wav
  
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
  
  return(out.resampled) 
}
