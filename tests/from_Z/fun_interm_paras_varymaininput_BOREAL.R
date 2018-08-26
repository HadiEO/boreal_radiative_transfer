paras.interm.varymaininput.wL <- function(LAIe, CI, DIFN, i0, cgf.view,   # Gap fractions data from DHP
                                          wL.b1, wL.b2, wL.b3, wL.b4, wL.b5, wL.b7,            # Now, wL
                                          Rg.b1, Rg.b2, Rg.b3, Rg.b4, Rg.b5, Rg.b7) { # Ground reflectance, scalar for each ETM+ band    
  
  wL <- list(b1 = wL.b1, b2 = wL.b2, b3 = wL.b3, b4 = wL.b4, b5 = wL.b5, b7 = wL.b7) # now wL
  
  LAIt <- LAIe / CI                                                 # True LAI
  
  p = 1 - ((1 - DIFN) / LAIt)                                       # analytical formula based on Stenberg (2007)
  
  wC <- plyr::laply(wL, function(x) (x - p*x) / (1 - p*x))          # Canopy scattering coefficient
  
  q <- 1 - exp(-0.1684 * LAIt)                                      # based on Mottus & Stenberg (2008)
  Q <- plyr::laply(wL, function(x) 0.5 + (0.5*q) * ((1 - (p*x)) / (1 - (p*q*x))))
  
  BRF <- base::mapply(                                             # Ouput MUST be vector for sobol()
    FUN = function(cgf.view, Rg, i0, Q, wC) (cgf.view * Rg * (1 - i0)) + (Q * i0 * wC),      # Based on Rautiainen & Stenberg (2005)    
    cgf.view = cgf.view, Rg = c(Rg.b1, Rg.b2, Rg.b3, Rg.b4, Rg.b5, Rg.b7), i0 = i0, Q = Q, wC = wC)  
  
  out <- c(BRF, LAIt, p, q, wC, Q)   # <======================= return as appended row-vector so .combine = "rbind"
  return(out)
}