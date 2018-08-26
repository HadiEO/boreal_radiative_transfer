# Hypothesis: DASF is most sensitive to i0?
# Based on Knyazikhin et al. (2013)


# 2017.11.28
# WAIT UP! I SEE, SO THE LINEAR RELATIONSHIP BETWEEN BRF/LEAF ALBEDO VS. BRF IN THE 
# NIR DOMAIN ARE CONSTRUCTED FOR EACH ONE FIELD PLOT, THE DATA POINTS BEING THE SEVERAL
# WAVEBANDS IN THE NIR DOMAIN RECORDED BY THE SENSOR. MEANING, FOR SENTINEL-2 SENSOR, 
# THERE CAN ONLY BE THREE DATA POINTS TO FIT (I.E. TO OBTAIN DASF=INTERCEPT/(1-SLOPE)), 
# BECAUSE THERE ARE THREE BANDS APPROXIMATELY LYING IN THE 710-790 NM RANGE 
# (RE1, RE2, AND RE3, COVERING 690-803 nm) 
# SO NEED TO CORRECT THE ANALYSIS BELOW


# [1] Simulate reference leaf albedo --------------------------------------

library(Rprospect)
prospect.ref <- prospect4(N=1.5, Cab=16, Cw=0.005, Cm=0.002)
str(prospect.ref)
prospect.ref <- as_tibble(prospect.ref)

# Resample to Sentinel-2A bands
wL.ref <- prospect.ref %>% mutate(Albedo=Reflectance+Transmittance) %>% 
  dplyr::filter(Wavelength >= 710, Wavelength <= 790) %>% 
  dplyr::select(Albedo) %>% 
  colMeans()
# 0.9287335



# [2] Get measured canopy BRF + read various dataset ---------------------------------------------
# Real Sentinel-2 BRF in RE1, RE2, and RE3 (690-803 nm)

# Read satellite BRF
S2.BOA.w12.5 <- read_csv2("data/image_extracted_spectra/S2_BOA_w12_5.csv")
# Make ID character
L8.BOA.w12.5 <- L8.BOA.w12.5 %>% mutate(ID=as.character(ID))
# Rename bands to "colour"
names(S2.BOA.w12.5) <- c("X1", "ID",
                         "Sat.Blue", "Sat.Green", "Sat.Red", "Sat.RE1", "Sat.RE2", "Sat.RE3",
                         "Sat.NIR", "Sat.SWIR1", "Sat.SWIR2")
# Read gaps data
gaps <- read_csv2("results/gaps.csv") 
# Read simulated BRF
paras.BRF.S2A <- read_csv2("results/paras_BRF_fixedInputs_S2A_extended_Nilson.csv")   # LAItrue corrected for crown clumping
# Merge gaps and satellite BRF
gaps.sat.S2A <- left_join(gaps, S2.BOA.w12.5, by=c("ID"="ID"))
# Merge gaps, satellite, and PARAS BRF
gaps.sat.paras.S2A <- left_join(gaps.sat.S2A, paras.BRF.S2A, by = c("ID"="ID"))


# Keep only dense tree cover plots where understory contribution is negligible
gaps.sat.paras.S2A <- gaps.sat.paras.S2A %>% dplyr::filter(LAItrue>=3)


# Average satellite BRF in RE1, RE2, and RE3
gaps.sat.paras.S2A <- gaps.sat.paras.S2A %>% 
  mutate(Sat.DASF.wav=(Sat.RE1+Sat.RE2+Sat.RE3)/3)


# Test also including NIRn?


# [3] Get DASF ------------------------------------------------------------
gaps.sat.paras.S2A <- gaps.sat.paras.S2A %>% 
  mutate(BRF.to.wLref=Sat.DASF.wav/wL.ref)

plot(gaps.sat.paras.S2A$Sat.DASF.wav, gaps.sat.paras.S2A$BRF.to.wLref, cex=1.5,
     xlim=c(0,0.4), ylim=c(0,0.4),
     xlab=expression(bold("BRF (690-803 nm)")), ylab=expression(bold(paste("BRF/", omega[paste("L,710-790 nm")]))),
     main=expression(bold(paste(LAI[true], " > 3"))))
abline(0,1)
# Fit a linear model between BRF/wL to wL
fit <- lm(BRF.to.wLref~Sat.DASF.wav, data=gaps.sat.paras.S2A)
b <- coef(fit)[1]    # Intercept of fit = 0.00000000000000002617
k <- coef(fit)[2]    # Slope of fit = 1.07673517667962048705  
abline(fit, lty=2)
  
# Calculate DASF
gaps.sat.paras.S2A <- gaps.sat.paras.S2A %>% mutate(DASF=)











