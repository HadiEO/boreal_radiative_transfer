# Read leaf data -------------------------------------------
# The spectra are stored by species, refl/trans, and sample type (sunlit or shaded, adaxial or abaxial)
# PINE (mature shoots) : Ea, Eb, Sa, Sb <- sun-exposed adaxial, sun-exposed abaxial, shaded adaxial, shaded abaxial
# SPRUCE : E0, E1, S <- current year juvenile, mature, shaded shoots
# BIRCH (fully-grown leaves) : Ea, Eb, Sa, Sb <- sun-exposed adaxial, sun-exposed abaxial, shaded adaxial, shaded abaxial

# TODO: need to assign column type as numeric when reading the csv 

R.pine.Ea <- read_csv2("data/leaf/pine/R/pine_Refl_Ea.csv", col_types = strrep("d", 15))    # Pine Refl
R.pine.Eb <- read_csv2("data/leaf/pine/R/pine_Refl_Eb.csv", col_types = strrep("d", 15)) 
R.pine.Sa <- read_csv2("data/leaf/pine/R/pine_Refl_Sa.csv", col_types = strrep("d", 15)) 
R.pine.Sb <- read_csv2("data/leaf/pine/R/pine_Refl_Sb.csv", col_types = strrep("d", 15)) 

T.pine.Ea <- read_csv2("data/leaf/pine/T/pine_Trans_Ea.csv", col_types = strrep("d", 15))    # Pine Trans
T.pine.Eb <- read_csv2("data/leaf/pine/T/pine_Trans_Eb.csv", col_types = strrep("d", 15)) 
T.pine.Sa <- read_csv2("data/leaf/pine/T/pine_Trans_Sa.csv", col_types = strrep("d", 15)) 
T.pine.Sb <- read_csv2("data/leaf/pine/T/pine_Trans_Sb.csv", col_types = strrep("d", 15)) 

R.spruce.E0 <- read_csv2("data/leaf/spruce/R/spruce_Refl_E0.csv", col_types = strrep("d", 15))   # Spruce Refl
R.spruce.E1 <- read_csv2("data/leaf/spruce/R/spruce_Refl_E1.csv", col_types = strrep("d", 15))   
R.spruce.S <- read_csv2("data/leaf/spruce/R/spruce_Refl_S.csv", col_types = strrep("d", 15))   

T.spruce.E0 <- read_csv2("data/leaf/spruce/T/spruce_Trans_E0.csv", col_types = strrep("d", 15))   # Spruce Trans
T.spruce.E1 <- read_csv2("data/leaf/spruce/T/spruce_Trans_E1.csv", col_types = strrep("d", 15))  
T.spruce.S <- read_csv2("data/leaf/spruce/T/spruce_Trans_S.csv", col_types = strrep("d", 15))   

R.birch.Ea <- read_csv2("data/leaf/birch/R/birch_Refl_Ea.csv", col_types = strrep("d", 15))    # Birch Refl
R.birch.Eb <- read_csv2("data/leaf/birch/R/birch_Refl_Eb.csv", col_types = strrep("d", 15)) 
R.birch.Sa <- read_csv2("data/leaf/birch/R/birch_Refl_Sa.csv", col_types = strrep("d", 15)) 
R.birch.Sb <- read_csv2("data/leaf/birch/R/birch_Refl_Sb.csv", col_types = strrep("d", 15)) 

T.birch.Ea <- read_csv2("data/leaf/birch/T/birch_Trans_Ea.csv", col_types = strrep("d", 15))    # Birch Trans
T.birch.Eb <- read_csv2("data/leaf/birch/T/birch_Trans_Eb.csv", col_types = strrep("d", 15)) 
T.birch.Sa <- read_csv2("data/leaf/birch/T/birch_Trans_Sa.csv", col_types = strrep("d", 15)) 
T.birch.Sb <- read_csv2("data/leaf/birch/T/birch_Trans_Sb.csv", col_types = strrep("d", 15)) 


# Common wavelength -------------------------------------------------------

all(sapply(list(R.pine.Eb$Wl, R.pine.Sa$Wl, R.pine.Sb$Wl,
                T.pine.Ea$Wl, T.pine.Eb$Wl, T.pine.Sa$Wl, T.pine.Sb$Wl,
                R.spruce.E0$Wl, R.spruce.E1$Wl, R.spruce.S$Wl,
                T.spruce.E0$Wl, T.spruce.E1$Wl, T.spruce.S$Wl,
                R.pine.Ea$Wl, R.pine.Eb$Wl, R.pine.Sa$Wl, R.pine.Sb$Wl,
                T.pine.Ea$Wl, T.pine.Eb$Wl, T.pine.Sa$Wl, T.pine.Sb$Wl), FUN = identical, R.pine.Ea$Wl))
# TRUE
leafData.Wav <- R.pine.Ea$Wl
write.csv2(leafData.Wav, "data/leaf/leaf_asd_wav.csv")


# [1] Calculate leaf albedo per sample = per leaf  ----------------------------
# Caution, there is NaN in several individual measurements
# Do each sample to see influence of stand density on difference between E and S
# Pine E
wL.pine.Ea <- R.pine.Ea + T.pine.Ea
wL.pine.Eb <- R.pine.Eb + T.pine.Eb
wL.pine.E <- 0.5 * wL.pine.Ea + 0.5 * wL.pine.Eb
wL.pine.E <- as_tibble(wL.pine.E) %>% mutate(Wl = leafData.Wav)
# Pine S
wL.pine.Sa <- R.pine.Sa + T.pine.Sa
wL.pine.Sb <- R.pine.Sb + T.pine.Sb
wL.pine.S <- 0.5 * wL.pine.Sa + 0.5 * wL.pine.Sb
wL.pine.S <- as_tibble(wL.pine.S)  %>% mutate(Wl = leafData.Wav)
# Pine E and S
wL.pine <- 0.5 * wL.pine.E + 0.5 * wL.pine.S
wL.pine <- as_tibble(wL.pine)  %>% mutate(Wl = leafData.Wav)


# Spruce E0
wL.spruce.E0 <- R.spruce.E0 + T.spruce.E0
wL.spruce.E0 <- as_tibble(wL.spruce.E0) %>% mutate(Wl = leafData.Wav)
# Spruce E1
wL.spruce.E1 <- R.spruce.E1 + T.spruce.E1
wL.spruce.E1 <- as_tibble(wL.spruce.E1) %>% mutate(Wl = leafData.Wav)
# Spruce S
wL.spruce.S <- R.spruce.S + T.spruce.S
wL.spruce.S <- as_tibble(wL.spruce.S) %>% mutate(Wl = leafData.Wav)
# Spruce E = E0 and E1
wL.spruce.E <- 0.5 * wL.spruce.E0 + 0.5 * wL.spruce.E1
wL.spruce.E <- as_tibble(wL.spruce.E)  %>% mutate(Wl = leafData.Wav)
# Spruce E and S
wL.spruce <- 0.5 * wL.spruce.E + 0.5 * wL.spruce.S
wL.spruce <- as_tibble(wL.spruce)  %>% mutate(Wl = leafData.Wav)


# Birch E
wL.birch.Ea <- R.birch.Ea + T.birch.Ea
wL.birch.Eb <- R.birch.Eb + T.birch.Eb
wL.birch.E <- 0.5 * wL.birch.Ea + 0.5 * wL.birch.Eb
wL.birch.E <- as_tibble(wL.birch.E) %>% mutate(Wl = leafData.Wav)
# Birch S
wL.birch.Sa <- R.birch.Sa + T.birch.Sa
wL.birch.Sb <- R.birch.Sb + T.birch.Sb
wL.birch.S <- 0.5 * wL.birch.Sa + 0.5 * wL.birch.Sb
wL.birch.S <- as_tibble(wL.birch.S) %>% mutate(Wl = leafData.Wav)
# Birch E and S
wL.birch <- 0.5 * wL.birch.E + 0.5 * wL.birch.S
wL.birch <- as_tibble(wL.birch) %>% mutate(Wl = leafData.Wav)


# Get leaf albedo variation due to stand densities -----------------------
# Not sure if I have assigned stand densities to the correct trees, but the three stands can be known
# From the geographical locations. We're interested in the variance.
stands <- read_csv2("data/leaf/stands.csv")
# For each stand, average over trees, NOT separately for sun-exposed and shaded leaves
# there's NaN
# R.pine.Ea ==> P2
# R.pine.Sa ==> P10
# R.pine.Sb ==> P2
# T.pine.Sb ==> P2, P6, P7
# So pine E is ok, just exclude P2
# wL.pine.E <- as_tibble(wL.pine.E) %>% select(-P2) %>% mutate(Wl = leafData.Wav)   # Run if want to plot!

# R.spruce.E1 ==> S8, S10
# R.spruce.S ==> S6
# So spruce E0 is fine

# Birch are all ok

# Pine
wL.pine.lowDens <- wL.pine %>% select(P4, P5, P6)
wL.pine.lowDens.meanstd <- tibble(Wl = leafData.Wav, MEAN = apply(wL.pine.lowDens, 1, mean, na.rm = TRUE), STD = apply(wL.pine.lowDens, 1, sd, na.rm = TRUE),
                                  MIN = apply(wL.pine.lowDens, 1, min, na.rm = TRUE), MAX = apply(wL.pine.lowDens, 1, max, na.rm = TRUE))
wL.pine.lowDens.meanstd <- wL.pine.lowDens.meanstd %>% mutate(SE = STD / sqrt(ncol(wL.pine.lowDens)))

wL.pine.modDens <- wL.pine %>% select(P1, P3)
wL.pine.modDens.meanstd <- tibble(Wl = leafData.Wav, MEAN = apply(wL.pine.modDens, 1, mean, na.rm = TRUE), STD = apply(wL.pine.modDens, 1, sd, na.rm = TRUE),
                                  MIN = apply(wL.pine.modDens, 1, min, na.rm = TRUE), MAX = apply(wL.pine.modDens, 1, max, na.rm = TRUE))
wL.pine.modDens.meanstd <- wL.pine.modDens.meanstd %>% mutate(SE = STD / sqrt(ncol(wL.pine.modDens)))

wL.pine.highDens <- wL.pine %>% select(P7, P8, P9, P10)
wL.pine.highDens.meanstd <- tibble(Wl = leafData.Wav, MEAN = apply(wL.pine.highDens, 1, mean, na.rm = TRUE), STD = apply(wL.pine.highDens, 1, sd, na.rm = TRUE),
                                   MIN = apply(wL.pine.highDens, 1, min, na.rm = TRUE), MAX = apply(wL.pine.highDens, 1, max, na.rm = TRUE))
wL.pine.highDens.meanstd <- wL.pine.highDens.meanstd %>% mutate(SE = STD / sqrt(ncol(wL.pine.highDens)))


# Spruce
wL.spruce.lowDens <- wL.spruce %>% select(S5, S6, S10)
wL.spruce.lowDens.meanstd <- tibble(Wl = leafData.Wav, MEAN = apply(wL.spruce.lowDens, 1, mean, na.rm = TRUE), STD = apply(wL.spruce.lowDens, 1, sd, na.rm = TRUE),
                                    MIN = apply(wL.spruce.lowDens, 1, min, na.rm = TRUE), MAX = apply(wL.spruce.lowDens, 1, max, na.rm = TRUE))
wL.spruce.lowDens.meanstd <- wL.spruce.lowDens.meanstd %>% mutate(SE = STD / sqrt(ncol(wL.spruce.lowDens)))

wL.spruce.modDens <- wL.spruce %>% select(S1, S2, S3, S4)
wL.spruce.modDens.meanstd <- tibble(Wl = leafData.Wav, MEAN = apply(wL.spruce.modDens, 1, mean, na.rm = TRUE), STD = apply(wL.spruce.modDens, 1, sd, na.rm = TRUE),
                                    MIN = apply(wL.spruce.modDens, 1, min, na.rm = TRUE), MAX = apply(wL.spruce.modDens, 1, max, na.rm = TRUE))
wL.spruce.modDens.meanstd <- wL.spruce.modDens.meanstd %>% mutate(SE = STD / sqrt(ncol(wL.spruce.modDens)))

wL.spruce.highDens <- wL.spruce %>% select(S7, S8, S9)
wL.spruce.highDens.meanstd <- tibble(Wl = leafData.Wav, MEAN = apply(wL.spruce.highDens, 1, mean, na.rm = TRUE), STD = apply(wL.spruce.highDens, 1, sd, na.rm = TRUE),
                                     MIN = apply(wL.spruce.highDens, 1, min, na.rm = TRUE), MAX = apply(wL.spruce.highDens, 1, max, na.rm = TRUE))
wL.spruce.highDens.meanstd <- wL.spruce.highDens.meanstd %>% mutate(SE = STD / sqrt(ncol(wL.spruce.highDens)))


#  Birch
wL.birch.lowDens <- wL.birch %>% select(B8, B9, B10)
wL.birch.lowDens.meanstd <- tibble(Wl = leafData.Wav, MEAN = apply(wL.birch.lowDens, 1, mean, na.rm = TRUE), STD = apply(wL.birch.lowDens, 1, sd, na.rm = TRUE),
                                   MIN = apply(wL.birch.lowDens, 1, min, na.rm = TRUE), MAX = apply(wL.birch.lowDens, 1, max, na.rm = TRUE))
wL.birch.lowDens.meanstd <- wL.birch.lowDens.meanstd %>% mutate(SE = STD / sqrt(ncol(wL.birch.lowDens)))

wL.birch.modDens <- wL.birch %>% select(B4, B5, B6, B7)
wL.birch.modDens.meanstd <- tibble(Wl = leafData.Wav, MEAN = apply(wL.birch.modDens, 1, mean, na.rm = TRUE), STD = apply(wL.birch.modDens, 1, sd, na.rm = TRUE),
                                   MIN = apply(wL.birch.modDens, 1, min, na.rm = TRUE), MAX = apply(wL.birch.modDens, 1, max, na.rm = TRUE))
wL.birch.modDens.meanstd <- wL.birch.modDens.meanstd %>% mutate(SE = STD / sqrt(ncol(wL.birch.modDens)))

wL.birch.highDens <- wL.birch %>% select(B1, B2, B3)
wL.birch.highDens.meanstd <- tibble(Wl = leafData.Wav, MEAN = apply(wL.birch.highDens, 1, mean, na.rm = TRUE), STD = apply(wL.birch.highDens, 1, sd, na.rm = TRUE),
                                    MIN = apply(wL.birch.highDens, 1, min, na.rm = TRUE), MAX = apply(wL.birch.highDens, 1, max, na.rm = TRUE))
wL.birch.highDens.meanstd <- wL.birch.highDens.meanstd %>% mutate(SE = STD / sqrt(ncol(wL.birch.highDens)))


# So means by stand density group
# Pine
wL.pine.byDens <- tibble(Wav = leafData.Wav, 
                         low = wL.pine.lowDens.meanstd$MEAN,
                         moderate = wL.pine.modDens.meanstd$MEAN,
                         high = wL.pine.highDens.meanstd$MEAN)

# Spruce
wL.spruce.byDens <- tibble(Wav = leafData.Wav, 
                         low = wL.spruce.lowDens.meanstd$MEAN,
                         moderate = wL.spruce.modDens.meanstd$MEAN,
                         high = wL.spruce.highDens.meanstd$MEAN)

# Birch
wL.birch.byDens <- tibble(Wav = leafData.Wav, 
                         low = wL.birch.lowDens.meanstd$MEAN,
                         moderate = wL.birch.modDens.meanstd$MEAN,
                         high = wL.birch.highDens.meanstd$MEAN)


# Save to disk
write.csv2(wL.pine.byDens, "data/leaf/wL_pine_byDens.csv")
write.csv2(wL.spruce.byDens, "data/leaf/wL_spruce_byDens.csv")
write.csv2(wL.birch.byDens, "data/leaf/wL_birch_byDens.csv")

# Resample to S2 and save to disk, for GSA
S2.bands.names <-
  my_spectralResampling(spectra = wL.pine.byDens$low, wavelength = leafData.Wav, sensor = "Sentinel2A") %>% 
  attr(., "names")

prep.wLbyDens.gsa <- function(x) {
  temp <- x %>% dplyr::select(-Wav) %>% 
    map_df(my_spectralResampling, wavelength = leafData.Wav, sensor = "Sentinel2A") %>% t()
  colnames(temp) <- S2.bands.names
  out <- as_tibble(temp)  # This has no information of stand density cause we are not sure which cluster of plots belong to which stand density                    
  return(out)
} 

prep.wLbyDens.gsa(wL.pine.byDens) %>% write.csv2("data/leaf/wL_pine_byDens_S2A.csv")
prep.wLbyDens.gsa(wL.spruce.byDens) %>% write.csv2("data/leaf/wL_spruce_byDens_S2A.csv")
prep.wLbyDens.gsa(wL.birch.byDens) %>% write.csv2("data/leaf/wL_birch_byDens_S2A.csv")



# Plot spectra ------------------------------------------------------------
# Easier if convert them to spectral library object (hsdar::speclib)

wL.pine.byDens <- read_csv2("data/leaf/wL_pine_byDens.csv")
wL.spruce.byDens <- read_csv2("data/leaf/wL_spruce_byDens.csv")
wL.birch.byDens <- read_csv2("data/leaf/wL_birch_byDens.csv")

wL.mean.bySpecies <- read_csv2("data/leaf/wL_bySpecies.csv")

leafData.Wav <- read_csv2("data/leaf/leaf_asd_wav.csv")

temp1 <- wL.pine.byDens %>% 
        dplyr::select(low, moderate, high) %>% t() %>% 
        hsdar::speclib(., leafData.Wav$x) 

temp2 <- wL.spruce.byDens %>% 
  dplyr::select(low, moderate, high) %>% t() %>% 
  hsdar::speclib(., leafData.Wav$x) 

temp3 <- wL.birch.byDens %>% 
  dplyr::select(low, moderate, high) %>% t() %>% 
  hsdar::speclib(., leafData.Wav$x) 

temp4 <- wL.mean.bySpecies %>% 
  dplyr::select(Pine, Spruce, Birch) %>% t() %>% 
  hsdar::speclib(., leafData.Wav$x) 



  
x11()
par(mfrow = c(2,2))
plot(temp1, FUN = "mean", lty = 2, ylim = c(0,1), main = "Pine by stand density")
plot(temp1, new = FALSE, FUN = "min", lty = 2)
plot(temp1, new = FALSE, FUN = "max", lty = 2)

plot(temp2, FUN = "mean", lty = 2, ylim = c(0,1), main = "Spruce by stand density")
plot(temp2, new = FALSE, FUN = "min", lty = 2)
plot(temp2, new = FALSE, FUN = "max", lty = 2)

plot(temp3, FUN = "mean", lty = 2, ylim = c(0,1), main = "Birch by stand density")
plot(temp3, new = FALSE, FUN = "min", lty = 2)
plot(temp3, new = FALSE, FUN = "max", lty = 2)

plot(temp4, FUN = "mean", lty = 2, ylim = c(0,1), main = "Interspecific means")
plot(temp4, new = FALSE, FUN = "min", lty = 2)
plot(temp4, new = FALSE, FUN = "max", lty = 2)




# Plot speclib methods
# mask(spectral_data) <- c(1040, 1060, 1350, 1450)
# plot(spectral_data, legend = list(x = "topleft"))
# plot(spectral_data, bygroups = TRUE, legend = list(x = "topleft"))
# par(mfrow = c(2,3))
# plot(spectral_data, FUN = "min", main = "Minimum of speclib")
# plot(spectral_data, FUN = "max", main = "Maximum of speclib")
# plot(spectral_data, FUN = "median", main = "Median of speclib")
# plot(spectral_data, FUN = "mean", main = "Mean of speclib")
# plot(spectral_data, FUN = "var", main = "Variance of speclib")





