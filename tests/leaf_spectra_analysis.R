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

# Spruce E0
wL.spruce.E0 <- R.spruce.E0 + T.spruce.E0
wL.spruce.E0 <- as_tibble(wL.spruce.E0) %>% mutate(Wl = leafData.Wav)
# Spruce E1
wL.spruce.E1 <- R.spruce.E1 + T.spruce.E1
wL.spruce.E1 <- as_tibble(wL.spruce.E1) %>% mutate(Wl = leafData.Wav)
# Spruce S
wL.spruce.S <- R.spruce.S + T.spruce.S
wL.spruce.S <- as_tibble(wL.spruce.S) %>% mutate(Wl = leafData.Wav)

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



# See if leaf albedo differ between stand densities -----------------------
stands <- read_csv2("data/leaf/stands.csv")
# For each stand, average over trees, separately for sun-exposed and shaded leaves
# there's NaN
# R.pine.Ea ==> P2
# R.pine.Sa ==> P10
# R.pine.Sb ==> P2
# T.pine.Sb ==> P2, P6, P7
# So pine E is ok, just exclude P2
wL.pine.E <- as_tibble(wL.pine.E) %>% select(-P2) %>% mutate(Wl = leafData.Wav)   # Run if want to plot!

# R.spruce.E1 ==> S8, S10
# R.spruce.S ==> S6
# So spruce E0 is fine

# Birch are all ok

# Pine E
wL.pine.E.lowDens <- wL.pine.E %>% select(P4, P5, P6)
wL.pine.E.lowDens.meanstd <- tibble(Wl = leafData.Wav, MEAN = apply(wL.pine.E.lowDens, 1, mean), STD = apply(wL.pine.E.lowDens, 1, sd))
wL.pine.E.lowDens.meanstd <- wL.pine.E.lowDens.meanstd %>% mutate(SE = STD / sqrt(ncol(wL.pine.E.lowDens)))

wL.pine.E.modDens <- wL.pine.E %>% select(P1, P3)
wL.pine.E.modDens.meanstd <- tibble(Wl = leafData.Wav, MEAN = apply(wL.pine.E.modDens, 1, mean), STD = apply(wL.pine.E.modDens, 1, sd))
wL.pine.E.modDens.meanstd <- wL.pine.E.modDens.meanstd %>% mutate(SE = STD / sqrt(ncol(wL.pine.E.modDens)))

wL.pine.E.highDens <- wL.pine.E %>% select(P7, P8, P9, P10)
wL.pine.E.highDens.meanstd <- tibble(Wl = leafData.Wav, MEAN = apply(wL.pine.E.highDens, 1, mean), STD = apply(wL.pine.E.highDens, 1, sd))
wL.pine.E.highDens.meanstd <- wL.pine.E.highDens.meanstd %>% mutate(SE = STD / sqrt(ncol(wL.pine.E.highDens)))


# Spruce
wL.spruce.E0.lowDens <- wL.spruce.E0 %>% select(S5, S6, S10)
wL.spruce.E0.lowDens.meanstd <- tibble(Wl = leafData.Wav, MEAN = apply(wL.spruce.E0.lowDens, 1, mean), STD = apply(wL.spruce.E0.lowDens, 1, sd))
wL.spruce.E0.lowDens.meanstd <- wL.spruce.E0.lowDens.meanstd %>% mutate(SE = STD / sqrt(ncol(wL.spruce.E0.lowDens)))

wL.spruce.E0.modDens <- wL.spruce.E0 %>% select(S1, S2, S3, S4)
wL.spruce.E0.modDens.meanstd <- tibble(Wl = leafData.Wav, MEAN = apply(wL.spruce.E0.modDens, 1, mean), STD = apply(wL.spruce.E0.modDens, 1, sd))
wL.spruce.E0.modDens.meanstd <- wL.spruce.E0.modDens.meanstd %>% mutate(SE = STD / sqrt(ncol(wL.spruce.E0.modDens)))

wL.spruce.E0.highDens <- wL.spruce.E0 %>% select(S7, S8, S9)
wL.spruce.E0.highDens.meanstd <- tibble(Wl = leafData.Wav, MEAN = apply(wL.spruce.E0.highDens, 1, mean), STD = apply(wL.spruce.E0.highDens, 1, sd))
wL.spruce.E0.highDens.meanstd <- wL.spruce.E0.highDens.meanstd %>% mutate(SE = STD / sqrt(ncol(wL.spruce.E0.highDens)))


# Birch E & S
wL.birch.E.lowDens <- wL.birch.E %>% select(B8, B9, B10)
wL.birch.E.lowDens.meanstd <- tibble(Wl = leafData.Wav, MEAN = apply(wL.birch.E.lowDens, 1, mean), STD = apply(wL.birch.E.lowDens, 1, sd))
wL.birch.E.lowDens.meanstd <- wL.birch.E.lowDens.meanstd %>% mutate(SE = STD / sqrt(ncol(wL.birch.E.lowDens)))

wL.birch.E.modDens <- wL.birch.E %>% select(B4, B5, B6, B7)
wL.birch.E.modDens.meanstd <- tibble(Wl = leafData.Wav, MEAN = apply(wL.birch.E.modDens, 1, mean), STD = apply(wL.birch.E.modDens, 1, sd))
wL.birch.E.modDens.meanstd <- wL.birch.E.modDens.meanstd %>% mutate(SE = STD / sqrt(ncol(wL.birch.E.modDens)))

wL.birch.E.highDens <- wL.birch.E %>% select(B1, B2, B3)
wL.birch.E.highDens.meanstd <- tibble(Wl = leafData.Wav, MEAN = apply(wL.birch.E.highDens, 1, mean), STD = apply(wL.birch.E.highDens, 1, sd))
wL.birch.E.highDens.meanstd <- wL.birch.E.highDens.meanstd %>% mutate(SE = STD / sqrt(ncol(wL.birch.E.highDens)))


wL.birch.S.lowDens <- wL.birch.S %>% select(B8, B9, B10)
wL.birch.S.lowDens.meanstd <- tibble(Wl = leafData.Wav, MEAN = apply(wL.birch.S.lowDens, 1, mean), STD = apply(wL.birch.S.lowDens, 1, sd))
wL.birch.S.lowDens.meanstd <- wL.birch.S.lowDens.meanstd %>% mutate(SE = STD / sqrt(ncol(wL.birch.S.lowDens)))

wL.birch.S.modDens <- wL.birch.S %>% select(B4, B5, B6, B7)
wL.birch.S.modDens.meanstd <- tibble(Wl = leafData.Wav, MEAN = apply(wL.birch.S.modDens, 1, mean), STD = apply(wL.birch.S.modDens, 1, sd))
wL.birch.S.modDens.meanstd <- wL.birch.S.modDens.meanstd %>% mutate(SE = STD / sqrt(ncol(wL.birch.S.modDens)))

wL.birch.S.highDens <- wL.birch.S %>% select(B1, B2, B3)
wL.birch.S.highDens.meanstd <- tibble(Wl = leafData.Wav, MEAN = apply(wL.birch.S.highDens, 1, mean), STD = apply(wL.birch.S.highDens, 1, sd))
wL.birch.S.highDens.meanstd <- wL.birch.S.highDens.meanstd %>% mutate(SE = STD / sqrt(ncol(wL.birch.S.highDens)))



# Plot leaf albedo by stand densities, separate E/S -----------------------

x11()
par(mfrow = c(2,2), ps = 14)
# Pine
# Low density
plot(leafData.Wav, wL.pine.E.lowDens.meanstd$MEAN, type = "l", col = "red", ylim = c(0,1), xlab = "Wavelength (nm)", ylab = "Leaf albedo", main = "Pine E")
lines(leafData.Wav, wL.pine.E.lowDens.meanstd$MEAN + wL.pine.E.lowDens.meanstd$SE, col = "red", lty = 2)
lines(leafData.Wav, wL.pine.E.lowDens.meanstd$MEAN - wL.pine.E.lowDens.meanstd$SE, col = "red", lty = 2)
# Moderate density
lines(leafData.Wav, wL.pine.E.modDens.meanstd$MEAN, col = "cyan", lty = 2)
lines(leafData.Wav, wL.pine.E.modDens.meanstd$MEAN + wL.pine.E.modDens.meanstd$SE, col = "cyan", lty = 2)
lines(leafData.Wav, wL.pine.E.modDens.meanstd$MEAN - wL.pine.E.modDens.meanstd$SE, col = "cyan", lty = 2)
# High density
lines(leafData.Wav, wL.pine.E.highDens.meanstd$MEAN, col = "blue", lty = 2)
lines(leafData.Wav, wL.pine.E.highDens.meanstd$MEAN + wL.pine.E.highDens.meanstd$SE, col = "blue", lty = 2)
lines(leafData.Wav, wL.pine.E.highDens.meanstd$MEAN - wL.pine.E.highDens.meanstd$SE, col = "blue", lty = 2)

legend("topright", c("Low density (n=3)", "Moderate density (n=2)", "High density (n=4)"), lty = rep(1,3), col = c("red", "cyan", "blue"))

# Spruce
# Low density
plot(leafData.Wav, wL.spruce.E0.lowDens.meanstd$MEAN, type = "l", col = "red", ylim = c(0,1), xlab = "Wavelength (nm)", ylab = "Leaf albedo", main = "Spruce E0")
lines(leafData.Wav, wL.spruce.E0.lowDens.meanstd$MEAN + wL.spruce.E0.lowDens.meanstd$SE, col = "red", lty = 2)
lines(leafData.Wav, wL.spruce.E0.lowDens.meanstd$MEAN - wL.spruce.E0.lowDens.meanstd$SE, col = "red", lty = 2)
# Moderate density
lines(leafData.Wav, wL.spruce.E0.modDens.meanstd$MEAN, col = "cyan", lty = 2)
lines(leafData.Wav, wL.spruce.E0.modDens.meanstd$MEAN + wL.spruce.E0.modDens.meanstd$SE, col = "cyan", lty = 2)
lines(leafData.Wav, wL.spruce.E0.modDens.meanstd$MEAN - wL.spruce.E0.modDens.meanstd$SE, col = "cyan", lty = 2)
# High density
lines(leafData.Wav, wL.spruce.E0.highDens.meanstd$MEAN, col = "blue", lty = 2)
lines(leafData.Wav, wL.spruce.E0.highDens.meanstd$MEAN + wL.spruce.E0.highDens.meanstd$SE, col = "blue", lty = 2)
lines(leafData.Wav, wL.spruce.E0.highDens.meanstd$MEAN - wL.spruce.E0.highDens.meanstd$SE, col = "blue", lty = 2)

legend("topright", c("Low density (n=3)", "Moderate density (n=4)", "High density (n=3)"), lty = rep(1,3), col = c("red", "cyan", "blue"))


# Birch E
# Low density
plot(leafData.Wav, wL.birch.E.lowDens.meanstd$MEAN, type = "l", col = "red", ylim = c(0,1), xlab = "Wavelength (nm)", ylab = "Leaf albedo", main = "Birch E")
lines(leafData.Wav, wL.birch.E.lowDens.meanstd$MEAN + wL.birch.E.lowDens.meanstd$SE, col = "red", lty = 2)
lines(leafData.Wav, wL.birch.E.lowDens.meanstd$MEAN - wL.birch.E.lowDens.meanstd$SE, col = "red", lty = 2)
# Moderate density
lines(leafData.Wav, wL.birch.E.modDens.meanstd$MEAN, col = "cyan", lty = 2)
lines(leafData.Wav, wL.birch.E.modDens.meanstd$MEAN + wL.birch.E.modDens.meanstd$SE, col = "cyan", lty = 2)
lines(leafData.Wav, wL.birch.E.modDens.meanstd$MEAN - wL.birch.E.modDens.meanstd$SE, col = "cyan", lty = 2)
# High density
lines(leafData.Wav, wL.birch.E.highDens.meanstd$MEAN, col = "blue", lty = 2)
lines(leafData.Wav, wL.birch.E.highDens.meanstd$MEAN + wL.birch.E.highDens.meanstd$SE, col = "blue", lty = 2)
lines(leafData.Wav, wL.birch.E.highDens.meanstd$MEAN - wL.birch.E.highDens.meanstd$SE, col = "blue", lty = 2)

legend("topright", c("Low density (n=3)", "Moderate density (n=4)", "High density (n=3)"), lty = rep(1,3), col = c("red", "cyan", "blue"))


# Birch S
# Low density
plot(leafData.Wav, wL.birch.S.lowDens.meanstd$MEAN, type = "l", col = "red", ylim = c(0,1), xlab = "Wavelength (nm)", ylab = "Leaf albedo", main = "Birch S")
lines(leafData.Wav, wL.birch.S.lowDens.meanstd$MEAN + wL.birch.S.lowDens.meanstd$SE, col = "red", lty = 2)
lines(leafData.Wav, wL.birch.S.lowDens.meanstd$MEAN - wL.birch.S.lowDens.meanstd$SE, col = "red", lty = 2)
# Moderate density
lines(leafData.Wav, wL.birch.S.modDens.meanstd$MEAN, col = "cyan", lty = 2)
lines(leafData.Wav, wL.birch.S.modDens.meanstd$MEAN + wL.birch.S.modDens.meanstd$SE, col = "cyan", lty = 2)
lines(leafData.Wav, wL.birch.S.modDens.meanstd$MEAN - wL.birch.S.modDens.meanstd$SE, col = "cyan", lty = 2)
# High density
lines(leafData.Wav, wL.birch.S.highDens.meanstd$MEAN, col = "blue", lty = 2)
lines(leafData.Wav, wL.birch.S.highDens.meanstd$MEAN + wL.birch.S.highDens.meanstd$SE, col = "blue", lty = 2)
lines(leafData.Wav, wL.birch.S.highDens.meanstd$MEAN - wL.birch.S.highDens.meanstd$SE, col = "blue", lty = 2)

legend("topright", c("Low density (n=3)", "Moderate density (n=4)", "High density (n=3)"), lty = rep(1,3), col = c("red", "cyan", "blue"))



# [2] For each sample (tree), average sunlit/shaded and adaxial/abaxial -------
# This is ok because all given equal weights 
# Columns are sample (tree) ID
# Solution : MAKE 3-D ARRAY!!! But check order of columns are the same: Yes checked OK

R.pine.allSamples <- abind(R.pine.Ea, R.pine.Eb, R.pine.Sa, R.pine.Sb, along = 3)       # Pine R
R.pine.meanByTree <- rowMeans(R.pine.allSamples, dims = 2, na.rm = TRUE)

T.pine.allSamples <- abind(T.pine.Ea, T.pine.Eb, T.pine.Sa, T.pine.Sb, along = 3)       # Pine T
T.pine.meanByTree <- rowMeans(T.pine.allSamples, dims = 2, na.rm = TRUE)

R.spruce.allSamples <- abind(R.spruce.E0, R.spruce.E1, R.spruce.S, along = 3)           # Spruce R
R.spruce.meanByTree <- rowMeans(R.spruce.allSamples, dims = 2, na.rm = TRUE)

T.spruce.allSamples <- abind(T.spruce.E0, T.spruce.E1, T.spruce.S, along = 3)           # Spruce T
T.spruce.meanByTree <- rowMeans(T.spruce.allSamples, dims = 2, na.rm = TRUE)

R.birch.allSamples <- abind(R.birch.Ea, R.birch.Eb, R.birch.Sa, R.birch.Sb, along = 3)  # Birch R
R.birch.meanByTree <- rowMeans(R.birch.allSamples, dims = 2, na.rm = TRUE)

T.birch.allSamples <- abind(T.birch.Ea, T.birch.Eb, T.birch.Sa, T.birch.Sb, along = 3)  # Birch T
T.birch.meanByTree <- rowMeans(T.birch.allSamples, dims = 2, na.rm = TRUE)


# Albedo = Reflectance + Transmittance
wL.pine.meanByTree <- R.pine.meanByTree + T.pine.meanByTree; wL.pine.meanByTree <- as_data_frame(wL.pine.meanByTree)  # Pine wL
wL.spruce.meanByTree <- R.spruce.meanByTree + T.spruce.meanByTree; wL.spruce.meanByTree <- as_data_frame(wL.spruce.meanByTree) # Spruce wL
wL.birch.meanByTree <- R.birch.meanByTree + T.birch.meanByTree; wL.birch.meanByTree <- as_data_frame(wL.birch.meanByTree) # Birch wL


# Resample leaf albedo by tree --------------------------------------------

# Need to change this to my_spectralResampling()
# source("munge/spc_resample.R")                                                    # Resample wL to intended sensor
# wav.hypers <- R.pine.Ea$Wl   # Wav of all spectra are the same
# 
# wL.pine.meanByTree.S2 <- wL.pine.meanByTree %>%  dplyr::select(P1:STDEV) %>%     # Pine S2 by tree
#   map(spc_resample, wav = wav.hypers, sensor = "Sentinel2")
# 
# wL.spruce.meanByTree.S2 <- wL.spruce.meanByTree %>%  dplyr::select(S1:STDEV) %>%    # Spruce S2 by tree
#     map(spc_resample, wav = wav.hypers, sensor = "Sentinel2")
# 
# wL.birch.meanByTree.S2 <- wL.birch.meanByTree %>%  dplyr::select(B1:STDEV) %>%      # Birch S2 by tree
#     map(spc_resample, wav = wav.hypers, sensor = "Sentinel2")


# Plot resampled leaf albedo by tree ------------------------------------------------

# Tidy data to data_frame i.e. rows = obs (sample trees), cols = vars (spectral bands)
wL.pine.meanByTree.S2.df <- ldply(wL.pine.meanByTree.S2)
wL.spruce.meanByTree.S2.df <- ldply(wL.spruce.meanByTree.S2)
wL.birch.meanByTree.S2.df <- ldply(wL.birch.meanByTree.S2)

# Add column of stand density (low, moderate, high)
stands <- read_csv2("data/leaf/stands.csv")
wL.pine.meanByTree.S2.df <- left_join(wL.pine.meanByTree.S2.df, stands, by = c(".id" = "Stand_id")) 
wL.spruce.meanByTree.S2.df <- left_join(wL.spruce.meanByTree.S2.df, stands, by = c(".id" = "Stand_id")) 
wL.birch.meanByTree.S2.df <- left_join(wL.birch.meanByTree.S2.df, stands, by = c(".id" = "Stand_id")) 


# Add column of band center wavelength, add after melt
wav.S2 <- read_csv2("data/image_data/wav_S2.csv")

require(reshape)
myPlot.wL <- function(df, legend.position = c(0.8,0.8), pt.shp = 1, text.label) {
  
  dplyr::filter(df, !.id %in% c("MEAN", "STDEV")) %>% dplyr::select(-c(b1, b9, b10)) %>% 
    melt(id.vars = c(".id", "Stand_dens", "Stand_treesPerHa"), variable_name = "band") %>% left_join(wav.S2, by = "band") %>% 
    mutate(Stand_dens = factor(Stand_dens, level = c("High", "Moderate", "Low"))) %>% 
    ggplot(aes(x = wav_center, y = value, group = .id, col = Stand_dens, shape = Stand_dens)) + geom_line(size = 0.3) + geom_point() + 
    scale_x_continuous(name = "Wavelength (nm)", limits = c(400,2300), breaks = seq(400,2300,200), expand = c(0,0)) +
    scale_y_continuous(name = "Leaf albedo", limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0,0)) +
    geom_text(x = 1300, y = 0.1, label = text.label, cex = 2, col = "black") +
    theme_bw(base_size = 8, base_family = "Helvetica") +
    theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
          axis.text = element_text(size = 7), axis.title.x = element_text(size = 8, face = "bold"), axis.title.y = element_text(size = 8, face = "bold"),
          legend.text = element_text(size = 7), legend.title = element_text(size = 7),
          legend.background = element_rect(fill = "transparent"),
          legend.position = legend.position) + guides(colour = guide_legend(override.aes = list(size = 1))) +
    scale_shape_manual(name = "Stand density", values = c(1,0,2)) + scale_color_manual(name = "Stand density", values = c("blue", "magenta", "green")) 
      
  }

source("tests/multiplot.R")

plot.wL.pine <- myPlot.wL(wL.pine.meanByTree.S2.df, legend.position = c(0.85,0.8), text.label = "Pine")        # Call the function
plot.wL.spruce <- myPlot.wL(wL.spruce.meanByTree.S2.df, legend.position = c(0.85,0.8), text.label = "Spruce")
plot.wL.birch <- myPlot.wL(wL.birch.meanByTree.S2.df, legend.position = c(0.85,0.8), text.label = "Birch")

pdf("graphs/wL_meanByTree.pdf", width = 5.5, height = 8, pointsize = 10)

multiplot(plot.wL.pine, plot.wL.spruce, plot.wL.birch, cols = 1)

dev.off()



# Leaf albedo variance by tree per species --------------------------------

wL.pine.meanByTree.S2.df %>% dplyr::filter(!.id %in% c("MEAN", "STDEV")) %>% 
  dplyr::select(-c(.id, Stand_treesPerHa, Stand_dens)) %>% apply(2, min) %>% 
  write.csv2("data/leaf/wL_pine_minOfTrees_S2.csv")

wL.pine.meanByTree.S2.df %>% dplyr::filter(!.id %in% c("MEAN", "STDEV")) %>% 
  dplyr::select(-c(.id, Stand_treesPerHa, Stand_dens)) %>% apply(2, max) %>% 
  write.csv2("data/leaf/wL_pine_maxOfTrees_S2.csv")


# Leaf albedo variance by stand dens. per species --------------------------------

wL.pine.meanByTree.S2.df %>% group_by(Stand_dens) %>% 
  summarise(b1 = mean(b1), b2 = mean(b2), b3 = mean(b3), b4 = mean(b4), b5 = mean(b5),
            b6 = mean(b6), b7 = mean(b7), b8 = mean(b8), b8a = mean(b8a), 
            b10 = mean(b10), b11 = mean(b11), b12 = mean(b12))



# [3] Leaf albedo by species --------------------------------------------------

# [3a] Average over leaf/needle sample type for each species --------------
# This is ok because all given equal weights
R.pine <- apply(cbind(R.pine.Ea$MEAN, R.pine.Eb$MEAN, R.pine.Sa$MEAN, R.pine.Sb$MEAN), 1, mean) # What do we do with negative values? 
T.pine <- apply(cbind(T.pine.Ea$MEAN, T.pine.Eb$MEAN, T.pine.Sa$MEAN, T.pine.Sb$MEAN), 1, mean)
wL.pine <- R.pine + T.pine

R.spruce <- apply(cbind(R.spruce.E0$MEAN, R.spruce.E1$MEAN, R.spruce.S$MEAN), 1, mean)
T.spruce <- apply(cbind(T.spruce.E0$MEAN, T.spruce.E1$MEAN, T.spruce.S$MEAN), 1, mean)
wL.spruce <- R.spruce + T.spruce

# Spruce with cohort weight (see Hovi et al., 2017)
R.spruce.1090 <- R.spruce.E0$MEAN*0.1*0.5 + R.spruce.E1$MEAN*0.9*0.5 + R.spruce.S$MEAN*0.5
T.spruce.1090 <- T.spruce.E0$MEAN*0.1*0.5 + T.spruce.E1$MEAN*0.9*0.5 + T.spruce.S$MEAN*0.5
wL.spruce.1090 <- R.spruce.1090 + T.spruce.1090

R.birch <- apply(cbind(R.birch.Ea$MEAN, R.birch.Eb$MEAN, R.birch.Sa$MEAN, R.birch.Sb$MEAN), 1, mean)
T.birch <- apply(cbind(T.birch.Ea$MEAN, T.birch.Eb$MEAN, T.birch.Sa$MEAN, T.birch.Sb$MEAN), 1, mean)
wL.birch <- R.birch + T.birch

write.csv2(tibble(Wav = leafData.Wav, Pine = wL.pine, Spruce = wL.spruce, Birch = wL.birch), 
           "data/leaf/wL_bySpecies.csv")

write.csv2(tibble(Wav = leafData.Wav, Pine = wL.pine, Spruce = wL.spruce.1090, Birch = wL.birch), 
           "data/leaf/wL_bySpecies_spruce1090.csv")


# Resample to S2A
wL_bySpecies <- read_csv2("data/leaf/wL_bySpecies.csv")
wL_bySpecies_S2A <- wL_bySpecies %>% dplyr::select(Pine:Birch) %>% 
  map_df(my_spectralResampling, wavelength=wL_bySpecies$Wav, sensor="Sentinel2A") %>% 
  mutate(Band=c("Blue", "Green", "Red", "RE1", "RE2", "RE3", "NIRnarrow", "SWIR1", "SWIR2"))
      
write.csv2(wL_bySpecies_S2A, "data/leaf/wL_bySpecies_S2A.csv")

# Resample to S2A (Spruce cohort weighted)
wL_bySpecies_spruce1090 <- read_csv2("data/leaf/wL_bySpecies_spruce1090.csv")                     
wL_bySpecies_spruce1090_S2A <- wL_bySpecies_spruce1090 %>% dplyr::select(Pine:Birch) %>% 
  map_df(my_spectralResampling, wavelength=wL_bySpecies_spruce1090$Wav, sensor="Sentinel2A") %>% 
  mutate(Band=c("Blue", "Green", "Red", "RE1", "RE2", "RE3", "NIRnarrow", "SWIR1", "SWIR2"))

write.csv2(wL_bySpecies_spruce1090_S2A, "data/leaf/wL_bySpecies_spruce1090_S2A.csv")



# [3b] Average for each species, only sunlit leaves -----------------------

R.pine.sunlit <- apply(cbind(R.pine.Ea$MEAN, R.pine.Eb$MEAN), 1, mean) # What do we do with negative values? 
T.pine.sunlit <- apply(cbind(T.pine.Ea$MEAN, T.pine.Eb$MEAN), 1, mean)
wL.pine.sunlit <- R.pine.sunlit + T.pine.sunlit

R.spruce.sunlit <- apply(cbind(R.spruce.E0$MEAN, R.spruce.E1$MEAN), 1, mean)
T.spruce.sunlit <- apply(cbind(T.spruce.E0$MEAN, T.spruce.E1$MEAN), 1, mean)
wL.spruce.sunlit <- R.spruce.sunlit + T.spruce.sunlit

# Spruce with cohort weighted
R.spruce.sunlit.1090 <- R.spruce.E0$MEAN*0.1 + R.spruce.E1$MEAN*0.9
T.spruce.sunlit.1090 <- T.spruce.E0$MEAN*0.1 + T.spruce.E1$MEAN*0.9
wL.spruce.sunlit.1090 <- R.spruce.sunlit.1090 + T.spruce.sunlit.1090

R.birch.sunlit <- apply(cbind(R.birch.Ea$MEAN, R.birch.Eb$MEAN), 1, mean)
T.birch.sunlit <- apply(cbind(T.birch.Ea$MEAN, T.birch.Eb$MEAN), 1, mean)
wL.birch.sunlit <- R.birch.sunlit + T.birch.sunlit

write.csv2(tibble(Wav = leafData.Wav, Pine = wL.pine.sunlit, Spruce = wL.spruce.sunlit, Birch = wL.birch.sunlit), 
           "data/leaf/wL_bySpecies_onlySunlit.csv")

# Spruce cohort weighted
write.csv2(tibble(Wav = leafData.Wav, Pine = wL.pine.sunlit, Spruce = wL.spruce.sunlit.1090, Birch = wL.birch.sunlit), 
           "data/leaf/wL_bySpecies_onlySunlit_spruce1090.csv")


# Resample to S2A
wL_bySpecies_onlySunlit <- read_csv2("data/leaf/wL_bySpecies_onlySunlit.csv")
wL_bySpecies_onlySunlit_S2A <- wL_bySpecies_onlySunlit %>% dplyr::select(Pine:Birch) %>% 
  map_df(my_spectralResampling, wavelength=wL_bySpecies_onlySunlit$Wav, sensor="Sentinel2A") %>% 
  mutate(Band=c("Blue", "Green", "Red", "RE1", "RE2", "RE3", "NIRnarrow", "SWIR1", "SWIR2"))

write.csv2(wL_bySpecies_onlySunlit_S2A, "data/leaf/wL_bySpecies_onlySunlit_S2A.csv")


# Resample to S2A (spruce cohort weighted)
wL_bySpecies_onlySunlit_spruce1090 <- read_csv2("data/leaf/wL_bySpecies_onlySunlit_spruce1090.csv")
wL_bySpecies_onlySunlit_spruce1090_S2A <- wL_bySpecies_onlySunlit_spruce1090 %>% dplyr::select(Pine:Birch) %>% 
  map_df(my_spectralResampling, wavelength=wL_bySpecies_onlySunlit_spruce1090$Wav, sensor="Sentinel2A") %>% 
  mutate(Band=c("Blue", "Green", "Red", "RE1", "RE2", "RE3", "NIRnarrow", "SWIR1", "SWIR2"))

write.csv2(wL_bySpecies_onlySunlit_spruce1090_S2A, "data/leaf/wL_bySpecies_onlySunlit_spruce1090_S2A.csv")



# Save mean spruce current year and mature shoots for different we --------
# Spruce E0
wL.spruce.E0.allMean <- R.spruce.E0$MEAN + T.spruce.E0$MEAN
# Spruce E1
wL.spruce.E1.allMean <- R.spruce.E1$MEAN + T.spruce.E1$MEAN
# Save data frame
wL.spruce.E0.E1.allMean <- tibble(Wl = leafData.Wav, E0 = wL.spruce.E0.allMean, E1 = wL.spruce.E1.allMean)
write.csv2(wL.spruce.E0.E1.allMean, "data/leaf/wL_spruce_E0_E1.csv")



# [3c] Average for each species, only shaded leaves -----------------------

R.pine.shaded <- apply(cbind(R.pine.Sa$MEAN, R.pine.Sb$MEAN), 1, mean) # What do we do with negative values? 
T.pine.shaded <- apply(cbind(T.pine.Sa$MEAN, T.pine.Sb$MEAN), 1, mean)
wL.pine.shaded <- R.pine.shaded + T.pine.shaded

R.spruce.shaded <- R.spruce.S$MEAN
T.spruce.shaded <- T.spruce.S$MEAN
wL.spruce.shaded <- R.spruce.shaded + T.spruce.shaded

R.birch.shaded <- apply(cbind(R.birch.Sa$MEAN, R.birch.Sb$MEAN), 1, mean)
T.birch.shaded <- apply(cbind(T.birch.Sa$MEAN, T.birch.Sb$MEAN), 1, mean)
wL.birch.shaded <- R.birch.shaded + T.birch.shaded

write.csv2(tibble(Wav = leafData.Wav, Pine = wL.pine.shaded, Spruce = wL.spruce.shaded, Birch = wL.birch.shaded), 
           "data/leaf/wL_bySpecies_onlyShaded.csv")

# Resample to S2A
wL_bySpecies_onlyShaded <- read_csv2("data/leaf/wL_bySpecies_onlyShaded.csv")
wL_bySpecies_onlyShaded_S2A <- wL_bySpecies_onlyShaded %>% dplyr::select(Pine:Birch) %>% 
  map_df(my_spectralResampling, wavelength=wL_bySpecies_onlyShaded$Wav, sensor="Sentinel2A") %>% 
  mutate(Band=c("Blue", "Green", "Red", "RE1", "RE2", "RE3", "NIRnarrow", "SWIR1", "SWIR2"))

write.csv2(wL_bySpecies_onlyShaded_S2A, "data/leaf/wL_bySpecies_onlyShaded_S2A.csv")




# Compare wL variations ---------------------------------------------------
# wL_bySpecies_onlyShaded_S2A
# wL_bySpecies_onlySunlit_S2A
# wL_bySpecies_S2A

S2.bandnames <- c("Blue", "Green", "Red", "RE1", "RE2", "RE3", "NIRnarrow", "SWIR1", "SWIR2")
S2.bandcenters <- wav.S2 %>% dplyr::filter(!band %in% c("b1", "b8", "b9", "b10")) %>% .[["wav_center"]]


# wL variation between sun-exposed and shaded leaves ----------------------

par(mfrow=c(1,3), ps=18)
# Pine
plot(S2.bandcenters, wL_bySpecies_onlyShaded_S2A$Pine, type="b", ylim=c(0,1), main="PINE", col="blue")
points(S2.bandcenters, wL_bySpecies_onlySunlit_spruce1090_S2A$Pine, type="b", col="red")
legend("topright", c("Shaded", "Sun-exposed"), lty=c(1,1), pch=c(1,1), col=c("blue", "red"))
# Spruce
plot(S2.bandcenters, wL_bySpecies_onlyShaded_S2A$Spruce, type="b", ylim=c(0,1), main="SPRUCE", col="blue")
points(S2.bandcenters, wL_bySpecies_onlySunlit_spruce1090_S2A$Spruce, type="b", col="red")                      
# points(S2.bandcenters, wL_bySpecies_onlySunlit_S2A$Spruce, type="b", col="red", lty=2)             # Spruce cohort equal weighted
# Birch
plot(S2.bandcenters, wL_bySpecies_onlyShaded_S2A$Birch, type="b", ylim=c(0,1), main="BIRCH", col="blue")
points(S2.bandcenters, wL_bySpecies_onlySunlit_spruce1090_S2A$Birch, type="b", col="red")



# wL variation from species composition -----------------------------------

wL_byPlot <- read_rds("results/gaps_and_testNilson_and_spectra.rds")
names(wL_byPlot)
str(wL_byPlot$wL.plot.S2A)

temp.fun <- function(species="Pine") {
  out <- wL_byPlot %>% dplyr::filter(Dominant_species==species) %>% 
    dplyr::select(wL.plot.S2A) %>% 
    mutate(Blue=map_dbl(wL.plot.S2A, function(z) z[attr(z, "names")=="Blue"]),
           Green=map_dbl(wL.plot.S2A, function(z) z[attr(z, "names")=="Green"]),
           Red=map_dbl(wL.plot.S2A, function(z) z[attr(z, "names")=="Red"]),
           RE1=map_dbl(wL.plot.S2A, function(z) z[attr(z, "names")=="RE1"]),
           RE2=map_dbl(wL.plot.S2A, function(z) z[attr(z, "names")=="RE2"]),
           RE3=map_dbl(wL.plot.S2A, function(z) z[attr(z, "names")=="RE3"]),
           NIRnarrow=map_dbl(wL.plot.S2A, function(z) z[attr(z, "names")=="NIRnarrow"]),
           SWIR1=map_dbl(wL.plot.S2A, function(z) z[attr(z, "names")=="SWIR1"]),
           SWIR2=map_dbl(wL.plot.S2A, function(z) z[attr(z, "names")=="SWIR2"])) %>% 
    summarise_at(vars(Blue:SWIR2), funs(min, max))
  
  return(out)
}

wL.byPlot.range.PINE <- temp.fun(species="Pine")
wL.byPlot.range.SPRUCE <- temp.fun(species="Spruce")
wL.byPlot.range.BIRCH <- temp.fun(species="Birch")

wL.byPlot.range.bySpecies <- tibble(Band=S2.bandnames,
       Pine.min=unlist(dplyr::select(wL.byPlot.range.PINE, Blue_min:SWIR2_min)),
       Spruce.min=unlist(dplyr::select(wL.byPlot.range.SPRUCE, Blue_min:SWIR2_min)),
       Birch.min=unlist(dplyr::select(wL.byPlot.range.BIRCH, Blue_min:SWIR2_min)),
       
       Pine.max=unlist(dplyr::select(wL.byPlot.range.PINE, Blue_max:SWIR2_max)),
       Spruce.max=unlist(dplyr::select(wL.byPlot.range.SPRUCE, Blue_max:SWIR2_max)),
       Birch.max=unlist(dplyr::select(wL.byPlot.range.BIRCH, Blue_max:SWIR2_max)))


par(mfrow=c(1,3), ps=18)
# Pine
plot(S2.bandcenters, wL.byPlot.range.bySpecies$Pine.min, type="b", ylim=c(0,1), main="PINE", col="blue")
points(S2.bandcenters, wL.byPlot.range.bySpecies$Pine.max, type="b", col="red")
# Spruce
plot(S2.bandcenters, wL.byPlot.range.bySpecies$Spruce.min, type="b", ylim=c(0,1), main="SPRUCE", col="blue")
points(S2.bandcenters, wL.byPlot.range.bySpecies$Spruce.max, type="b", col="red")                      
# Birch
plot(S2.bandcenters, wL.byPlot.range.bySpecies$Birch.min, type="b", ylim=c(0,1), main="BIRCH", col="blue")
points(S2.bandcenters, wL.byPlot.range.bySpecies$Birch.max, type="b", col="red")








# Below is possibly outdated!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# # Resample leaf albedo by species ----------------------------------------
# 
# # OK RESAMPLE TO SENTINEL-2, SO SAVE COLUMNS wL.b1, wL.b2, ... , wL.b13
# wav <- R.pine.Ea$Wl   # Wav of all spectra are the same
# # Need to change this to my_spectralResampling()
# # source("munge/spc_resample.R")                              # Resample wL to intended sensor
# # wL.pine.S2 <- spc_resample(wL.pine, wav, "Sentinel2")
# # wL.spruce.S2 <- spc_resample(wL.spruce, wav, "Sentinel2")
# # wL.birch.S2 <- spc_resample(wL.birch, wav, "Sentinel2")    
# 
# ( wL.3Species.S2 <- bind_rows(wL.pine.S2, wL.spruce.S2, wL.birch.S2) %>% 
#     mutate(species = c("Pine", "Spruce", "Birch")) )
# 
# # Remove non-land wavelengths (60-m b1, b9, b10)
# wL.3Species.S2 <- dplyr::select(wL.3Species.S2, -c(b1, b9, b10))
# write.csv2(wL.3Species.S2, "data/leaf/wL_bySpecies_S2.csv")                          # Write to disk
# 
# 
# 
# # Plot leaf albedo by species ---------------------------------------------
# 
# myPlot.wL.3Species <- function(df, legend.position = c(0.85,0.85), pt.shp = 1) {
#   
#   melt(df, id.vars = "species", variable_name = "band") %>% left_join(wav.S2, by = "band") %>% 
#     mutate(class = factor(species, level = c("Pine", "Spruce", "Birch"))) %>% 
#     ggplot(aes(x = wav_center, y = value, col = species, shape = species)) + geom_line(size = 0.3) + geom_point() + 
#     scale_x_continuous(name = "Wavelength (nm)", limits = c(400,2300), breaks = seq(400,2300,200), expand = c(0,0)) +
#     scale_y_continuous(name = "Leaf albedo", limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0,0)) +
#     theme_bw(base_size = 8) +
#     theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
#           axis.text = element_text(size = 7), axis.title.x = element_text(size = 8, face = "bold"), axis.title.y = element_text(size = 8, face = "bold"),
#           legend.text = element_text(size = 7), legend.title = element_text(size = 7),
#           legend.background = element_rect(fill = "transparent"),
#           legend.position = legend.position) + guides(colour = guide_legend(override.aes = list(size = 1))) +
#     scale_shape_manual(name = "", values = c(1,0,2)) + scale_color_manual(name = "", values = c("blue", "magenta", "green")) 
#   
# }
# 
# plot.wL.3Species <- myPlot.wL.3Species(wL.3Species.S2, legend.position = c(0.85,0.8))   # Call the plotting function
# 
# pdf("graphs/wL_meanByTree_meanBySpecies.pdf", width = 5.5, height = 10, pointsize = 10)
# 
# multiplot(plot.wL.pine, plot.wL.spruce, plot.wL.birch, plot.wL.3Species, cols = 1)
# 
# dev.off()
# 
# 
# 
# # Plot plot-level species-weighted leaf albedo ---------------------------
# 
# wL.weighted <- read_csv2("data/leaf/wL_speciesWeighted_byPlot.csv") %>% dplyr::select(-X1)
# 
# myPlot.wL.weighted <- function(df, legend.position = c(0.85,0.85), pt.shp = 1) {
#   
#     melt(as.data.frame(df), id.vars = "ID", variable_name = "band") %>%                # ! melt works with data.frame! 
#     left_join(wav.S2, by = "band") %>% 
#     ggplot(aes(x = wav_center, y = value, group = ID)) + geom_line(size = 0.1, col = "grey40") + geom_point(col = "grey40", shape = 1) + 
#     scale_x_continuous(name = "Wavelength (nm)", limits = c(400,2300), breaks = seq(400,2300,200), expand = c(0,0)) +
#     scale_y_continuous(name = "Leaf albedo", limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0,0)) +
#     theme_bw(base_size = 8, base_family = "Helvetica") +
#     theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
#           axis.text = element_text(size = 7), axis.title.x = element_text(size = 8, face = "bold"), axis.title.y = element_text(size = 8, face = "bold"),
#           legend.text = element_text(size = 7), legend.title = element_text(size = 7),
#           legend.background = element_rect(fill = "transparent"),
#           legend.position = legend.position) + guides(colour = guide_legend(override.aes = list(size = 1)))
#    
# }
# 
# pdf("graphs/wL_byPlot_speciesWeighted.pdf", width = 5.5, height = 2.5, pointsize = 10)
# myPlot.wL.weighted(wL.weighted)
# dev.off()










