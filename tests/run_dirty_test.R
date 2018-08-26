# Run the function --------------------------------------------------------

source("tests/dirty_test.R")

gaps.Nilson <- read_csv2("results/gaps_and_testNilson.csv") 
SZA.S2A <- 41.33                # SZA of the Sentinel-2 scene 17 Aug 2015
SZA.L8 <- 49.82                 # SZA of the Landsat-8 scene 20 Aug 2015



# wL from measurements, species-weighted for plot -------------------------

# Else use species mean
wL.finnish <- read_csv2("data/leaf/wL_bySpecies.csv", col_types = cols())  # Equal weights sunlit vs shaded, current vs older spruce needle

# Weight by species  
calc_wL <- function(Prop_pine, Prop_spruce, Prop_birch, as.list = FALSE) {
  res <- (wL.finnish$Pine * Prop_pine) + (wL.finnish$Spruce * Prop_spruce) + (wL.finnish$Birch * Prop_birch)
  if(as.list == TRUE){
    out <- as.list(res)
    names(out) <- wL.finnish$Wav
  } else {
    out <- res
    attr(out, "Wav") <- wL.finnish$Wav
  }
  
  return(out)
}

# Attempt 1 map by row
test1 <- gaps.Nilson %>% split(.$ID) %>%
  purrr::map(function(z) calc_wL(z$Prop_pine, z$Prop_spruce, z$Prop_birch, as.list = FALSE))

# # Attempt 2 create list column from function returning a list
# test2 <- gaps.Nilson %>% mutate(wL.plot = calc_wL(Prop_pine, Prop_spruce, Prop_birch))

# Attempt 3 create list column from a named list
test3 <- tibble::enframe(test1) %>% rename(wL.plot = value)

# Attempt 4 add columns from a separate list column
test4 <- left_join(gaps.Nilson, test3, by = c("ID" = "name"))

# Succeed with attempt 4, which depends on attempt 1 and 3
gaps.Nilson <- test4

# Resample: map over list column, how cool is that!
gaps.Nilson <- gaps.Nilson %>% 
  mutate(wL.plot.S2A = map(wL.plot, my_spectralResampling, wavelength = wL.finnish$Wav, sensor = "Sentinel2A"),
         wL.plot.L8 = map(wL.plot, my_spectralResampling, wavelength = wL.finnish$Wav, sensor = "Landsat8"))

# Keep selected bands of wL
sel.bands <- c("Red", "NIR", "NIRnarrow")
gaps.Nilson <- gaps.Nilson %>% 
  mutate(wL.plot.S2A.sel = map(wL.plot.S2A, function(z) z[names(z) %in% sel.bands]),
         wL.plot.L8.sel = map(wL.plot.L8, function(z) z[names(z) %in% sel.bands]))


# Rg from measurements, to assign plot by site type -------------------------

Rg.finnish <- read_csv2("data/understory/understory_Refl_3class.csv", col_types = cols())      

assign.Rg <- function(Site_type_lab){
  out <- Rg.finnish[[Site_type_lab]]
  attr(out, "Wav") <- Rg.finnish$Wl
  return(out)
}

Rg.lsc <- gaps.Nilson %>% split(.$ID) %>%
              purrr::map(function(z) assign.Rg(z$Site_type_lab)) %>% 
              enframe() %>% rename(Rg.plot = value)

gaps.Nilson <- left_join(gaps.Nilson, Rg.lsc, by = c("ID" = "name"))

# Resample: map over list column
gaps.Nilson <- gaps.Nilson %>% 
  mutate(Rg.plot.S2A = map(Rg.plot, my_spectralResampling, wavelength = Rg.finnish$Wl, sensor = "Sentinel2A"),
         Rg.plot.L8 = map(Rg.plot, my_spectralResampling, wavelength = Rg.finnish$Wl, sensor = "Landsat8"))


# Keep selected bands of wL
sel.bands <- c("Red", "NIR", "NIRnarrow")
gaps.Nilson <- gaps.Nilson %>% 
  mutate(Rg.plot.S2A.sel = map(Rg.plot.S2A, function(z) z[names(z) %in% sel.bands]),
         Rg.plot.L8.sel = map(Rg.plot.L8, function(z) z[names(z) %in% sel.bands]))




# Save gaps with Nilson correction, and spectra ---------------------------

write_rds(gaps.Nilson, "results/gaps_and_testNilson_and_spectra.rds")


# Custom wL and Rg --------------------------------------------------------

custom.wL <- c(0.1, 0.7)                          # Rautiainen et al. (2005) Saarinen & Puumala
attr(custom.wL, "Wav") <- c("red", "NIR")

custom.Rg <- c(0.07, 0.2)
attr(custom.Rg, "Wav") <- c("red", "NIR")


# Arbitrary wL and Rg -----------------------------------------------------

# Sentinel-2 Nilson (crown-level clumping corrected LAI)
temp <- gaps.Nilson %>% mutate(LAIeff = PAI_estimate) %>% # Replace LAIeff with crown-level corrected LAIeff
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch) %>% 
  plyr::mdply(PARAS.arbitrary, var = "BRF",
              wL = custom.wL,
              Rg = custom.Rg,
              VZA = 0, 
              SZA = SZA.S2A,                                      # SZA
              default.finnish.species = TRUE)   


temp %>% write.csv2("results/parasArbitrary1_BRF_fixedInputs_S2A_Nilson.csv")

# Landsat-8 Nilson
temp <- gaps.Nilson %>% mutate(LAIeff = PAI_estimate) %>% # Replace LAIeff with crown-level corrected LAIeff
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch) %>% 
  plyr::mdply(PARAS.arbitrary, var = "BRF",
              wL = custom.wL,
              Rg = custom.Rg,
              VZA = 0, 
              SZA = SZA.L8,                                      # SZA
              default.finnish.species = TRUE)   

temp %>% write.csv2("results/parasArbitrary1_BRF_fixedInputs_L8_Nilson.csv")


# Sentinel-2 
temp <- gaps.Nilson %>%  # Don't replace LAIeff with crown-level corrected LAIeff
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch) %>% 
  plyr::mdply(PARAS.arbitrary, var = "BRF",
              wL = custom.wL,
              Rg = custom.Rg,
              VZA = 0, 
              SZA = SZA.S2A,                                      # SZA
              default.finnish.species = TRUE)   

temp %>% write.csv2("results/parasArbitrary1_BRF_fixedInputs_S2A.csv")


# Landsat-8 
temp <- gaps.Nilson %>%  # Don't replace LAIeff with crown-level corrected LAIeff
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch) %>% 
  plyr::mdply(PARAS.arbitrary, var = "BRF",
              wL = custom.wL,
              Rg = custom.Rg,
              VZA = 0, 
              SZA = SZA.L8,                                      # SZA
              default.finnish.species = TRUE)   

temp %>% write.csv2("results/parasArbitrary1_BRF_fixedInputs_L8.csv")



# Arbitrary Rg -----------------------------------------------------

# Sentinel-2 Nilson (crown-level clumping corrected LAI)
temp <- gaps.Nilson %>% mutate(LAIeff = PAI_estimate,              # Replace LAIeff with crown-level corrected LAIeff
                               wL = wL.plot.S2A.sel) %>%           # Create a wL column based on other column
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch, 
                wL) %>%                                          # wL is internal
  plyr::mdply(PARAS.arbitrary, var = "BRF",
              Rg = custom.Rg,
              VZA = 0, 
              SZA = SZA.S2A,                                      # SZA
              default.finnish.species = TRUE)   


temp %>% dplyr::select(-wL) %>%  # Omit list column
  write.csv2("results/parasArbitrary2_BRF_fixedInputs_S2A_Nilson.csv")

# Landsat-8 Nilson
temp <- gaps.Nilson %>% mutate(LAIeff = PAI_estimate,              # Replace LAIeff with crown-level corrected LAIeff
                               wL = wL.plot.L8.sel) %>%           # Create a wL column based on other column
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch, 
                wL) %>%                                          # wL is internal
  plyr::mdply(PARAS.arbitrary, var = "BRF",
              Rg = custom.Rg,
              VZA = 0, 
              SZA = SZA.L8,                                      # SZA
              default.finnish.species = TRUE)   

temp %>% dplyr::select(-wL) %>%  # Omit list column
  write.csv2("results/parasArbitrary2_BRF_fixedInputs_L8_Nilson.csv")


# Sentinel-2 without Nilson
temp <- gaps.Nilson %>% mutate(wL = wL.plot.S2A.sel) %>%  # Don't replace LAIeff with crown-level corrected LAIeff
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch,
                wL) %>% 
  plyr::mdply(PARAS.arbitrary, var = "BRF",
              Rg = custom.Rg,
              VZA = 0, 
              SZA = SZA.S2A,                                      # SZA
              default.finnish.species = TRUE)   

temp %>% dplyr::select(-wL) %>%  # Omit list column
  write.csv2("results/parasArbitrary2_BRF_fixedInputs_S2A.csv")


# Landsat-8 without Nilson
temp <- gaps.Nilson %>% mutate(wL = wL.plot.L8.sel) %>%  # Don't replace LAIeff with crown-level corrected LAIeff
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch,
                wL) %>% 
  plyr::mdply(PARAS.arbitrary, var = "BRF",
              Rg = custom.Rg,
              VZA = 0, 
              SZA = SZA.L8,                                      # SZA
              default.finnish.species = TRUE)   

temp %>% dplyr::select(-wL) %>%  # Omit list column
  write.csv2("results/parasArbitrary2_BRF_fixedInputs_L8.csv")



# Arbitrary wL ------------------------------------------------------------
# Sentinel-2 Nilson (crown-level clumping corrected LAI)

temp <- gaps.Nilson %>% mutate(LAIeff = PAI_estimate,              # Replace LAIeff with crown-level corrected LAIeff
                               Rg = Rg.plot.S2A.sel) %>%           # Create a Rg column based on other column
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch, 
                Rg) %>%                                          # wL is internal
  plyr::mdply(PARAS.arbitrary, var = "BRF",
              wL = custom.wL,
              VZA = 0, 
              SZA = SZA.S2A,                                      # SZA
              default.finnish.species = TRUE)   


temp %>% dplyr::select(-Rg) %>%  # Omit list column
  write.csv2("results/parasArbitrary3_BRF_fixedInputs_S2A_Nilson.csv")

# Landsat-8 Nilson
temp <- gaps.Nilson %>% mutate(LAIeff = PAI_estimate,              # Replace LAIeff with crown-level corrected LAIeff
                               Rg = Rg.plot.L8.sel) %>%           # Create a Rg column based on other column
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch, 
                Rg) %>%                                          # wL is internal
  plyr::mdply(PARAS.arbitrary, var = "BRF",
              wL = custom.wL,
              VZA = 0, 
              SZA = SZA.L8,                                      # SZA
              default.finnish.species = TRUE)   

temp %>% dplyr::select(-Rg) %>%  # Omit list column
  write.csv2("results/parasArbitrary3_BRF_fixedInputs_L8_Nilson.csv")


# Sentinel-2 without Nilson
temp <- gaps.Nilson %>% mutate(Rg = Rg.plot.S2A.sel) %>%  # Don't replace LAIeff with crown-level corrected LAIeff
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch,
                Rg) %>% 
  plyr::mdply(PARAS.arbitrary, var = "BRF",
              wL = custom.wL,
              VZA = 0, 
              SZA = SZA.S2A,                                      # SZA
              default.finnish.species = TRUE)   

temp %>% dplyr::select(-Rg) %>%  # Omit list column
  write.csv2("results/parasArbitrary3_BRF_fixedInputs_S2A.csv")


# Landsat-8 without Nilson
temp <- gaps.Nilson %>% mutate(Rg = Rg.plot.L8.sel) %>%  # Don't replace LAIeff with crown-level corrected LAIeff
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch,
                Rg) %>% 
  plyr::mdply(PARAS.arbitrary, var = "BRF",
              wL = custom.wL,
              VZA = 0, 
              SZA = SZA.L8,                                      # SZA
              default.finnish.species = TRUE)   

temp %>% dplyr::select(-Rg) %>%  # Omit list column
  write.csv2("results/parasArbitrary3_BRF_fixedInputs_L8.csv")




# Rg = image-based understory endmember -----------------------------------
# Based on Hyytiälä endmember in file in D:\Aalto\PHD_RESEARCH\STUDY_I\Revisit_StudyI\EM_for_Aarne
imgEM.Rg <- c(0.038446275, 0.066929588, 0.076105729, 0.174908679, 0.226481046, 0.146087669)
attr(imgEM.Rg, "Wav") <- c("Blue", "Green", "Red", "NIR", "SWIR1", "SWIR2")

# Order is different L8, S2, ...
# Landsat-8 without Nilson
temp <- gaps.Nilson %>% mutate(wL = wL.plot.L8) %>%  # Don't replace LAIeff with crown-level corrected LAIeff
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch,
                wL) %>% 
  plyr::mdply(PARAS.arbitrary, var = "BRF",
              Rg = imgEM.Rg,
              VZA = 0, 
              SZA = SZA.L8,                                      # SZA
              default.finnish.species = TRUE)   

temp %>% dplyr::select(-wL) %>%  # Omit list column
  write.csv2("results/parasArbitrary4_BRF_fixedInputs_L8.csv")



# Landsat-8 Nilson
temp <- gaps.Nilson %>% mutate(LAIeff = PAI_estimate,              # Replace LAIeff with crown-level corrected LAIeff
                               wL = wL.plot.L8) %>%           # Create a wL column based on other column
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch, 
                wL) %>%                                          # wL is internal
  plyr::mdply(PARAS.arbitrary, var = "BRF",
              Rg = imgEM.Rg,
              VZA = 0, 
              SZA = SZA.L8,                                      # SZA
              default.finnish.species = TRUE)   

temp %>% dplyr::select(-wL) %>%  # Omit list column
  write.csv2("results/parasArbitrary4_BRF_fixedInputs_L8_Nilson.csv")


# Todo Sentinel2














