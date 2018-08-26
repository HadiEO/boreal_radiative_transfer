
# Input A: Canopy structure ----------------------------------------------------------
gaps <- read_csv2("results/gaps.csv")


# Add VZA and SZA 
# If differ between plots, add as columns to gaps data frame 
# In this case, all plots have same angles, so can add as external input for mdply call below
SZA.S2A <- 41.33                # SZA of the Sentinel-2 scene 17 Aug 2015
SZA.L8 <- 49.82                # SZA of the Landsat-8 scene 20 Aug 2015

# VZA = gaps1 in PARAS() *****************************

# Input B: Leaf single scattering albedo  -------------------------------------------


 # Input C: Understory reflectance -----------------------------------------


# Run PARAS BRF simulation ------------------------------------------------
source("tests/PARAS.R")                                                   ###################### source the PARAS functions
# Easiest to make it run with input data frame. Must name the variables as the function input arguments' names
# plyr::splat() doesn't accept additional arguments
# Consider pmap solution
# gaps %>% 
#   dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
#                 Prop_pine, Prop_spruce, Prop_birch,
#                 Site_type_lab) %>% 
#   pmap_df(.f = paras_BRF,
#           VZA = 0, 
#           SZA = SZA.S2A, 
#           default.finnish.species = TRUE,
#           default.finnish.understory = TRUE,
#           resample = TRUE, sensor = "Sentinel2A",
#           output.as.df = TRUE, output.intermediate = TRUE)

# Resample to Sentinel2A
temp <- gaps[1,] %>% 
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch,
                Site_type_lab) %>% 
  plyr::mdply(PARAS, var = "BRF",
              VZA = 0, 
              SZA = SZA.S2A, 
              default.finnish.species = TRUE,
              default.finnish.understory = TRUE,
              resample = TRUE, sensor = "Sentinel2A",
              output.as.df = TRUE, output.intermediate = TRUE)  
 

temp %>% dplyr::select(ID, Blue:SWIR2) %>% write.csv2("results/paras_BRF_fixedInputs_S2A.csv")
temp %>% dplyr::select(ID, cgf.view:p, Blue:SWIR2) %>% write.csv2("results/paras_BRF_fixedInputs_S2A_extended.csv")

# Resample to Landsat8
temp <- gaps %>% 
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch,
                Site_type_lab) %>% 
  plyr::mdply(PARAS, var = "BRF",
              VZA = 0, 
              SZA = SZA.L8, 
              default.finnish.species = TRUE,
              default.finnish.understory = TRUE,
              resample = TRUE, sensor = "Landsat8",
              output.as.df = TRUE, output.intermediate = TRUE) 

temp %>% dplyr::select(ID, Blue:SWIR2) %>% write.csv2("results/paras_BRF_fixedInputs_L8.csv")
temp %>% dplyr::select(ID, cgf.view:p, Blue:SWIR2) %>% write.csv2("results/paras_BRF_fixedInputs_L8_extended.csv")


# Run PARAS albedo simulation ------------------------------------------------
source("tests/PARAS.R") 

# Resample to Sentinel2A
temp <- gaps %>% 
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch,
                Site_type_lab) %>% 
  plyr::mdply(PARAS, var = "albedo", scale.Albedo = FALSE,          # var, scale albedo
              VZA = 0, 
              SZA = SZA.S2A, 
              default.finnish.species = TRUE,
              default.finnish.understory = TRUE,
              resample = TRUE, sensor = "Sentinel2A",
              output.as.df = TRUE, output.intermediate = FALSE)  


temp %>% dplyr::select(ID, Blue:SWIR2) %>% write.csv2("results/paras_Albedo_fixedInputs_S2A.csv")
temp %>% dplyr::select(ID, cgf.view:p, Blue:SWIR2) %>% write.csv2("results/paras_Albedo_fixedInputs_S2A_extended.csv")



# Resample to Landsat8
temp <- gaps %>% 
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch,
                Site_type_lab) %>% 
  plyr::mdply(PARAS, var = "albedo", scale.Albedo = FALSE,
              VZA = 0, 
              SZA = SZA.L8, 
              default.finnish.species = TRUE,
              default.finnish.understory = TRUE,
              resample = TRUE, sensor = "Landsat8",
              output.as.df = TRUE, output.intermediate = FALSE) 

temp %>% dplyr::select(ID, Blue:SWIR2) %>% write.csv2("results/paras_Albedo_fixedInputs_L8.csv")
temp %>% dplyr::select(ID, cgf.view:p, Blue:SWIR2) %>% write.csv2("results/paras_Albedo_fixedInputs_L8_extended.csv")



# Run simulation of crown-level corrected LAI -----------------------------
# Resample to Sentinel2A
temp <- read_csv2("results/gaps_and_testNilson.csv") 
  
temp <- temp %>% mutate(LAIeff = PAI_estimate) %>% # Replace LAIeff with crown-level corrected LAIeff
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch,
                Site_type_lab) %>% 
  plyr::mdply(PARAS, var = "albedo", scale.Albedo = FALSE,          # var, scale albedo
              VZA = 0, 
              SZA = SZA.S2A, 
              default.finnish.species = TRUE,
              default.finnish.understory = TRUE,
              resample = TRUE, sensor = "Sentinel2A",
              output.as.df = TRUE, output.intermediate = TRUE)  

temp %>% dplyr::select(ID, cgf.view:p, Blue:SWIR2) %>% write.csv2("results/paras_BRF_fixedInputs_S2A_extended_Nilson.csv")
temp %>% dplyr::select(ID, cgf.view:p, Blue:SWIR2) %>% write.csv2("results/paras_Albedo_fixedInputs_S2A_extended_Nilson.csv")

#**********************************************************
# Resample to Sentinel2A - updated (19.12.2017) relative spectral response function
#**********************************************************
temp <- read_csv2("results/gaps_and_testNilson.csv") 
temp <- temp %>% mutate(LAIeff = PAI_estimate) %>% # Replace LAIeff with crown-level corrected LAIeff
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch,
                Site_type_lab) %>% 
  plyr::mdply(PARAS, var = "BRF", scale.Albedo = FALSE,          # var, scale albedo
              VZA = 0,                                           # VZA = gaps1 in PARAS()
              SZA = SZA.S2A, 
              default.finnish.species = TRUE,
              default.finnish.understory = TRUE,
              resample = TRUE, sensor = "Sentinel2A_new",       # new
              output.as.df = TRUE, output.intermediate = TRUE)  

temp %>% dplyr::select(ID, cgf.view:p, Blue:SWIR2) %>% write.csv2("results/paras_BRF_fixedInputs_S2A_extended_Nilson_newRSR.csv")

#**********************************************************
# VZA = 5 degrees, line 99 in PARAS() uncommented, line 100 commented out, temporarily
#**********************************************************
temp <- read_csv2("results/gaps_and_testNilson.csv") 
temp <- temp %>% mutate(LAIeff = PAI_estimate) %>% # Replace LAIeff with crown-level corrected LAIeff
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch,
                Site_type_lab) %>% 
  plyr::mdply(PARAS, var = "BRF", scale.Albedo = FALSE,          # var, scale albedo
              VZA = 5,                                           # # VZA = gaps1 in PARAS(), try using VZA at plot locations
              SZA = SZA.S2A, 
              default.finnish.species = TRUE,
              default.finnish.understory = TRUE,
              resample = TRUE, sensor = "Sentinel2A_new",       # new
              output.as.df = TRUE, output.intermediate = TRUE)  

temp %>% dplyr::select(ID, cgf.view:p, Blue:SWIR2) %>% write.csv2("results/paras_BRF_fixedInputs_S2A_extended_Nilson_newRSR_vza5.csv")


#**********************************************************
# Resample to Sentinel2A - updated RSR, save omega_C, canopy contribution
#**********************************************************
temp <- read_csv2("results/gaps_and_testNilson.csv") 
temp <- temp %>% mutate(LAIeff = PAI_estimate) %>% # Replace LAIeff with crown-level corrected LAIeff
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch,
                Site_type_lab) %>% 
  plyr::mdply(PARAS.addOutput, var = "BRF", scale.Albedo = FALSE,          # var, scale albedo
              VZA = 0,                                           # VZA = gaps1 in PARAS()
              SZA = SZA.S2A, 
              default.finnish.species = TRUE,
              default.finnish.understory = TRUE,
              resample = TRUE, sensor = "Sentinel2A_new",       # new
              output.as.df = TRUE, output.intermediate = TRUE)  

temp %>% dplyr::select(ID, cgf.view:p, Blue:SWIR2, wC.Blue:wC.SWIR2,
                       can.Blue:can.SWIR2) %>% write.csv2("results/paras_BRF_fixedInputs_S2A_extended_Nilson_newRSR_wC.csv")

#**********************************************************
# Resample to Landsat8 - updated RSR, save omega_C, canopy contribution
#**********************************************************
temp <- read_csv2("results/gaps_and_testNilson.csv") 
temp <- temp %>% mutate(LAIeff = PAI_estimate) %>% # Replace LAIeff with crown-level corrected LAIeff
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch,
                Site_type_lab) %>% 
  plyr::mdply(PARAS.addOutput, var = "BRF", scale.Albedo = FALSE,          # var, scale albedo
              VZA = 0,                                           # VZA = gaps1 in PARAS()
              SZA = SZA.S2A, 
              default.finnish.species = TRUE,
              default.finnish.understory = TRUE,
              resample = TRUE, sensor = "Landsat8",       # new
              output.as.df = TRUE, output.intermediate = TRUE)  

temp %>% dplyr::select(ID, cgf.view:p, Blue:SWIR2, wC.Blue:wC.SWIR2,
                       can.Blue:can.SWIR2) %>% write.csv2("results/paras_BRF_fixedInputs_L8_extended_Nilson_newRSR_wC.csv")





# Resample to Landsat8
temp <- read_csv2("results/gaps_and_testNilson.csv") 

temp <- temp %>% mutate(LAIeff = PAI_estimate) %>% # Replace LAIeff with crown-level corrected LAIeff
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch,
                Site_type_lab) %>% 
  plyr::mdply(PARAS, var = "albedo", scale.Albedo = FALSE,          # var, scale albedo
              VZA = 0, 
              SZA = SZA.L8, 
              default.finnish.species = TRUE,
              default.finnish.understory = TRUE,
              resample = TRUE, sensor = "Landsat8",
              output.as.df = TRUE, output.intermediate = TRUE)  

temp %>% dplyr::select(ID, cgf.view:p, Blue:SWIR2) %>% write.csv2("results/paras_BRF_fixedInputs_L8_extended_Nilson.csv")
temp %>% dplyr::select(ID, cgf.view:p, Blue:SWIR2) %>% write.csv2("results/paras_Albedo_fixedInputs_L8_extended_Nilson.csv")




# Simulation using only sunlit leaves -------------------------------------

# Resample to Landsat8
temp <- read_csv2("results/gaps_and_testNilson.csv") 

temp <- temp %>% mutate(LAIeff = PAI_estimate) %>% # Replace LAIeff with crown-level corrected LAIeff
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch,
                Site_type_lab) %>% 
  plyr::mdply(PARAS, var = "BRF", scale.Albedo = FALSE, only.sunlit.wL = TRUE,          # var, scale albedo
              VZA = 0, 
              SZA = SZA.L8, 
              default.finnish.species = TRUE,
              default.finnish.understory = TRUE,
              resample = TRUE, sensor = "Landsat8",
              output.as.df = TRUE, output.intermediate = TRUE)  

temp %>% dplyr::select(ID, cgf.view:p, Blue:SWIR2) %>% write.csv2("results/paras_BRF_fixedInputs_L8_extended_Nilson_onlySunExposedLeaves.csv")

# Simulation for spruce using 100% current year vs 100% older needles -----------------------

# Resample to Landsat8
temp <- read_csv2("results/gaps_and_testNilson.csv") 

temp <- temp %>% dplyr::filter(Dominant_species == 'Spruce') %>% 
  mutate(LAIeff = PAI_estimate) %>% # Replace LAIeff with crown-level corrected LAIeff
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch,
                Site_type_lab) %>% 
  plyr::mdply(PARAS, var = "BRF", scale.Albedo = FALSE,  spruce.currentYear = 1,        # var, scale albedo
              VZA = 0, 
              SZA = SZA.L8, 
              default.finnish.species = TRUE,
              default.finnish.understory = TRUE,
              resample = TRUE, sensor = "Landsat8",
              output.as.df = TRUE, output.intermediate = TRUE)  

temp %>% dplyr::select(ID, cgf.view:p, Blue:SWIR2) %>% 
  write.csv2("results/Spruce_simBRF_onlyCurrentYear_L8_Nilson.csv")



# Test correct birch clumping ---------------------------------------------
# In Ryu et al. (2010), element clumping index (beyond shoot) reported 0.98 (LAIe 3.65) and 0.93 (LAIe 3.29) for birch
# in Estonia and Canada respectively


temp <- read_csv2("results/gaps_and_testNilson.csv") 

temp[temp$Dominant_species == "Birch", "PAI_estimate"] <- 
  temp[temp$Dominant_species == "Birch", "PAI_estimate"] / 0.9

temp <- temp %>% mutate(LAIeff = PAI_estimate) %>% # Replace LAIeff with crown-level corrected LAIeff
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch,
                Site_type_lab) %>% 
  plyr::mdply(PARAS, var = "BRF", scale.Albedo = FALSE,        
              VZA = 0, 
              SZA = SZA.L8, 
              default.finnish.species = TRUE,
              default.finnish.understory = TRUE,
              resample = TRUE, sensor = "Landsat8",
              output.as.df = TRUE, output.intermediate = TRUE)  

temp %>% dplyr::select(ID, cgf.view:p, Blue:SWIR2) %>% write.csv2("results/paras_BRF_fixedInputs_L8_extended_Nilson_birchCI09.csv")



# Test incorporating stem spectra -----------------------------------------
# BAI/LAI ratio = 0.18 for conifers, 0.15 for birch (Hovi et al. 2017, AFM)

# Resample to Landsat8
temp <- read_csv2("results/gaps_and_testNilson.csv") 

temp <- temp %>% mutate(LAIeff = PAI_estimate) %>% # Replace LAIeff with crown-level corrected LAIeff
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch,
                Site_type_lab) %>% 
  plyr::mdply(PARAS, var = "BRF", scale.Albedo = FALSE,          # var, scale albedo
              VZA = 0, 
              SZA = SZA.L8, 
              default.finnish.species = TRUE,
              default.finnish.understory = TRUE,
              resample = TRUE, sensor = "Landsat8",
              output.as.df = TRUE, output.intermediate = TRUE,
              BAI2LAI = list(Pine = 0.18, Spruce = 0.18, Birch = 0.15))  

temp %>% dplyr::select(ID, cgf.view:p, Blue:SWIR2) %>% write.csv2("results/paras_BRF_fixedInputs_L8_extended_Nilson_stemAsHovi2017.csv")




# Case not knowing site type ----------------------------------------------

# Resample to Sentinel2A
temp <- read_csv2("results/gaps_and_testNilson.csv") 

temp <- temp %>% mutate(LAIeff = PAI_estimate, # Replace LAIeff with crown-level corrected LAIeff
                        Site_type_lab = "Herb-rich") %>%  # Change site type
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch,
                Site_type_lab) %>% 
  plyr::mdply(PARAS, var = "BRF", scale.Albedo = FALSE,          # var, scale albedo
              VZA = 0, 
              SZA = SZA.S2A, 
              default.finnish.species = TRUE,
              default.finnish.understory = TRUE,
              resample = TRUE, sensor = "Sentinel2A",
              output.as.df = TRUE, output.intermediate = TRUE)  

# temp %>% dplyr::select(ID, cgf.view:p, Blue:SWIR2) %>% write.csv2("results/paras_BRF_fixedInputs_S2A_Nilson_allXeric.csv")
# temp %>% dplyr::select(ID, cgf.view:p, Blue:SWIR2) %>% write.csv2("results/paras_BRF_fixedInputs_S2A_Nilson_allMesic.csv")
temp %>% dplyr::select(ID, cgf.view:p, Blue:SWIR2) %>% write.csv2("results/paras_BRF_fixedInputs_S2A_Nilson_allHerbRich.csv")




# Scale modelled BRF to satellite-observed values -------------------------
# Mean difference (relative to observed)
# See results/stats

# Resample to Sentinel2A
temp <- read_csv2("results/gaps_and_testNilson.csv") 

temp <- temp %>% mutate(LAIeff = PAI_estimate) %>% # Replace LAIeff with crown-level corrected LAIeff
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch,
                Site_type_lab) %>% 
  plyr::mdply(PARAS, var = "BRF", scale.Albedo = FALSE,          # var, scale albedo
              VZA = 0, 
              SZA = SZA.S2A, 
              default.finnish.species = TRUE,
              default.finnish.understory = TRUE,
              resample = TRUE, sensor = "Sentinel2A",
              output.as.df = TRUE, output.intermediate = TRUE)  

# Rescale with MD
# (a) Mean difference for pooled data (see results/stats)
MD.pooled <- list(Blue = 0.011, Green = 0.023, Red = 0.009, RE1 = 0.020, RE2 = 0.089, RE3 = 0.100, 
                  NIRnarrow = 0.088, SWIR1 = 0.023, SWIR2 = 0.018)

temp.rescaled <- temp %>% mutate(Blue = Blue - MD.pooled$Blue, Green = Green - MD.pooled$Green, Red = Red - MD.pooled$Red,
                        RE1 = RE1 - MD.pooled$RE1, RE2 = RE2 - MD.pooled$RE2, RE3 = RE3 - MD.pooled$RE3, 
                        NIRnarrow = NIRnarrow - MD.pooled$NIRnarrow, SWIR1 = SWIR1 - MD.pooled$SWIR1, SWIR2 = SWIR2 - MD.pooled$SWIR2)
  
temp.rescaled %>% dplyr::select(ID, cgf.view:p, Blue:SWIR2) %>% write.csv2("results/paras_BRF_fixedInputs_S2A_extended_Nilson_rescalePooled.csv")

# (b) Mean difference by species (see results/stats)
MD.pine <- list(Blue = 0.01, Green = 0.014, Red = 0.01, RE1 = 0.012, RE2 = 0.065, RE3 = 0.081, 
                  NIRnarrow = 0.074, SWIR1 = 0.007, SWIR2 = 0.008)

MD.spruce <- list(Blue = 0.009, Green = 0.019, Red = 0.007, RE1 = 0.018, RE2 = 0.082, RE3 = 0.092, 
                NIRnarrow = 0.084, SWIR1 = 0.003, SWIR2 = 0.001)

MD.birch <- list(Blue = 0.015, Green = 0.036, Red = 0.013, RE1 = 0.033, RE2 = 0.126, RE3 = 0.136, 
                  NIRnarrow = 0.119, SWIR1 = 0.072, SWIR2 = 0.051)

MD.mixed <- list(Blue = 0.01, Green = 0.021, Red = 0.006, RE1 = 0.01, RE2 = 0.069, RE3 = 0.078, 
                 NIRnarrow = 0.059, SWIR1 = 0, SWIR2 = 0.006)


# Need species column
temp <- temp %>% mutate(Dominant_species = gaps$Dominant_species)              # This is ok cause same rows


temp.pine.rescaled <- temp %>% dplyr::filter(Dominant_species == "Pine") %>% 
  mutate(Blue = Blue - MD.pine$Blue, Green = Green - MD.pine$Green, Red = Red - MD.pine$Red,
         RE1 = RE1 - MD.pine$RE1, RE2 = RE2 - MD.pine$RE2, RE3 = RE3 - MD.pine$RE3, 
         NIRnarrow = NIRnarrow - MD.pine$NIRnarrow, SWIR1 = SWIR1 - MD.pine$SWIR1, SWIR2 = SWIR2 - MD.pine$SWIR2)

temp.spruce.rescaled <- temp %>% dplyr::filter(Dominant_species == "Spruce") %>% 
  mutate(Blue = Blue - MD.spruce$Blue, Green = Green - MD.spruce$Green, Red = Red - MD.spruce$Red,
         RE1 = RE1 - MD.spruce$RE1, RE2 = RE2 - MD.spruce$RE2, RE3 = RE3 - MD.spruce$RE3, 
         NIRnarrow = NIRnarrow - MD.spruce$NIRnarrow, SWIR1 = SWIR1 - MD.spruce$SWIR1, SWIR2 = SWIR2 - MD.spruce$SWIR2)

temp.birch.rescaled <- temp %>% dplyr::filter(Dominant_species == "Birch") %>% 
  mutate(Blue = Blue - MD.birch$Blue, Green = Green - MD.birch$Green, Red = Red - MD.birch$Red,
         RE1 = RE1 - MD.birch$RE1, RE2 = RE2 - MD.birch$RE2, RE3 = RE3 - MD.birch$RE3, 
         NIRnarrow = NIRnarrow - MD.birch$NIRnarrow, SWIR1 = SWIR1 - MD.birch$SWIR1, SWIR2 = SWIR2 - MD.birch$SWIR2)

temp.mixed.rescaled <- temp %>% dplyr::filter(Dominant_species == "Mixed") %>% 
  mutate(Blue = Blue - MD.mixed$Blue, Green = Green - MD.mixed$Green, Red = Red - MD.mixed$Red,
         RE1 = RE1 - MD.mixed$RE1, RE2 = RE2 - MD.mixed$RE2, RE3 = RE3 - MD.mixed$RE3, 
         NIRnarrow = NIRnarrow - MD.mixed$NIRnarrow, SWIR1 = SWIR1 - MD.mixed$SWIR1, SWIR2 = SWIR2 - MD.mixed$SWIR2)

# Merge
temp.rescaled.bySpecies <- bind_rows(temp.pine.rescaled, temp.spruce.rescaled, temp.birch.rescaled, temp.mixed.rescaled)
# Save
temp.rescaled.bySpecies %>%  dplyr::select(ID, cgf.view:p, Blue:SWIR2) %>% write.csv2("results/paras_BRF_fixedInputs_S2A_extended_Nilson_rescaleBySpecies.csv")




# Plot satellite BRF vs leaf albedo ---------------------------------------
# Resample to Sentinel2A
temp <- read_csv2("results/gaps_and_testNilson.csv") 

temp <- temp %>% mutate(LAIeff = PAI_estimate) %>% # Replace LAIeff with crown-level corrected LAIeff
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch,
                Site_type_lab) %>% 
  plyr::mdply(PARAS, var = "BRF", scale.Albedo = FALSE,          # var, scale albedo
              VZA = 0, 
              SZA = SZA.S2A, 
              default.finnish.species = TRUE,
              default.finnish.understory = TRUE,
              resample = TRUE, sensor = "Sentinel2A",
              output.as.df = TRUE, output.intermediate = TRUE)  
































