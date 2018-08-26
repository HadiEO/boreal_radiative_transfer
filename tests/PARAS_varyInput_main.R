
# Input A: Canopy structure ----------------------------------------------------------
gaps <- read_csv2("results/gaps.csv")
gaps.Nilson <- read_csv2("results/gaps_and_testNilson.csv") 


# Add VZA and SZA  --------------------------------------------------------
# If differ between plots, add as columns to gaps data frame 
# In this case, all plots have same angles, so can add as external input for mdply call below
SZA.S2A <- 41.33                # SZA of the Sentinel-2 scene 17 Aug 2015
SZA.L8 <- 49.82                # SZA of the Landsat-8 scene 20 Aug 2015



# Source the PARAS function -----------------------------------------------

source("tests/PARAS_varyInput.R")


# Case not having leaf measurements of local tree species ----------------------
# Run simulation with each species leaf albedo, separately for conifer and broadleaved. Exclude mixed plots (n = 3)
# Then get the mean, min, max of the simulations.

# Read the leaf albedo speclib
otaniemi2016.conifer.speclib <- read_rds("data/leaf/from_Aarne/otaniemi2016_conifer.rds")
otaniemi2016.broadleaf.speclib <- read_rds("data/leaf/from_Aarne/otaniemi2016_broadleaf.rds")

# Select coniferous plots, run with coniferous leaf albedo speclib
# use hsdar::apply(X = speclib, FUN) to get mean, min, max of spectra in the speclib
# Resample to Sentinel2A
parasRes.vary.wL.conifer <- gaps.Nilson %>%              # With or without crown-level clumping correction ***********************
  
  dplyr::filter(Dominant_species %in% c("Pine", "Spruce")) %>%    # Select coniferous or broadleaved plots
  
  mutate(LAIeff = PAI_estimate) %>%  # Replace LAIeff with crown-level corrected LAIeff
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch,
                Site_type_lab) %>% 

  plyr::mdply(PARAS.varyInput, var = "BRF",          # var
              wL = otaniemi2016.conifer.speclib,        # Which wL speclib **************************************          
              VZA = 0, 
              SZA = SZA.S2A, 
              default.finnish.species = TRUE,
              default.finnish.understory = TRUE,
              resample = TRUE, sensor = "Sentinel2A",
              output.as.df = TRUE, output.intermediate = TRUE) 


# Select broadleaved plots, run with coniferous leaf albedo speclib
# use hsdar::apply(X = speclib, FUN) to get mean, min, max of spectra in the speclib
# Resample to Sentinel2A

parasRes.vary.wL.broadleaf <- gaps.Nilson %>%              # With or without crown-level clumping correction ***********************
 
  dplyr::filter(Dominant_species == "Birch") %>%    # Select coniferous or broadleaved plots
  
  mutate(LAIeff = PAI_estimate) %>%  # Replace LAIeff with crown-level corrected LAIeff
  dplyr::select(ID, gaps1, gaps2, gaps3, gaps4, gaps5, LAIeff,
                Prop_pine, Prop_spruce, Prop_birch,
                Site_type_lab) %>% 
  
  plyr::mdply(PARAS.varyInput, var = "BRF",          # var
              wL = otaniemi2016.broadleaf.speclib,        # Which wL speclib **************************************          
              VZA = 0, 
              SZA = SZA.S2A, 
              default.finnish.species = TRUE,
              default.finnish.understory = TRUE,
              resample = TRUE, sensor = "Sentinel2A",
              output.as.df = TRUE, output.intermediate = TRUE) 


# Merge
parasRes.vary.wL.both <- bind_rows(parasRes.vary.wL.conifer, parasRes.vary.wL.broadleaf)
# plot(parasRes.vary.wL.both$LAItrue, parasRes.vary.wL.both$SWIR2)

by.ID <- parasRes.vary.wL.both %>%  group_by(ID)

by.ID.summarized <- by.ID %>% summarize_at(vars(ID, LAItrue, Blue:SWIR1), funs(mean, min, max))
plot(by.ID.summarized$LAItrue_mean, by.ID.summarized$SWIR1_mean, ylim = c(0, 0.4))
points(by.ID.summarized$LAItrue_mean, by.ID.summarized$SWIR1_min, pch = 19)
points(by.ID.summarized$LAItrue_mean, by.ID.summarized$SWIR1_max, pch = 7)

# Write to disk
write.csv2(by.ID.summarized, "results/paras_BRF_Nilson_vary_wL_Otaniemi2016_S2A.csv")

  



