
# Read satellite BRF ----------------------------------------------------------

S2.BOA.w12.5 <- read_csv2("data/image_extracted_spectra/S2_BOA_w12_5.csv")

# Make ID character
S2.BOA.w12.5 <- S2.BOA.w12.5 %>% mutate(ID = as.character(ID))

# Rename bands to "colour"
names(S2.BOA.w12.5) <- c("X1", "ID",
                         "Sat.Blue", "Sat.Green", "Sat.Red", "Sat.RE1", "Sat.RE2", "Sat.RE3",
                         "Sat.NIRnarrow", "Sat.SWIR1", "Sat.SWIR2")


# Read gaps ---------------------------------------------------------------  

gaps <- read_csv2("results/gaps.csv") 

# Read PARAS BRF ----------------------------------------------------------
paras.BRF.S2A.withoutNilson <- read_csv2("results/paras_BRF_fixedInputs_S2A_extended.csv")
paras.BRF.S2A.withNilson <- read_csv2("results/paras_BRF_fixedInputs_S2A_extended_Nilson.csv")
paras.BRF.S2A.withNilson.newRSR <- read_csv2("results/paras_BRF_fixedInputs_S2A_extended_Nilson_newRSR.csv")
paras.BRF.S2A.withNilson.newRSR.vza5 <- read_csv2("results/paras_BRF_fixedInputs_S2A_extended_Nilson_newRSR_vza5.csv")


# Merge gaps and satellite BRF ------------------------------------------------

gaps.sat.S2A <- left_join(gaps, S2.BOA.w12.5, by = c("ID" = "ID"))

# Merge gaps, satellite, and PARAS BRF ------------------------------------

gaps.sat.paras.S2A.withoutNilson <- left_join(gaps.sat.S2A, paras.BRF.S2A.withoutNilson, by = c("ID" = "ID"))
gaps.sat.paras.S2A.withNilson <- left_join(gaps.sat.S2A, paras.BRF.S2A.withNilson, by = c("ID" = "ID"))
gaps.sat.paras.S2A.withNilson.newRSR <- left_join(gaps.sat.S2A, paras.BRF.S2A.withNilson.newRSR, by = c("ID" = "ID"))
gaps.sat.paras.S2A.withNilson.newRSR.vza5 <- left_join(gaps.sat.S2A, paras.BRF.S2A.withNilson.newRSR.vza5, by = c("ID" = "ID"))


# Get statistics ----------------------------------------------------------
# pred, obs are vectors c(B1, B2, ...). The (pred,obs) pair are based on element index in the vector i.e. pred[i],obs[i]
# Make function
get.stats <- function(df, pred, obs) {
  # Get the pred and obs columns
  df.pred <- df[,pred]
  df.obs <- df[,obs]
  # From tibble to list
  df.pred <- as.list(df.pred)
  df.obs <- as.list(df.obs)
  # Map over the two lists.
  r <-  map2_dbl(.x = df.pred, .y = df.obs, .f = cor)
  md <- map2_dbl(.x = df.pred, .y = df.obs, .f = MD)       # Order must be .x = pred, .y = obs. Notice capital e.g. "RMSD" are function name, small case e.g. "rmsd" the object
  rmsd <- map2_dbl(.x = df.pred, .y = df.obs, .f = RMSD)        
  rmsdc <- map2_dbl(.x = df.pred, .y = df.obs, .f = RMSDC)
  
  # Relative to mean of simulated values
  md.nsim <- map2_dbl(.x = df.pred, .y = df.obs, .f = nsimMD)       # Order must be .x = pred, .y = obs. Notice capital e.g. "RMSD" are function name, small case e.g. "rmsd" the object
  rmsd.nsim <- map2_dbl(.x = df.pred, .y = df.obs, .f = nsimRMSD)        
  rmsdc.nsim <- map2_dbl(.x = df.pred, .y = df.obs, .f = nsimRMSDC)
  

  # Make data frame
  res <- tibble(Band = c("Blue", "Green", "Red", "RE1", "RE2", "RE3", "NIRnarrow", "SWIR1", "SWIR2"),
                 r = round(r, 2),
                 md = round(md, 3),
                 rmsd = round(rmsd, 3),
                 rmsdc = round(rmsdc, 3),
                # Relative values
                md.nsim = round(md.nsim, 1),
                rmsd.nsim = round(rmsd.nsim, 1),
                rmsdc.nsim = round(rmsdc.nsim, 1))
  
  return(res)
}

# Debug  # ! To comment out!!!
# df = gaps.sat.paras.S2A.withoutNilson
# pred = c("Blue", "Green", "Red", "RE1", "RE2", "RE3", "NIRnarrow", "SWIR1", "SWIR2")
# obs = c("Sat.Blue", "Sat.Green", "Sat.Red", "Sat.RE1", "Sat.RE2", "Sat.RE3", "Sat.NIRnarrow", "Sat.SWIR1", "Sat.SWIR2")


# Run the function
stats.withoutNilson <- get.stats(df = gaps.sat.paras.S2A.withoutNilson, 
          pred = c("Blue", "Green", "Red", "RE1", "RE2", "RE3", "NIRnarrow", "SWIR1", "SWIR2"),
          obs = c("Sat.Blue", "Sat.Green", "Sat.Red", "Sat.RE1", "Sat.RE2", "Sat.RE3", "Sat.NIRnarrow", "Sat.SWIR1", "Sat.SWIR2"))

stats.withNilson <- get.stats(df = gaps.sat.paras.S2A.withNilson, 
                            pred = c("Blue", "Green", "Red", "RE1", "RE2", "RE3", "NIRnarrow", "SWIR1", "SWIR2"),
                            obs = c("Sat.Blue", "Sat.Green", "Sat.Red", "Sat.RE1", "Sat.RE2", "Sat.RE3", "Sat.NIRnarrow", "Sat.SWIR1", "Sat.SWIR2"))

stats.withNilson.newRSR <- get.stats(df = gaps.sat.paras.S2A.withNilson.newRSR, 
                              pred = c("Blue", "Green", "Red", "RE1", "RE2", "RE3", "NIRnarrow", "SWIR1", "SWIR2"),
                              obs = c("Sat.Blue", "Sat.Green", "Sat.Red", "Sat.RE1", "Sat.RE2", "Sat.RE3", "Sat.NIRnarrow", "Sat.SWIR1", "Sat.SWIR2"))

stats.withNilson.newRSR.vza5 <- get.stats(df = gaps.sat.paras.S2A.withNilson.newRSR.vza5, 
                                     pred = c("Blue", "Green", "Red", "RE1", "RE2", "RE3", "NIRnarrow", "SWIR1", "SWIR2"),
                                     obs = c("Sat.Blue", "Sat.Green", "Sat.Red", "Sat.RE1", "Sat.RE2", "Sat.RE3", "Sat.NIRnarrow", "Sat.SWIR1", "Sat.SWIR2"))



write.csv2(stats.withoutNilson, "results/stats/sim_vs_sat_S2A_withoutNilson.csv")
write.csv2(stats.withNilson, "results/stats/sim_vs_sat_S2A_withNilson.csv")
write.csv2(stats.withNilson.newRSR, "results/stats/sim_vs_sat_S2A_withNilson_newRSR.csv")
write.csv2(stats.withNilson.newRSR.vza5, "results/stats/sim_vs_sat_S2A_withNilson_newRSR.vza5.csv")


# By species
# Pine
stats.withoutNilson.PINE <- get.stats(df = dplyr::filter(gaps.sat.paras.S2A.withoutNilson, Dominant_species == "Pine"), 
                                 pred = c("Blue", "Green", "Red", "RE1", "RE2", "RE3", "NIRnarrow", "SWIR1", "SWIR2"),
                                 obs = c("Sat.Blue", "Sat.Green", "Sat.Red", "Sat.RE1", "Sat.RE2", "Sat.RE3", "Sat.NIRnarrow", "Sat.SWIR1", "Sat.SWIR2"))

stats.withNilson.PINE <- get.stats(df = dplyr::filter(gaps.sat.paras.S2A.withNilson, Dominant_species == "Pine"), 
                              pred = c("Blue", "Green", "Red", "RE1", "RE2", "RE3", "NIRnarrow", "SWIR1", "SWIR2"),
                              obs = c("Sat.Blue", "Sat.Green", "Sat.Red", "Sat.RE1", "Sat.RE2", "Sat.RE3", "Sat.NIRnarrow", "Sat.SWIR1", "Sat.SWIR2"))

write.csv2(stats.withoutNilson.PINE, "results/stats/sim_vs_sat_S2A_withoutNilson_PINE.csv")
write.csv2(stats.withNilson.PINE, "results/stats/sim_vs_sat_S2A_withNilson_PINE.csv")

# Spruce
stats.withoutNilson.SPRUCE <- get.stats(df = dplyr::filter(gaps.sat.paras.S2A.withoutNilson, Dominant_species == "Spruce"), 
                                      pred = c("Blue", "Green", "Red", "RE1", "RE2", "RE3", "NIRnarrow", "SWIR1", "SWIR2"),
                                      obs = c("Sat.Blue", "Sat.Green", "Sat.Red", "Sat.RE1", "Sat.RE2", "Sat.RE3", "Sat.NIRnarrow", "Sat.SWIR1", "Sat.SWIR2"))

stats.withNilson.SPRUCE <- get.stats(df = dplyr::filter(gaps.sat.paras.S2A.withNilson, Dominant_species == "Spruce"), 
                                   pred = c("Blue", "Green", "Red", "RE1", "RE2", "RE3", "NIRnarrow", "SWIR1", "SWIR2"),
                                   obs = c("Sat.Blue", "Sat.Green", "Sat.Red", "Sat.RE1", "Sat.RE2", "Sat.RE3", "Sat.NIRnarrow", "Sat.SWIR1", "Sat.SWIR2"))

write.csv2(stats.withoutNilson.SPRUCE, "results/stats/sim_vs_sat_S2A_withoutNilson_SPRUCE.csv")
write.csv2(stats.withNilson.SPRUCE, "results/stats/sim_vs_sat_S2A_withNilson_SPRUCE.csv")


# Birch
stats.withoutNilson.BIRCH <- get.stats(df = dplyr::filter(gaps.sat.paras.S2A.withoutNilson, Dominant_species == "Birch"), 
                                        pred = c("Blue", "Green", "Red", "RE1", "RE2", "RE3", "NIRnarrow", "SWIR1", "SWIR2"),
                                        obs = c("Sat.Blue", "Sat.Green", "Sat.Red", "Sat.RE1", "Sat.RE2", "Sat.RE3", "Sat.NIRnarrow", "Sat.SWIR1", "Sat.SWIR2"))

stats.withNilson.BIRCH <- get.stats(df = dplyr::filter(gaps.sat.paras.S2A.withNilson, Dominant_species == "Birch"), 
                                     pred = c("Blue", "Green", "Red", "RE1", "RE2", "RE3", "NIRnarrow", "SWIR1", "SWIR2"),
                                     obs = c("Sat.Blue", "Sat.Green", "Sat.Red", "Sat.RE1", "Sat.RE2", "Sat.RE3", "Sat.NIRnarrow", "Sat.SWIR1", "Sat.SWIR2"))

write.csv2(stats.withoutNilson.BIRCH, "results/stats/sim_vs_sat_S2A_withoutNilson_BIRCH.csv")
write.csv2(stats.withNilson.BIRCH, "results/stats/sim_vs_sat_S2A_withNilson_BIRCH.csv")


# Mixed
stats.withoutNilson.MIXED <- get.stats(df = dplyr::filter(gaps.sat.paras.S2A.withoutNilson, Dominant_species == "Mixed"), 
                                       pred = c("Blue", "Green", "Red", "RE1", "RE2", "RE3", "NIRnarrow", "SWIR1", "SWIR2"),
                                       obs = c("Sat.Blue", "Sat.Green", "Sat.Red", "Sat.RE1", "Sat.RE2", "Sat.RE3", "Sat.NIRnarrow", "Sat.SWIR1", "Sat.SWIR2"))

stats.withNilson.MIXED <- get.stats(df = dplyr::filter(gaps.sat.paras.S2A.withNilson, Dominant_species == "Mixed"), 
                                    pred = c("Blue", "Green", "Red", "RE1", "RE2", "RE3", "NIRnarrow", "SWIR1", "SWIR2"),
                                    obs = c("Sat.Blue", "Sat.Green", "Sat.Red", "Sat.RE1", "Sat.RE2", "Sat.RE3", "Sat.NIRnarrow", "Sat.SWIR1", "Sat.SWIR2"))

write.csv2(stats.withoutNilson.MIXED, "results/stats/sim_vs_sat_S2A_withoutNilson_MIXED.csv")
write.csv2(stats.withNilson.MIXED, "results/stats/sim_vs_sat_S2A_withNilson_MIXED.csv")






