
# Input A: Canopy structure ----------------------------------------------------------
gaps <- read_csv2("results/gaps.csv")


# Add image SZA 
gaps <- gaps %>% mutate(Img_sza_S2 = 41.33)                              # SZA of the Sentinel-2 scene 17 Aug 2015
gaps <- gaps %>% mutate(Img_sza_L8 = 49.82)                              # SZA of the Landsat-8 scene 20 Aug 2015




# Input B: Leaf single scattering albedo  -------------------------------------------
wL.3Species.S2 <- read_csv2("data/leaf/wL_bySpecies_S2.csv")
wL.pine.S2 <- wL.3Species.S2 %>% dplyr::filter(species == "Pine") %>% dplyr::select(-c(X1, species)) 
wL.spruce.S2 <- wL.3Species.S2 %>% dplyr::filter(species == "Spruce") %>% dplyr::select(-c(X1, species)) 
wL.birch.S2 <- wL.3Species.S2 %>% dplyr::filter(species == "Birch") %>% dplyr::select(-c(X1, species))

wL.3Species.S2.ls <- list(Pine = wL.pine.S2, Spruce = wL.spruce.S2, Birch = wL.birch.S2)            # Make list object for function



# Plot-level wL weighted by species proportion (keep here maybe need to plot it, but better put in paras_BRF function!)
wL.weighted <- as_data_frame(array(NA, c(nrow(gaps), length(wL.pine.S2)+1)))  # column 1 is plot ID
colnames(wL.weighted) <- c("ID", names(wL.pine.S2))
get.wL.weighted <- function(x) wL.pine.S2 * x$prop_pine + wL.spruce.S2 * x$prop_spruce + wL.birch.S2 * x$prop_birch 
wL.weighted <- ddply(gaps, .(ID), get.wL.weighted) 

wL.weighted <- left_join(gaps, wL.weighted, by = "ID") %>% dplyr::select(ID, b2:b12)  # Re-arrange rows to be same as gaps
write.csv2(wL.weighted, "data/leaf/wL_speciesWeighted_byPlot.csv")            # Write to disk ***************



 # Input C: Understory reflectance -----------------------------------------
Rg.3Class.S2 <- read_csv2("data/understory/Rg_3Class_S2.csv") %>% dplyr::select(-X1)
Rg.HerbRich.S2 <- Rg.3Class.S2 %>% dplyr::filter(class == "HerbRich") %>% dplyr::select(-class)
Rg.Mesic.S2 <- Rg.3Class.S2 %>% dplyr::filter(class == "Mesic") %>% dplyr::select(-class)
Rg.Xeric.S2 <- Rg.3Class.S2 %>% dplyr::filter(class == "Xeric") %>% dplyr::select(-class)

Rg.3Class.S2.ls <- list(HerbRich = Rg.HerbRich.S2, Mesic = Rg.Mesic.S2, Xeric =  Rg.Xeric.S2)



# Run PARAS BRF simulation ------------------------------------------------
source("tests/paras_BRF_fun.R")                                                   ###################### source the PARAS functions

# Call function with m*ply? YES, below are unsuccesful alternative
# pmap_df(.l = gaps, .f = paras_BRF, wL.S2 = wL.3Species.S2.ls, Rg.S2 = Rg.3Class.S2.ls)
# gaps %>% rowwise() %>% do(paras_BRF(., wL.S2 = wL.3Species.S2.ls, Rg.S2 = Rg.3Class.S2.ls)) %>% bind_rows()
# gaps %>% rowwise() %>% map(paras_BRF, wL.S2 = wL.3Species.S2.ls, Rg.S2 = Rg.3Class.S2.ls)

paras.BRF.res <- gaps %>% dplyr::select(cgf.view, cgf.sun, DIFN, LAItrue,                             # Line 1:3 needs to keep only columns which are arguments to functions 
                       prop_pine, prop_spruce, prop_birch,
                       Site_type_lab) %>% 
                 plyr::mdply(paras_BRF, wL.S2 = wL.3Species.S2.ls, Rg.S2 = Rg.3Class.S2.ls) %>%  # Apply paras_BRF function to each row
                 dplyr::select(LAItrue, b2:b12) %>% left_join(gaps) %>%        # Select only the BRF result columns, and join table with gaps table, LAItrue as key  
                 dplyr::select(ID:LAIeff, LAItrue, DIFN:dominant_species, b2:b12) # Re-arrange the columns so BRFs are last


write.csv2(paras.BRF.res, "results/paras_BRF_fixedInputs.csv")
