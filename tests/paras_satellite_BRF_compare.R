
# Read satellite BRF ----------------------------------------------------------

S2.BOA.w12.5 <- read_csv2("data/image_extracted_spectra/S2_BOA_w12_5.csv")
L8.BOA.w12.5 <- read_csv2("data/image_extracted_spectra/L8_BOA_w12_5.csv")

# Make ID character
S2.BOA.w12.5 <- S2.BOA.w12.5 %>% mutate(ID = as.character(ID))
L8.BOA.w12.5 <- L8.BOA.w12.5 %>% mutate(ID = as.character(ID))

# Rename bands to "colour"
names(S2.BOA.w12.5) <- c("X1", "ID",
                         "Sat.Blue", "Sat.Green", "Sat.Red", "Sat.RE1", "Sat.RE2", "Sat.RE3",
                         "Sat.NIR", "Sat.SWIR1", "Sat.SWIR2")

names(L8.BOA.w12.5) <- c("X1", "ID",
                         "Sat.Blue", "Sat.Green", "Sat.Red", 
                         "Sat.NIR", "Sat.SWIR1", "Sat.SWIR2")

# Read gaps ---------------------------------------------------------------  

gaps <- read_csv2("results/gaps.csv") 

# Read PARAS BRF ----------------------------------------------------------
# Check which line to run!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Case of not knowing site type -------------------------------------------

paras.BRF.S2A.allXeric <- read_csv2("results/paras_BRF_fixedInputs_S2A_Nilson_allXeric.csv")
paras.BRF.S2A.allMesic <- read_csv2("results/paras_BRF_fixedInputs_S2A_Nilson_allMesic.csv")
paras.BRF.S2A.allHerbRich <- read_csv2("results/paras_BRF_fixedInputs_S2A_Nilson_allHerbRich.csv")
# Need to get median, min, max
# Define a function
summarizeSameColumnNames <- function(df, fun) {
  res = as.data.frame( # sapply returns a list here, so we convert it to a data.frame
    sapply(unique(names(df)), # for each unique column name
           function(col) apply(df[names(df) == col], 1, fun))) # calculate row means
  return(res)
}

# Cbind data, now there are multiple columns with same names
temp <- bind_cols(dplyr::select(paras.BRF.S2A.allXeric, Blue:SWIR2),  # bind_cols is ok cause we know the row matches
                           dplyr::select(paras.BRF.S2A.allMesic, Blue:SWIR2),             
                           dplyr::select(paras.BRF.S2A.allHerbRich, Blue:SWIR2))
# Get mean, min, max of the columns with same names
temp.mean <- summarizeSameColumnNames(df = temp, fun = mean)
temp.min <- summarizeSameColumnNames(df = temp, fun = min)
temp.max <- summarizeSameColumnNames(df = temp, fun = max)
# Rename column 
colnames(temp.mean) <- paste(colnames(temp.mean), "mean", sep = "_")
colnames(temp.min) <- paste(colnames(temp.min), "min", sep = "_")
colnames(temp.max) <- paste(colnames(temp.max), "max", sep = "_")


# Final form of PARAS BRF
paras.BRF.S2A <- dplyr::select(paras.BRF.S2A.allX eric, -(Blue:SWIR2)) %>% 
  bind_cols(temp.mean, temp.min, temp.max)                                # bind_cols is ok cause we know the row matches



# Case of not knowing leaf species/having leaf albedo ---------------------
# But at least know conifer or broadleaved
paras.BRF.S2A <- read_csv2("results/paras_BRF_Nilson_vary_wL_Otaniemi2016_S2A.csv")




# Case with rescaling modelled BRF to Sentinel-2 BRF ----------------------
# paras.BRF.S2A <- read_csv2("results/paras_BRF_fixedInputs_S2A_extended_Nilson_rescalePooled.csv")
paras.BRF.S2A <- read_csv2("results/paras_BRF_fixedInputs_S2A_extended_Nilson_rescaleBySpecies.csv")


# Final BRF for plotting ------------------------------------------------------------

paras.BRF.S2A <- read_csv2("results/paras_BRF_fixedInputs_S2A_extended_Nilson.csv")
paras.BRF.L8 <- read_csv2("results/paras_BRF_fixedInputs_L8_extended_Nilson.csv")






# Merge gaps and satellite BRF ------------------------------------------------

gaps.sat.S2A <- left_join(gaps, S2.BOA.w12.5, by = c("ID" = "ID"))
gaps.sat.L8 <- left_join(gaps, L8.BOA.w12.5, by = c("ID" = "ID"))

# Merge gaps, satellite, and PARAS BRF ------------------------------------

gaps.sat.paras.S2A <- left_join(gaps.sat.S2A, paras.BRF.S2A, by = c("ID" = "ID"))
gaps.sat.paras.L8 <- left_join(gaps.sat.L8, paras.BRF.L8, by = c("ID" = "ID"))


# Plot simulated BRF vs satellite BRF by Species -------------------------------------


# Sentinel-2A -------------------------------------------------------------

temp <- gaps.sat.paras.S2A %>%                    # S2A or L8                         
  mutate(ECC = (1 - gaps1) * 100,                             # Data prepared for plotting ***
         Site_type_lab = factor(Site_type_lab, c("Herb-rich", "Mesic", "Xeric")),
         Dominant_species = factor(Dominant_species, c("Pine", "Spruce", "Birch", "Mixed")))

simBRF.vs.satBRF.bySpecies <- list(
  
  Blue = my_ggplot_1to1_group_ms(temp, "Blue", "Sat.Blue", group = "Dominant_species", "Simulated blue", "Observed blue", 
                               xaxis.lim = c(0,0.042), yaxis.lim = c(0,0.042),  xaxis.break = seq(0,0.042,by=0.01), yaxis.break = seq(0,0.042,by=0.01),
                               shape.pch = pts.pchs, shape.col = pts.cols,
                               legend.pos = c(0.16, 0.8)),
  
  Green = my_ggplot_1to1_group_ms(temp, "Green", "Sat.Green", group = "Dominant_species", "Simulated green", "Observed green", 
                               xaxis.lim = c(0,0.09), yaxis.lim = c(0,0.09),  xaxis.break = seq(0,0.09,by=0.02), yaxis.break = seq(0,0.09,by=0.02),
                               shape.pch = pts.pchs, shape.col = pts.cols),
  
  Red = my_ggplot_1to1_group_ms(temp, "Red", "Sat.Red", group = "Dominant_species", "Simulated red", "Observed red", 
                               xaxis.lim = c(0,0.05), yaxis.lim = c(0,0.05),  xaxis.break = seq(0,0.05,by=0.01), yaxis.break = seq(0,0.05,by=0.01),
                               shape.pch = pts.pchs, shape.col = pts.cols),
  
  RE1 = my_ggplot_1to1_group_ms(temp, "RE1", "Sat.RE1", group = "Dominant_species", "Simulated RE1", "Observed RE1", 
                               xaxis.lim = c(0,0.14), yaxis.lim = c(0,0.14),  xaxis.break = seq(0,0.14,by=0.04), yaxis.break = seq(0,0.14,by=0.04),
                               shape.pch = pts.pchs, shape.col = pts.cols),
  
  RE2 = my_ggplot_1to1_group_ms(temp, "RE2", "Sat.RE2", group = "Dominant_species", "Simulated RE2", "Observed RE2", 
                               xaxis.lim = c(0,0.42), yaxis.lim = c(0,0.42),  xaxis.break = seq(0,0.42,by=0.1), yaxis.break = seq(0,0.42,by=0.1),
                               shape.pch = pts.pchs, shape.col = pts.cols),
  
  RE3 = my_ggplot_1to1_group_ms(temp, "RE3", "Sat.RE3", group = "Dominant_species", "Simulated RE3", "Observed RE3", 
                               xaxis.lim = c(0,0.5), yaxis.lim = c(0,0.5),  xaxis.break = seq(0,0.5,by=0.1), yaxis.break = seq(0,0.5,by=0.1),
                               shape.pch = pts.pchs, shape.col = pts.cols),
  
  NIR = my_ggplot_1to1_group_ms(temp, "NIRnarrow", "Sat.NIR", group = "Dominant_species", "Simulated NIRnarrow", "Observed NIRnarrow", 
                                xaxis.lim = c(0,0.5), yaxis.lim = c(0,0.5),  xaxis.break = seq(0,0.5,by=0.1), yaxis.break = seq(0,0.5,by=0.1),
                                shape.pch = pts.pchs, shape.col = pts.cols),
  
  SWIR1 = my_ggplot_1to1_group_ms(temp, "SWIR1", "Sat.SWIR1", group = "Dominant_species", "Simulated SWIR1", "Observed SWIR1", 
                                xaxis.lim = c(0,0.28), yaxis.lim = c(0,0.28),  xaxis.break = seq(0,0.28,by=0.05), yaxis.break = seq(0,0.28,by=0.05),
                                shape.pch = pts.pchs, shape.col = pts.cols),
  
  SWIR2 = my_ggplot_1to1_group_ms(temp, "SWIR2", "Sat.SWIR2", group = "Dominant_species", "Simulated SWIR2", "Observed SWIR2", 
                                xaxis.lim = c(0,0.17), yaxis.lim = c(0,0.17),  xaxis.break = seq(0,0.17,by=0.05), yaxis.break = seq(0,0.17,by=0.05),
                                shape.pch = pts.pchs, shape.col = pts.cols)
  
)


pdf("graphs/simVSsatBRF_bySpecies_S2A.pdf", width = 7.5, height = 7.5, pointsize = 10)
with(simBRF.vs.satBRF.bySpecies, multiplot(Blue, Green, Red, RE1, RE2, RE3, NIR, SWIR1, SWIR2, cols = 3))
dev.off()



# Landsat-8 -------------------------------------------------------------

temp <- gaps.sat.paras.L8 %>%                    # S2A or L8                         
  mutate(ECC = (1 - gaps1) * 100,                             # Data prepared for plotting ***
         Site_type_lab = factor(Site_type_lab, c("Herb-rich", "Mesic", "Xeric")),
         Dominant_species = factor(Dominant_species, c("Pine", "Spruce", "Birch", "Mixed")))

simBRF.vs.satBRF.bySpecies <- list(
  
  Blue = my_ggplot_1to1_group_ms(temp, "Blue", "Sat.Blue", group = "Dominant_species", "Simulated blue", "Observed blue", 
                                 xaxis.lim = c(0,0.042), yaxis.lim = c(0,0.042),  xaxis.break = seq(0,0.042,by=0.01), yaxis.break = seq(0,0.042,by=0.01),
                                 shape.pch = pts.pchs, shape.col = pts.cols,
                                 legend.pos = c(0.16, 0.8)),
  
  Green = my_ggplot_1to1_group_ms(temp, "Green", "Sat.Green", group = "Dominant_species", "Simulated green", "Observed green", 
                                  xaxis.lim = c(0,0.09), yaxis.lim = c(0,0.09),  xaxis.break = seq(0,0.09,by=0.02), yaxis.break = seq(0,0.09,by=0.02),
                                  shape.pch = pts.pchs, shape.col = pts.cols),
  
  Red = my_ggplot_1to1_group_ms(temp, "Red", "Sat.Red", group = "Dominant_species", "Simulated red", "Observed red", 
                                xaxis.lim = c(0,0.05), yaxis.lim = c(0,0.05),  xaxis.break = seq(0,0.05,by=0.01), yaxis.break = seq(0,0.05,by=0.01),
                                shape.pch = pts.pchs, shape.col = pts.cols),
  
  NIR = my_ggplot_1to1_group_ms(temp, "NIR", "Sat.NIR", group = "Dominant_species", "Simulated NIR", "Observed NIR", 
                                      xaxis.lim = c(0,0.5), yaxis.lim = c(0,0.5),  xaxis.break = seq(0,0.5,by=0.1), yaxis.break = seq(0,0.5,by=0.1),
                                      shape.pch = pts.pchs, shape.col = pts.cols),
  
  SWIR1 = my_ggplot_1to1_group_ms(temp, "SWIR1", "Sat.SWIR1", group = "Dominant_species", "Simulated SWIR1", "Observed SWIR1", 
                                  xaxis.lim = c(0,0.28), yaxis.lim = c(0,0.28),  xaxis.break = seq(0,0.28,by=0.05), yaxis.break = seq(0,0.28,by=0.05),
                                  shape.pch = pts.pchs, shape.col = pts.cols),
  
  SWIR2 = my_ggplot_1to1_group_ms(temp, "SWIR2", "Sat.SWIR2", group = "Dominant_species", "Simulated SWIR2", "Observed SWIR2", 
                                  xaxis.lim = c(0,0.17), yaxis.lim = c(0,0.17),  xaxis.break = seq(0,0.17,by=0.05), yaxis.break = seq(0,0.17,by=0.05),
                                  shape.pch = pts.pchs, shape.col = pts.cols)
  
)

pdf("graphs/simVSsatBRF_bySpecies_L8_Nilson_stemAsHovi2017.pdf", width = 7.5, height = 5, pointsize = 10)
with(simBRF.vs.satBRF.bySpecies, multiplot(Blue, Green, Red, NIR, SWIR1, SWIR2, cols = 3))
dev.off()

# RMSD sat BRF vs sim BRF ------------------------------------------
source("munge/RMSE.R")

temp <- sim.sat.BOA.25m                                          # Which extraction footprint ? BOA or TOA ?
temp <- sim.sat.BOA.60m 

RMSD.temp <- data_frame(
  b2 = with(temp, round(nRMSE(b2, B2_490), 3)),
  b3 = with(temp, round(nRMSE(b3, B3_560), 3)),
  b4 = with(temp, round(nRMSE(b4, B4_665), 3)),
  b5 = with(temp, round(nRMSE(b5, B5_705), 3)),
  b6 = with(temp, round(nRMSE(b6, B6_740), 3)),
  b7 = with(temp, round(nRMSE(b7, B7_783), 3)),
  b8a = with(temp, round(nRMSE(b8a, B8a_865), 3)),
  b11 = with(temp, round(nRMSE(b11, B11_1610), 3)),
  b12 = with(temp, round(nRMSE(b12, B12_2190), 3))
)

write.csv2(RMSD.temp, "results/RMSDsimsat_S2_BOA_25m.csv")
write.csv2(RMSD.temp, "results/RMSDsimsat_S2_BOA_60m.csv")








# Plot LAIt vs sim. and sat. BRF ------------------------------------------
# Sentinel-2
temp <- gaps.sat.paras.S2A %>%                    # S2A or L8                         
  mutate(ECC = (1 - gaps1) * 100,                             # Data prepared for plotting ***
         Site_type_lab = factor(Site_type_lab, c("Herb-rich", "Mesic", "Xeric")),
         Dominant_species = factor(Dominant_species, c("Pine", "Spruce", "Birch", "Mixed")))

tempplot <- myplot_simVSsat_LAIt_S2(temp, 
                                      forest.var = "LAItrue", 
                                      shape.col = c("dark orange", "dark blue"), 
                                      shape.pch = c(0,1,7,2), 
                                      xlab = expression(bold(LAI[true])))


pdf("graphs/LAItVSsimVSsat_S2_fixedInputs.pdf", width = 7.5, height = 7.5, pointsize = 10)
multiplot(tempplot$green, tempplot$re2, tempplot$swir1, 
          tempplot$red, tempplot$re3, tempplot$swir2, 
          tempplot$re1, tempplot$nir,
          cols = 3)
dev.off()



# Sentinel-2, by site type
temp <- gaps.sat.paras.S2A %>%                    # S2A or L8                         
  mutate(ECC = (1 - gaps1) * 100,                             # Data prepared for plotting ***
         Site_type_lab = factor(Site_type_lab, c("Herb-rich", "Mesic", "Xeric")),
         Dominant_species = factor(Dominant_species, c("Pine", "Spruce", "Birch", "Mixed")))

tempplot <- myplot_simVSsat_LAIt_S2_byFertile(temp, 
                                    forest.var = "LAItrue", 
                                    shape.col = c("dark orange", "dark blue"), 
                                    shape.pch = c(0,7,2), 
                                    xlab = expression(bold(LAI[true])))


pdf("graphs/LAItVSsimVSsat_S2_fixedInputs_byFertile.pdf", width = 7.5, height = 7.5, pointsize = 10)
multiplot(tempplot$green, tempplot$re2, tempplot$swir1, 
          tempplot$red, tempplot$re3, tempplot$swir2, 
          tempplot$re1, tempplot$nir,
          cols = 3)
dev.off()



# Landsat-8
temp <- gaps.sat.paras.L8 %>%                    # S2A or L8                         
  mutate(ECC = (1 - gaps1) * 100,                             # Data prepared for plotting ***
         Site_type_lab = factor(Site_type_lab, c("Herb-rich", "Mesic", "Xeric")),
         Dominant_species = factor(Dominant_species, c("Pine", "Spruce", "Birch", "Mixed")))

tempplot <- myplot_simVSsat_LAIt_L8(temp, 
                                    forest.var = "LAItrue", 
                                    shape.col = c("dark orange", "dark blue"), 
                                    shape.pch = c(0,1,7,2), 
                                    xlab = expression(bold(LAI[true])))


pdf("graphs/LAItVSsimVSsat_L8_fixedInputs.pdf", width = 7.5, height = 5, pointsize = 10)
multiplot(tempplot$green, tempplot$swir1, 
          tempplot$red, tempplot$swir2, 
          tempplot$nir,
          cols = 3)
dev.off()



# Sentinel-2 Nilson
temp <- gaps.sat.paras.S2A %>%                    # S2A or L8                         
  mutate(ECC = (1 - gaps1) * 100,                             # Data prepared for plotting ***
         Site_type_lab = factor(Site_type_lab, c("Herb-rich", "Mesic", "Xeric")),
         Dominant_species = factor(Dominant_species, c("Pine", "Spruce", "Birch", "Mixed")))

tempplot <- myplot_simVSsat_LAIt_S2_Nilson(temp, 
                                    forest.var = "LAItrue", 
                                    shape.col = c("dark orange", "dark blue"), 
                                    shape.pch = c(0,1,7,2), 
                                    xlab = expression(bold(LAI[true])))


pdf("graphs/LAItVSsimVSsat_S2_fixedInputs_Nilson.pdf", width = 7.5, height = 7.5, pointsize = 10)
multiplot(tempplot$green, tempplot$re2, tempplot$swir1, 
          tempplot$red, tempplot$re3, tempplot$swir2, 
          tempplot$re1, tempplot$nir,
          cols = 3)
dev.off()


# Landsat-8 Nilson
temp <- gaps.sat.paras.L8 %>%                    # S2A or L8                         
  mutate(ECC = (1 - gaps1) * 100,                             # Data prepared for plotting ***
         Site_type_lab = factor(Site_type_lab, c("Herb-rich", "Mesic", "Xeric")),
         Dominant_species = factor(Dominant_species, c("Pine", "Spruce", "Birch", "Mixed")))

tempplot <- myplot_simVSsat_LAIt_L8_Nilson(temp, 
                                    forest.var = "LAItrue", 
                                    shape.col = c("dark orange", "dark blue"), 
                                    shape.pch = c(0,1,7,2), 
                                    xlab = expression(bold(LAI[true])))


pdf("graphs/LAItVSsimVSsat_L8_fixedInputs_Nilson.pdf", width = 7.5, height = 5, pointsize = 10)
multiplot(tempplot$green, tempplot$swir1, 
          tempplot$red, tempplot$swir2, 
          tempplot$nir,
          cols = 3)
dev.off()



# Landsat-8 Nilson, only sun-exposed leaves
temp <- gaps.sat.paras.L8 %>%                    # S2A or L8                         
  mutate(ECC = (1 - gaps1) * 100,                             # Data prepared for plotting ***
         Site_type_lab = factor(Site_type_lab, c("Herb-rich", "Mesic", "Xeric")),
         Dominant_species = factor(Dominant_species, c("Pine", "Spruce", "Birch", "Mixed")))

tempplot <- myplot_simVSsat_LAIt_L8_Nilson(temp, 
                                           forest.var = "LAItrue", 
                                           shape.col = c("dark orange", "dark blue"), 
                                           shape.pch = c(0,1,7,2), 
                                           xlab = expression(bold(LAI[true])))


pdf("graphs/LAItVSsimVSsat_L8_fixedInputs_Nilson_onlySunExposedLeaves.pdf", width = 7.5, height = 5, pointsize = 10)
multiplot(tempplot$green, tempplot$swir1, 
          tempplot$red, tempplot$swir2, 
          tempplot$nir,
          cols = 3)
dev.off()





# Case of not knowing site type -------------------------------------------

temp <- gaps.sat.paras.S2A %>%                    # S2A or L8                         
  mutate(ECC = (1 - gaps1) * 100,                             # Data prepared for plotting ***
         Site_type_lab = factor(Site_type_lab, c("Herb-rich", "Mesic", "Xeric")),
         Dominant_species = factor(Dominant_species, c("Pine", "Spruce", "Birch", "Mixed")))

tempplot <- myplot_simVSsat_LAIt_S2_Nilson_errbar(temp, 
                                           forest.var = "LAItrue", 
                                           shape.col = c("dark orange", "dark blue"), 
                                           shape.pch = c(0,1,7,2), 
                                           xlab = expression(bold(LAI[true])),
                                           pts.cex = 1.5)


pdf("graphs/LAItVSsimVSsat_S2A_fixedInputs_Nilson_errbar_3siteType_MinMeanMax.pdf", width = 7.5, height = 7.5, pointsize = 10)
multiplot(tempplot$green, tempplot$re2, tempplot$swir1, 
          tempplot$red, tempplot$re3, tempplot$swir2, 
          tempplot$re1, tempplot$nir,
          cols = 3)
dev.off()


# Case of not knowing species -------------------------------------------

temp <- gaps.sat.paras.S2A %>%                    # S2A or L8     
  
  na.omit() %>%                             # Remove rows with missing values i.e. mixed-species plots
  
  mutate(ECC = (1 - gaps1) * 100,                             # Data prepared for plotting ***
         Site_type_lab = factor(Site_type_lab, c("Herb-rich", "Mesic", "Xeric")),
         Dominant_species = factor(Dominant_species, c("Pine", "Spruce", "Birch", "Mixed")))


# Axis range max + 0.05 (max - min)
max(temp$SWIR2_max) + 0.05 * (max(temp$SWIR2_max) - min(temp$SWIR2_min))   # Manual check, need to automate in function to plot graph


# Plot
tempplot <- myplot_simVSsat_LAIt_S2_Nilson_errbar(temp, 
                                                  forest.var = "LAItrue_mean", 
                                                  shape.col = c("dark orange", "dark blue"), 
                                                  shape.pch = c(0,1,7,2), 
                                                  xlab = expression(bold(LAI[true])),
                                                  pts.cex = 1.5,
            xaxis.lims = list(green=c(0,8), red=c(0,8), re1=c(0,8), re2=c(0,8), re3=c(0,8), nir=c(0,8), swir1=c(0,8), swir2=c(0,8)),
            yaxis.lims = list(green=c(0,0.068), red=c(0,0.040), re1 = c(0,0.103), re2=c(0,0.341), re3=c(0,0.396), nir=c(0,0.414), swir1=c(0,0.304), swir2=c(0,0.2)),
            xaxis.breaks = list(green=seq(0,8,1), red=seq(0,8,1), re1=seq(0,8,1), re2=seq(0,8,1), re3=seq(0,8,1), nir=seq(0,8,1), swir1=seq(0,8,1), swir2=seq(0,8,1)), 
            yaxis.breaks = list(green=seq(0,0.068,0.02), red=seq(0,0.040,0.01), re1 = seq(0,0.103,0.02), re2=seq(0,0.341,0.1), re3=seq(0,0.396,0.1), nir=seq(0,0.414,0.10), swir1=seq(0,0.304,0.1), swir2=seq(0,0.2,0.04))
)


pdf("graphs/LAItVSsimVSsat_S2A_fixedInputs_Nilson_errbar_Otaniemi2016wL_MinMeanMax.pdf", width = 7.5, height = 7.5, pointsize = 10)
multiplot(tempplot$green, tempplot$red, tempplot$re1,
          tempplot$re2, tempplot$re3, tempplot$nir, 
          tempplot$swir1, 
          cols = 3)
dev.off()



# Case with modelled BRF rescaled to Sentinel-2 ---------------------------

# Sentinel-2 Nilson
temp <- gaps.sat.paras.S2A %>%                    # S2A or L8                         
  mutate(ECC = (1 - gaps1) * 100,                             # Data prepared for plotting ***
         Site_type_lab = factor(Site_type_lab, c("Herb-rich", "Mesic", "Xeric")),
         Dominant_species = factor(Dominant_species, c("Pine", "Spruce", "Birch", "Mixed")))

tempplot <- myplot_simVSsat_LAIt_S2_Nilson(temp, 
                                           forest.var = "LAItrue", 
                                           shape.col = c("dark orange", "dark blue"), 
                                           shape.pch = c(0,1,7,2), 
                                           xlab = expression(bold(LAI[true])))


# pdf("graphs/LAItVSsimVSsat_S2_fixedInputs_Nilson_rescalePooled.pdf", width = 7.5, height = 7.5, pointsize = 10)         
pdf("graphs/LAItVSsimVSsat_S2_fixedInputs_Nilson_rescaleBySpecies.pdf", width = 7.5, height = 7.5, pointsize = 10)    
multiplot(tempplot$green, tempplot$re2, tempplot$swir1, 
          tempplot$red, tempplot$re3, tempplot$swir2, 
          tempplot$re1, tempplot$nir,
          cols = 3)
dev.off()

# Rescale with MD for pooled data cause slight underestimation of PARAS BRF in green, RE1, and 
# moderate underestimation in SWIR1, SWIR2. In SWIR birch are overestimated
# Rescale with MD by species reduces SWIR overestimation, but somewhat slightly underestimate now.
# So, rescale with MD by species is better.







