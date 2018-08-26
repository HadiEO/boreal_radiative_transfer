
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

# Read PARAS albedo ---------------------------------------------------------- *********

# paras.albedo.S2A <- read_csv2("results/paras_albedo_fixedInputs_S2A_extended.csv")
paras.albedo.S2A <- read_csv2("results/paras_Albedo_fixedInputs_S2A_extended_Nilson.csv")
paras.albedo.L8 <- read_csv2("results/paras_Albedo_fixedInputs_L8_extended_Nilson.csv") 

# Merge gaps and satellite albedo ------------------------------------------------

gaps.sat.S2A <- left_join(gaps, S2.BOA.w12.5, by = c("ID" = "ID"))
gaps.sat.L8 <- left_join(gaps, L8.BOA.w12.5, by = c("ID" = "ID"))

# Merge gaps, satellite, and PARAS albedo ------------------------------------

gaps.sat.paras.S2A <- left_join(gaps.sat.S2A, paras.albedo.S2A, by = c("ID" = "ID"))
gaps.sat.paras.L8 <- left_join(gaps.sat.L8, paras.albedo.L8, by = c("ID" = "ID"))


# Plot simulated albedo vs satellite albedo by Species -------------------------------------


# Sentinel-2A -------------------------------------------------------------

temp <- gaps.sat.paras.S2A %>%                    # S2A or L8                         
  mutate(ECC = (1 - gaps1) * 100,                             # Data prepared for plotting ***
         Site_type_lab = factor(Site_type_lab, c("Herb-rich", "Mesic", "Xeric")),
         Dominant_species = factor(Dominant_species, c("Pine", "Spruce", "Birch", "Mixed")))

simAlbedo.vs.satBRF.bySpecies.S2 <- list(
  
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
                                xaxis.lim = c(0,0.5), yaxis.lim = c(0,0.5),  xaxis.break = seq(0,0.5,by=0.1), yaxis.break = seq(0,0.5,by=0.1),
                                shape.pch = pts.pchs, shape.col = pts.cols),
  
  RE3 = my_ggplot_1to1_group_ms(temp, "RE3", "Sat.RE3", group = "Dominant_species", "Simulated RE3", "Observed RE3", 
                                xaxis.lim = c(0,0.55), yaxis.lim = c(0,0.55),  xaxis.break = seq(0,0.55,by=0.1), yaxis.break = seq(0,0.55,by=0.1),
                                shape.pch = pts.pchs, shape.col = pts.cols),
  
  NIRnarrow = my_ggplot_1to1_group_ms(temp, "NIRnarrow", "Sat.NIR", group = "Dominant_species", "Simulated NIRnarrow", "Observed NIRnarrow", 
                                      xaxis.lim = c(0,0.6), yaxis.lim = c(0,0.6),  xaxis.break = seq(0,0.6,by=0.1), yaxis.break = seq(0,0.6,by=0.1),
                                      shape.pch = pts.pchs, shape.col = pts.cols),
  
  SWIR1 = my_ggplot_1to1_group_ms(temp, "SWIR1", "Sat.SWIR1", group = "Dominant_species", "Simulated SWIR1", "Observed SWIR1", 
                                  xaxis.lim = c(0,0.3), yaxis.lim = c(0,0.3),  xaxis.break = seq(0,0.3,by=0.05), yaxis.break = seq(0,0.3,by=0.05),
                                  shape.pch = pts.pchs, shape.col = pts.cols),
  
  SWIR2 = my_ggplot_1to1_group_ms(temp, "SWIR2", "Sat.SWIR2", group = "Dominant_species", "Simulated SWIR2", "Observed SWIR2", 
                                  xaxis.lim = c(0,0.17), yaxis.lim = c(0,0.17),  xaxis.break = seq(0,0.17,by=0.05), yaxis.break = seq(0,0.17,by=0.05),
                                  shape.pch = pts.pchs, shape.col = pts.cols)
  
)


pdf("graphs/simAlbedoVSsatBRF_bySpecies_S2A_Nilson.pdf", width = 7.5, height = 7.5, pointsize = 10)
with(simAlbedo.vs.satBRF.bySpecies.S2, multiplot(Blue, Green, Red, RE1, RE2, RE3, NIRnarrow, SWIR1, SWIR2, cols = 3))
dev.off()



# Landsat-8 -------------------------------------------------------------

temp <- gaps.sat.paras.L8 %>%                    # S2A or L8                         
  mutate(ECC = (1 - gaps1) * 100,                             # Data prepared for plotting ***
         Site_type_lab = factor(Site_type_lab, c("Herb-rich", "Mesic", "Xeric")),
         Dominant_species = factor(Dominant_species, c("Pine", "Spruce", "Birch", "Mixed")))

simAlbedo.vs.satBRF.bySpecies.L8 <- list(
  
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
                                xaxis.lim = c(0,0.6), yaxis.lim = c(0,0.6),  xaxis.break = seq(0,0.6,by=0.1), yaxis.break = seq(0,0.6,by=0.1),
                                shape.pch = pts.pchs, shape.col = pts.cols),
  
  SWIR1 = my_ggplot_1to1_group_ms(temp, "SWIR1", "Sat.SWIR1", group = "Dominant_species", "Simulated SWIR1", "Observed SWIR1", 
                                  xaxis.lim = c(0,0.3), yaxis.lim = c(0,0.3),  xaxis.break = seq(0,0.3,by=0.05), yaxis.break = seq(0,0.3,by=0.05),
                                  shape.pch = pts.pchs, shape.col = pts.cols),
  
  SWIR2 = my_ggplot_1to1_group_ms(temp, "SWIR2", "Sat.SWIR2", group = "Dominant_species", "Simulated SWIR2", "Observed SWIR2", 
                                  xaxis.lim = c(0,0.17), yaxis.lim = c(0,0.17),  xaxis.break = seq(0,0.17,by=0.05), yaxis.break = seq(0,0.17,by=0.05),
                                  shape.pch = pts.pchs, shape.col = pts.cols)
  
)

pdf("graphs/simAlbedoVSsatBRF_bySpecies_L8_Nilson.pdf", width = 7.5, height = 5, pointsize = 10)
with(simAlbedo.vs.satBRF.bySpecies.L8, multiplot(Blue, Green, Red, NIR, SWIR1, SWIR2, cols = 3))
dev.off()

# RMSD sat albedo vs sim albedo ------------------------------------------
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





