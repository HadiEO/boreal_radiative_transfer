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

paras.BRF.S2A <- read_csv2("results/parasArbitrary3_BRF_fixedInputs_S2A_Nilson.csv") 
paras.BRF.L8 <- read_csv2("results/parasArbitrary3_BRF_fixedInputs_L8_Nilson.csv") 

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

simBRF.vs.satBRF.bySpecies.S2A <- list(
  # 
  # Blue = my_ggplot_1to1_group_ms(temp, "Blue", "Sat.Blue", group = "Dominant_species", "Simulated blue", "Observed blue", 
  #                                xaxis.lim = c(0,0.042), yaxis.lim = c(0,0.042),  xaxis.break = seq(0,0.042,by=0.01), yaxis.break = seq(0,0.042,by=0.01),
  #                                shape.pch = pts.pchs, shape.col = pts.cols,
  #                                legend.pos = c(0.16, 0.8)),
  # 
  # Green = my_ggplot_1to1_group_ms(temp, "Green", "Sat.Green", group = "Dominant_species", "Simulated green", "Observed green", 
  #                                 xaxis.lim = c(0,0.09), yaxis.lim = c(0,0.09),  xaxis.break = seq(0,0.09,by=0.02), yaxis.break = seq(0,0.09,by=0.02),
  #                                 shape.pch = pts.pchs, shape.col = pts.cols),
  
  Red = my_ggplot_1to1_group_ms(temp, "red", "Sat.Red", group = "Dominant_species", "Simulated red", "Observed red", 
                                xaxis.lim = c(0,0.05), yaxis.lim = c(0,0.05),  xaxis.break = seq(0,0.05,by=0.01), yaxis.break = seq(0,0.045,by=0.01),
                                shape.pch = pts.pchs, shape.col = pts.cols),
  
  # RE1 = my_ggplot_1to1_group_ms(temp, "RE1", "Sat.RE1", group = "Dominant_species", "Simulated RE1", "Observed RE1", 
  #                               xaxis.lim = c(0,0.14), yaxis.lim = c(0,0.14),  xaxis.break = seq(0,0.14,by=0.04), yaxis.break = seq(0,0.14,by=0.04),
  #                               shape.pch = pts.pchs, shape.col = pts.cols),
  # 
  # RE2 = my_ggplot_1to1_group_ms(temp, "RE2", "Sat.RE2", group = "Dominant_species", "Simulated RE2", "Observed RE2", 
  #                               xaxis.lim = c(0,0.42), yaxis.lim = c(0,0.42),  xaxis.break = seq(0,0.42,by=0.1), yaxis.break = seq(0,0.42,by=0.1),
  #                               shape.pch = pts.pchs, shape.col = pts.cols),
  # 
  # RE3 = my_ggplot_1to1_group_ms(temp, "RE3", "Sat.RE3", group = "Dominant_species", "Simulated RE3", "Observed RE3", 
  #                               xaxis.lim = c(0,0.5), yaxis.lim = c(0,0.5),  xaxis.break = seq(0,0.5,by=0.1), yaxis.break = seq(0,0.5,by=0.1),
  #                               shape.pch = pts.pchs, shape.col = pts.cols),
  
  NIR = my_ggplot_1to1_group_ms(temp, "NIR", "Sat.NIR", group = "Dominant_species", "Simulated NIR", "Observed NIR", 
                                xaxis.lim = c(0,0.5), yaxis.lim = c(0,0.5),  xaxis.break = seq(0,0.5,by=0.1), yaxis.break = seq(0,0.5,by=0.1),
                                shape.pch = pts.pchs, shape.col = pts.cols)
  
  # SWIR1 = my_ggplot_1to1_group_ms(temp, "SWIR1", "Sat.SWIR1", group = "Dominant_species", "Simulated SWIR1", "Observed SWIR1", 
  #                                 xaxis.lim = c(0,0.28), yaxis.lim = c(0,0.28),  xaxis.break = seq(0,0.28,by=0.05), yaxis.break = seq(0,0.28,by=0.05),
  #                                 shape.pch = pts.pchs, shape.col = pts.cols),
  # 
  # SWIR2 = my_ggplot_1to1_group_ms(temp, "SWIR2", "Sat.SWIR2", group = "Dominant_species", "Simulated SWIR2", "Observed SWIR2", 
  #                                 xaxis.lim = c(0,0.17), yaxis.lim = c(0,0.17),  xaxis.break = seq(0,0.17,by=0.05), yaxis.break = seq(0,0.17,by=0.05),
  #                                 shape.pch = pts.pchs, shape.col = pts.cols)
  
)


pdf("graphs/arbitrary3_simVSsatBRF_bySpecies_S2A_Nilson.pdf", width = 7.5, height = 5, pointsize = 10)
with(simBRF.vs.satBRF.bySpecies.S2A, multiplot(Red, NIR, cols = 2))
dev.off()


# Landsat-8 -------------------------------------------------------------

temp <- gaps.sat.paras.L8 %>%                    # S2A or L8                         
  mutate(ECC = (1 - gaps1) * 100,                             # Data prepared for plotting ***
         Site_type_lab = factor(Site_type_lab, c("Herb-rich", "Mesic", "Xeric")),
         Dominant_species = factor(Dominant_species, c("Pine", "Spruce", "Birch", "Mixed")))

simBRF.vs.satBRF.bySpecies.L8 <- list(
  # 
  # Blue = my_ggplot_1to1_group_ms(temp, "Blue", "Sat.Blue", group = "Dominant_species", "Simulated blue", "Observed blue", 
  #                                xaxis.lim = c(0,0.042), yaxis.lim = c(0,0.042),  xaxis.break = seq(0,0.042,by=0.01), yaxis.break = seq(0,0.042,by=0.01),
  #                                shape.pch = pts.pchs, shape.col = pts.cols,
  #                                legend.pos = c(0.16, 0.8)),
  # 
  # Green = my_ggplot_1to1_group_ms(temp, "Green", "Sat.Green", group = "Dominant_species", "Simulated green", "Observed green", 
  #                                 xaxis.lim = c(0,0.09), yaxis.lim = c(0,0.09),  xaxis.break = seq(0,0.09,by=0.02), yaxis.break = seq(0,0.09,by=0.02),
  #                                 shape.pch = pts.pchs, shape.col = pts.cols),
  
  Red = my_ggplot_1to1_group_ms(temp, "red", "Sat.Red", group = "Dominant_species", "Simulated red", "Observed red", 
                                xaxis.lim = c(0,0.05), yaxis.lim = c(0,0.05),  xaxis.break = seq(0,0.05,by=0.01), yaxis.break = seq(0,0.045,by=0.01),
                                shape.pch = pts.pchs, shape.col = pts.cols),
  
  # RE1 = my_ggplot_1to1_group_ms(temp, "RE1", "Sat.RE1", group = "Dominant_species", "Simulated RE1", "Observed RE1", 
  #                               xaxis.lim = c(0,0.14), yaxis.lim = c(0,0.14),  xaxis.break = seq(0,0.14,by=0.04), yaxis.break = seq(0,0.14,by=0.04),
  #                               shape.pch = pts.pchs, shape.col = pts.cols),
  # 
  # RE2 = my_ggplot_1to1_group_ms(temp, "RE2", "Sat.RE2", group = "Dominant_species", "Simulated RE2", "Observed RE2", 
  #                               xaxis.lim = c(0,0.42), yaxis.lim = c(0,0.42),  xaxis.break = seq(0,0.42,by=0.1), yaxis.break = seq(0,0.42,by=0.1),
  #                               shape.pch = pts.pchs, shape.col = pts.cols),
  # 
  # RE3 = my_ggplot_1to1_group_ms(temp, "RE3", "Sat.RE3", group = "Dominant_species", "Simulated RE3", "Observed RE3", 
  #                               xaxis.lim = c(0,0.5), yaxis.lim = c(0,0.5),  xaxis.break = seq(0,0.5,by=0.1), yaxis.break = seq(0,0.5,by=0.1),
  #                               shape.pch = pts.pchs, shape.col = pts.cols),
  
  NIR = my_ggplot_1to1_group_ms(temp, "NIR", "Sat.NIR", group = "Dominant_species", "Simulated NIR", "Observed NIR", 
                                xaxis.lim = c(0,0.5), yaxis.lim = c(0,0.5),  xaxis.break = seq(0,0.5,by=0.1), yaxis.break = seq(0,0.5,by=0.1),
                                shape.pch = pts.pchs, shape.col = pts.cols)
  
  # SWIR1 = my_ggplot_1to1_group_ms(temp, "SWIR1", "Sat.SWIR1", group = "Dominant_species", "Simulated SWIR1", "Observed SWIR1", 
  #                                 xaxis.lim = c(0,0.28), yaxis.lim = c(0,0.28),  xaxis.break = seq(0,0.28,by=0.05), yaxis.break = seq(0,0.28,by=0.05),
  #                                 shape.pch = pts.pchs, shape.col = pts.cols),
  # 
  # SWIR2 = my_ggplot_1to1_group_ms(temp, "SWIR2", "Sat.SWIR2", group = "Dominant_species", "Simulated SWIR2", "Observed SWIR2", 
  #                                 xaxis.lim = c(0,0.17), yaxis.lim = c(0,0.17),  xaxis.break = seq(0,0.17,by=0.05), yaxis.break = seq(0,0.17,by=0.05),
  #                                 shape.pch = pts.pchs, shape.col = pts.cols)
  
)


pdf("graphs/arbitrary3_simVSsatBRF_bySpecies_L8_Nilson.pdf", width = 7.5, height = 5, pointsize = 10)
with(simBRF.vs.satBRF.bySpecies.L8, multiplot(Red, NIR, cols = 2))
dev.off()