
# Read satellite BRF ----------------------------------------------------------

S2.BOA.w12.5 <- read_csv2("data/image_extracted_spectra/S2_BOA_w12_5.csv")
L8.BOA.w12.5 <- read_csv2("data/image_extracted_spectra/L8_BOA_w12_5.csv")

# Make ID character
S2.BOA.w12.5 <- S2.BOA.w12.5 %>% mutate(ID = as.character(ID))
L8.BOA.w12.5 <- L8.BOA.w12.5 %>% mutate(ID = as.character(ID))

# Rename bands to "colour"
names(S2.BOA.w12.5) <- c("X1", "ID",
                         "Sat.Blue", "Sat.Green", "Sat.Red", "Sat.RE1", "Sat.RE2", "Sat.RE3",
                         "Sat.NIRnarrow", "Sat.SWIR1", "Sat.SWIR2")

names(L8.BOA.w12.5) <- c("X1", "ID",
                         "Sat.Blue", "Sat.Green", "Sat.Red", 
                         "Sat.NIR", "Sat.SWIR1", "Sat.SWIR2")

# Read gaps ---------------------------------------------------------------  

gaps <- read_csv2("results/gaps.csv") 

# Read PARAS BRF ----------------------------------------------------------

paras.BRF.S2A <- read_csv2("results/paras_BRF_fixedInputs_S2A_extended_Nilson.csv") 
paras.BRF.L8 <- read_csv2("results/paras_BRF_fixedInputs_L8_extended_Nilson.csv") 

# Merge gaps and satellite BRF ------------------------------------------------

gaps.sat.S2A <- left_join(gaps, S2.BOA.w12.5, by = c("ID" = "ID"))
gaps.sat.L8 <- left_join(gaps, L8.BOA.w12.5, by = c("ID" = "ID"))

# Merge gaps, satellite, and PARAS BRF ------------------------------------

gaps.sat.paras.S2A <- left_join(gaps.sat.S2A, paras.BRF.S2A, by = c("ID" = "ID"))
gaps.sat.paras.L8 <- left_join(gaps.sat.L8, paras.BRF.L8, by = c("ID" = "ID"))


# Correlations ------------------------------------------------------------

gaps.sat.paras.S2A %>% dplyr::select(Blue:SWIR2) %>% 
  map_df(function(x) round(cor(x, gaps.sat.paras.S2A$ECC), 3)) %>% write.csv2("results/corr_satBRFvsECC_S2A.csv")
gaps.sat.paras.S2A %>% dplyr::select(Blue:SWIR2) %>% 
  map_df(function(x) round(cor(x, gaps.sat.paras.S2A$LAIeff), 3)) %>% write.csv2("results/corr_satBRFvsLAIeff_S2A.csv")
gaps.sat.paras.S2A %>% dplyr::select(Blue:SWIR2) %>% 
  map_df(function(x) round(cor(x, gaps.sat.paras.S2A$LAItrue), 3)) %>% write.csv2("results/corr_satBRFvsLAItrue_S2A.csv")

gaps.sat.paras.L8 %>% dplyr::select(Blue:SWIR2) %>% 
  map_df(function(x) round(cor(x, gaps.sat.paras.L8$ECC), 3)) %>% write.csv2("results/corr_satBRFvsECC_L8.csv")
gaps.sat.paras.L8 %>% dplyr::select(Blue:SWIR2) %>% 
  map_df(function(x) round(cor(x, gaps.sat.paras.L8$LAIeff), 3)) %>% write.csv2("results/corr_satBRFvsLAIeff_L8.csv")
gaps.sat.paras.L8 %>% dplyr::select(Blue:SWIR2) %>% 
  map_df(function(x) round(cor(x, gaps.sat.paras.L8$LAItrue), 3)) %>% write.csv2("results/corr_satBRFvsLAItrue_L8.csv")


# With corrr package
library(corrr)
gaps.sat.paras.S2A %>% dplyr::select(ECC, LAIeff, LAItrue, i0.sun, Sat.Blue:Sat.SWIR2) %>% 
  correlate() %>% 
  focus(Sat.Blue:Sat.SWIR2)







# Plot #######################################
temp <- gaps.sat.paras.L8 %>%                    # S2A or L8                         
  mutate(ECC = (1 - gaps1) * 100,                             # Data prepared for plotting ***
         Site_type_lab = factor(Site_type_lab, c("Herb-rich", "Mesic", "Xeric")),
         Dominant_species = factor(Dominant_species, c("Pine", "Spruce", "Birch", "Mixed")))


# satBRFvsLAItrue by species OR site type ---------------------------------------------------------


satBRFvsLAItrue <- list(                                # LAItrue ********************************************
  
  LAItrue.Blue = my_ggplot_ms(temp, "LAItrue", "Sat.Blue", "Dominant_species",
                            expression('LAI'[true]), "Observed BRF blue", xaxis.lim = c(0,6), yaxis.lim = c(0,0.045), 
                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.05,by=0.01), 
                            shape.col = pts.cols, shape.pch = pts.pchs, 
                            legend.pos = c(0.19, 0.25)),  # legend.pos = c(0.16, 0.25) for by Species),
  
  LAItrue.Green = my_ggplot_ms(temp, "LAItrue", "Sat.Green", "Dominant_species",
                            expression('LAI'[true]), "Observed BRF green", xaxis.lim = c(0,6), yaxis.lim = c(0,0.1), 
                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.1,by=0.02), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.Red = my_ggplot_ms(temp, "LAItrue", "Sat.Red", "Dominant_species",
                            expression('LAI'[true]), "Observed BRF red", xaxis.lim = c(0,6), yaxis.lim = c(0,0.05), 
                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.05,by=0.01), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  # LAItrue.RE1 = my_ggplot_ms(temp, "LAItrue", "Sat.RE1", "Dominant_species",
  #                           expression('LAI'[true]), "Observed BRF RE1", xaxis.lim = c(0,6), yaxis.lim = c(0,0.14), 
  #                           xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.14,by=0.02), 
  #                           shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  # 
  # LAItrue.RE2 = my_ggplot_ms(temp, "LAItrue", "Sat.RE2", "Dominant_species",
  #                           expression('LAI'[true]), "Observed BRF RE2", xaxis.lim = c(0,6), yaxis.lim = c(0,0.5), 
  #                           xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.5,by=0.05), 
  #                           shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  # 
  # LAItrue.RE3 = my_ggplot_ms(temp, "LAItrue", "Sat.RE3", "Dominant_species",
  #                           expression('LAI'[true]), "Observed BRF RE3", xaxis.lim = c(0,6), yaxis.lim = c(0,0.5), 
  #                           xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.5,by=0.05), 
  #                           shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  # 
  # LAItrue.NIRnarrow = my_ggplot_ms(temp, "LAItrue", "Sat.NIRnarrow", "Dominant_species",
  #                            expression('LAI'[true]), "Observed BRF NIRn", xaxis.lim = c(0,6), yaxis.lim = c(0,0.5), 
  #                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.5,by=0.05), 
  #                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  # Landsat NIR
  LAItrue.NIR = my_ggplot_ms(temp, "LAItrue", "Sat.NIR", "Dominant_species",
                             expression('LAI'[true]), "Observed BRF NIRn", xaxis.lim = c(0,6), yaxis.lim = c(0,0.5),
                             xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.5,by=0.05),
                             shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.SWIR1 = my_ggplot_ms(temp, "LAItrue", "Sat.SWIR1", "Dominant_species",
                             expression('LAI'[true]), "Observed BRF SWIR1", xaxis.lim = c(0,6), yaxis.lim = c(0,0.28), 
                             xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.28,by=0.05), 
                             shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.SWIR2 = my_ggplot_ms(temp, "LAItrue", "Sat.SWIR2", "Dominant_species",
                             expression('LAI'[true]), "Observed BRF SWIR2", xaxis.lim = c(0,6), yaxis.lim = c(0,0.17), 
                             xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.17,by=0.02), 
                             shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none")
  
)

# pdf("graphs/satBRF_vs_LAIt_bySiteType_S2A.pdf", width = 7.5, height = 7.5, pointsize = 10)
# with(satBRFvsLAItrue, multiplot(LAItrue.Blue, LAItrue.Green, LAItrue.Red, LAItrue.RE1, 
#                                 LAItrue.RE2, LAItrue.RE3, LAItrue.NIRnarrow, LAItrue.SWIR1, LAItrue.SWIR2, cols = 3))
# dev.off()

pdf("graphs/satBRF_vs_LAIt_bySpecies_L8.pdf", width = 7.5, height = 5, pointsize = 10)
with(satBRFvsLAItrue, multiplot(LAItrue.Blue, LAItrue.Green, LAItrue.Red, 
                                LAItrue.NIR, LAItrue.SWIR1, LAItrue.SWIR2, cols = 3))
dev.off()

# satBRFvsECC by species OR site type ---------------------------------------------------------

satBRFvsECC <- list(                                # ECC ********************************************
  
  ECC.Blue = my_ggplot_ms(temp, "ECC", "Sat.Blue", "Dominant_species",
                            "ECC (%)", "Observed BRF blue", xaxis.lim = c(0,100), yaxis.lim = c(0,0.045), 
                            xaxis.break = seq(0,100,by=20), yaxis.break = seq(0,0.05,by=0.01), 
                            shape.col = pts.cols, shape.pch = pts.pchs, 
                            legend.pos = c(0.19, 0.25)),  # legend.pos = c(0.16, 0.25) for by Species),
  
  ECC.Green = my_ggplot_ms(temp, "ECC", "Sat.Green", "Dominant_species",
                        "ECC (%)", "Observed BRF green", xaxis.lim = c(0,100), yaxis.lim = c(0,0.1), 
                            xaxis.break = seq(0,100,by=20), yaxis.break = seq(0,0.1,by=0.02), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  ECC.Red = my_ggplot_ms(temp, "ECC", "Sat.Red", "Dominant_species",
                        "ECC (%)", "Observed BRF red", xaxis.lim = c(0,100), yaxis.lim = c(0,0.05), 
                            xaxis.break = seq(0,100,by=20), yaxis.break = seq(0,0.05,by=0.01), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  # ECC.RE1 = my_ggplot_ms(temp, "ECC", "Sat.RE1", "Dominant_species",
  #                       "ECC (%)", "Observed BRF RE1", xaxis.lim = c(0,100), yaxis.lim = c(0,0.14), 
  #                           xaxis.break = seq(0,100,by=20), yaxis.break = seq(0,0.14,by=0.02), 
  #                           shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  # 
  # ECC.RE2 = my_ggplot_ms(temp, "ECC", "Sat.RE2", "Dominant_species",
  #                       "ECC (%)", "Observed BRF RE2", xaxis.lim = c(0,100), yaxis.lim = c(0,0.5), 
  #                           xaxis.break = seq(0,100,by=20), yaxis.break = seq(0,0.5,by=0.05), 
  #                           shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  # 
  # ECC.RE3 = my_ggplot_ms(temp, "ECC", "Sat.RE3", "Dominant_species",
  #                       "ECC (%)", "Observed BRF RE3", xaxis.lim = c(0,100), yaxis.lim = c(0,0.5), 
  #                           xaxis.break = seq(0,100,by=20), yaxis.break = seq(0,0.5,by=0.05), 
  #                           shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  # ECC.NIRnarrow = my_ggplot_ms(temp, "ECC", "Sat.NIRnarrow", "Dominant_species",
  #                        "ECC (%)", "Observed BRF NIRn", xaxis.lim = c(0,100), yaxis.lim = c(0,0.5), 
  #                            xaxis.break = seq(0,100,by=20), yaxis.break = seq(0,0.5,by=0.05), 
  #                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  # Landsat NIR
  ECC.NIR= my_ggplot_ms(temp, "ECC", "Sat.NIR", "Dominant_species",
                               "ECC (%)", "Observed BRF NIRn", xaxis.lim = c(0,100), yaxis.lim = c(0,0.5), 
                               xaxis.break = seq(0,100,by=20), yaxis.break = seq(0,0.5,by=0.05), 
                               shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  
  ECC.SWIR1 = my_ggplot_ms(temp, "ECC", "Sat.SWIR1", "Dominant_species",
                         "ECC (%)", "Observed BRF SWIR1", xaxis.lim = c(0,100), yaxis.lim = c(0,0.28), 
                             xaxis.break = seq(0,100,by=20), yaxis.break = seq(0,0.28,by=0.05), 
                             shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  ECC.SWIR2 = my_ggplot_ms(temp, "ECC", "Sat.SWIR2", "Dominant_species",
                         "ECC (%)", "Observed BRF SWIR2", xaxis.lim = c(0,100), yaxis.lim = c(0,0.17), 
                             xaxis.break = seq(0,100,by=20), yaxis.break = seq(0,0.17,by=0.02), 
                             shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none")
  
)

# pdf("graphs/satBRF_vs_ECC_bySiteType_S2A.pdf", width = 7.5, height = 7.5, pointsize = 10)
# with(satBRFvsECC, multiplot(ECC.Blue, ECC.Green, ECC.Red, ECC.RE1, ECC.RE2, ECC.RE3, 
#                             ECC.NIRnarrow, ECC.SWIR1, ECC.SWIR2, cols = 3))
# dev.off()

pdf("graphs/satBRF_vs_ECC_bySpecies_L8.pdf", width = 7.5, height = 5, pointsize = 10)
with(satBRFvsECC, multiplot(ECC.Blue, ECC.Green, ECC.Red, 
                            ECC.NIR, ECC.SWIR1, ECC.SWIR2, cols = 3))
dev.off()



# Sentinel-2 BOA vs TOA 25m -----------------------------------------------
# spc.TOA.25m; spc.BOA.25m
(temp <- left_join(spc.TOA.25m, spc.BOA.25m, by = "ID")) # x = TOA; y = BOA

print(map_df(temp, range), n = Inf, width = Inf)  # Check range


satBRFtoa.vs.satBRFboa <- list(
  
  b2 = my_ggplot_1to1_ms(temp, "B2_490.x", "B2_490.y", "TOA blue", "BOA blue", 
         xaxis.lim = c(0,0.088), yaxis.lim = c(0,0.088),  xaxis.break = seq(0,0.088,by=0.02), yaxis.break = seq(0,0.088,by=0.02)),
       
  b3 = my_ggplot_1to1_ms(temp, "B3_560.x", "B3_560.y", "TOA green", "BOA green", 
         xaxis.lim = c(0,0.082), yaxis.lim = c(0,0.082),  xaxis.break = seq(0,0.082,by=0.02), yaxis.break = seq(0,0.082,by=0.02)),
  
  b4 = my_ggplot_1to1_ms(temp, "B4_665.x", "B4_665.y", "TOA red", "BOA red", 
          xaxis.lim = c(0,0.055), yaxis.lim = c(0,0.055),  xaxis.break = seq(0,0.055,by=0.01), yaxis.break = seq(0,0.055,by=0.01)),
  
  b5 = my_ggplot_1to1_ms(temp, "B5_705.x", "B5_705.y", "TOA RE1", "BOA RE1", 
          xaxis.lim = c(0,0.11), yaxis.lim = c(0,0.11),  xaxis.break = seq(0,0.11,by=0.02), yaxis.break = seq(0,0.11,by=0.02)),
  
  b6 = my_ggplot_1to1_ms(temp, "B6_740.x", "B6_740.y", "TOA RE2", "BOA RE2", 
          xaxis.lim = c(0,0.27), yaxis.lim = c(0,0.27),  xaxis.break = seq(0,0.27,by=0.05), yaxis.break = seq(0,0.27,by=0.05)),
  
  b7 = my_ggplot_1to1_ms(temp, "B7_783.x", "B7_783.y", "TOA RE3", "BOA RE3", 
          xaxis.lim = c(0,0.30), yaxis.lim = c(0,0.30),  xaxis.break = seq(0,0.30,by=0.05), yaxis.break = seq(0,0.30,by=0.05)),
  
  b8 = my_ggplot_1to1_ms(temp, "B8_842.x", "B8_842.y", "TOA NIR", "BOA NIR", 
          xaxis.lim = c(0,0.33), yaxis.lim = c(0,0.33),  xaxis.break = seq(0,0.33,by=0.05), yaxis.break = seq(0,0.33,by=0.05)),
  
  b8a = my_ggplot_1to1_ms(temp, "B8a_865.x", "B8a_865.y", "TOA NIRn", "BOA NIRn", 
          xaxis.lim = c(0,0.33), yaxis.lim = c(0,0.33),  xaxis.break = seq(0,0.33,by=0.05), yaxis.break = seq(0,0.33,by=0.05)),
  
  b11 = my_ggplot_1to1_ms(temp, "B11_1610.x", "B11_1610.y", "TOA SWIR1", "BOA SWIR1", 
          xaxis.lim = c(0,0.19), yaxis.lim = c(0,0.19),  xaxis.break = seq(0,0.19,by=0.05), yaxis.break = seq(0,0.19,by=0.05)),
  
  b12 = my_ggplot_1to1_ms(temp, "B12_2190.x", "B12_2190.y", "TOA SWIR2", "BOA SWIR2", 
          xaxis.lim = c(0,0.1), yaxis.lim = c(0,0.1),  xaxis.break = seq(0,0.1,by=0.02), yaxis.break = seq(0,0.1,by=0.02))
  
  )
    

pdf("graphs/satBRF25m_TOAvsBOA.pdf", width = 7.5, height = 7.5, pointsize = 10)
with(satBRFtoa.vs.satBRFboa, multiplot(b2, b3, b4, b5, b6, b7, b8a, b11, b12, cols = 3))
dev.off()
   




