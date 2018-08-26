
# Read PARAS BRF ----------------------------------------------------------

paras.BRF.S2A <- read_csv2("results/paras_BRF_fixedInputs_S2A_extended.csv") 
paras.BRF.L8 <- read_csv2("results/paras_BRF_fixedInputs_L8_extended.csv") 


# Read gaps ---------------------------------------------------------------

gaps <- read_csv2("results/gaps.csv") 


# Merge gaps and PARAS BRF ------------------------------------------------

gaps.paras.S2A <- left_join(gaps, paras.BRF.S2A, by = c("ID" = "ID"))
gaps.paras.L8 <- left_join(gaps, paras.BRF.L8, by = c("ID" = "ID"))


# Correlations ------------------------------------------------------------

gaps.paras.S2A %>% dplyr::select(Blue:SWIR2) %>% 
  map_df(function(x) round(cor(x, gaps.paras.S2A$ECC), 3)) %>% write.csv2("results/corr_simBRFvsECC_S2A.csv")
gaps.paras.S2A %>% dplyr::select(Blue:SWIR2) %>% 
  map_df(function(x) round(cor(x, gaps.paras.S2A$LAIeff), 3)) %>% write.csv2("results/corr_simBRFvsLAIeff_S2A.csv")
gaps.paras.S2A %>% dplyr::select(Blue:SWIR2) %>% 
  map_df(function(x) round(cor(x, gaps.paras.S2A$LAItrue), 3)) %>% write.csv2("results/corr_simBRFvsLAItrue_S2A.csv")

gaps.paras.L8 %>% dplyr::select(Blue:SWIR2) %>% 
  map_df(function(x) round(cor(x, gaps.paras.L8$ECC), 3)) %>% write.csv2("results/corr_simBRFvsECC_L8.csv")
gaps.paras.L8 %>% dplyr::select(Blue:SWIR2) %>% 
  map_df(function(x) round(cor(x, gaps.paras.L8$LAIeff), 3)) %>% write.csv2("results/corr_simBRFvsLAIeff_L8.csv")
gaps.paras.L8 %>% dplyr::select(Blue:SWIR2) %>% 
  map_df(function(x) round(cor(x, gaps.paras.L8$LAItrue), 3)) %>% write.csv2("results/corr_simBRFvsLAItrue_L8.csv")


# Plot simulated BRF vs canopy structure -------------------------------------------
  
temp <- gaps.paras.S2A %>%                    # S2A or L8                         
  mutate(ECC = (1 - gaps1) * 100,                             # Data prepared for plotting ***
         Site_type_lab = factor(Site_type_lab, c("Herb-rich", "Mesic", "Xeric")),
         Dominant_species = factor(Dominant_species, c("Pine", "Spruce", "Birch", "Mixed")))

temp %>% dplyr::select(Blue:SWIR2) %>% map_df(range)         # Check the BRF range for plot y-axis range


## ECC by Site fertility OR dominant species #######################################
simBRFvsECC <- list(                                # ECC ********************************************
  
  ECC.Blue = my_ggplot_ms(temp, "ECC", "Blue", "Site_type_lab",
                         "ECC (%)", "Simulated BRF blue", xaxis.lim = c(0,100), yaxis.lim = c(0,0.045), 
                         xaxis.break = seq(0,100,by=20), yaxis.break = seq(0,0.05,by=0.01), 
                         shape.col = pts.cols, shape.pch = pts.pchs, 
                         legend.pos = c(0.19, 0.25)),  # legend.pos = c(0.16, 0.25) for by Species
  
  ECC.Green = my_ggplot_ms(temp, "ECC", "Green", "Site_type_lab",
                         "ECC (%)", "Simulated BRF green", xaxis.lim = c(0,100), yaxis.lim = c(0,0.1), 
                         xaxis.break = seq(0,100,by=20), yaxis.break = seq(0,0.1,by=0.02), 
                         shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  ECC.Red = my_ggplot_ms(temp, "ECC", "Red", "Site_type_lab",
                        "ECC (%)", "Simulated BRF red", xaxis.lim = c(0,100), yaxis.lim = c(0,0.05), 
                        xaxis.break = seq(0,100,by=20), yaxis.break = seq(0,0.05,by=0.01), 
                         shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  ECC.RE1 = my_ggplot_ms(temp, "ECC", "RE1", "Site_type_lab",
                        "ECC (%)", "Simulated BRF RE1", xaxis.lim = c(0,100), yaxis.lim = c(0,0.14),
                        xaxis.break = seq(0,100,by=20), yaxis.break = seq(0,0.14,by=0.02),
                        shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),

  ECC.RE2 = my_ggplot_ms(temp, "ECC", "RE2", "Site_type_lab",
                        "ECC (%)", "Simulated BRF RE2", xaxis.lim = c(0,100), yaxis.lim = c(0,0.5),
                        xaxis.break = seq(0,100,by=20), yaxis.break = seq(0,0.5,by=0.05),
                        shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),

  ECC.RE3 = my_ggplot_ms(temp, "ECC", "RE3", "Site_type_lab",
                        "ECC (%)", "Simulated BRF RE3", xaxis.lim = c(0,100), yaxis.lim = c(0,0.5),
                        xaxis.break = seq(0,100,by=20), yaxis.break = seq(0,0.5,by=0.05),
                        shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),

  ECC.NIRnarrow = my_ggplot_ms(temp, "ECC", "NIRnarrow", "Site_type_lab",
                        "ECC (%)", "Simulated BRF NIRn", xaxis.lim = c(0,100), yaxis.lim = c(0,0.5),
                        xaxis.break = seq(0,100,by=20), yaxis.break = seq(0,0.5,by=0.05),
                        shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),

  # Landsat NIR
  # ECC.NIR = my_ggplot_ms(temp, "ECC", "NIR", "Site_type_lab",
  #                       "ECC (%)", "Simulated BRF NIR", xaxis.lim = c(0,100), yaxis.lim = c(0,0.5),
  #                       xaxis.break = seq(0,100,by=20), yaxis.break = seq(0,0.5,by=0.05),
  #                       shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  ECC.SWIR1 = my_ggplot_ms(temp, "ECC", "SWIR1", "Site_type_lab",
                        "ECC (%)", "Simulated BRF SWIR1", xaxis.lim = c(0,100), yaxis.lim = c(0,0.28), 
                        xaxis.break = seq(0,100,by=20), yaxis.break = seq(0,0.28,by=0.05), 
                        shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  ECC.SWIR2 = my_ggplot_ms(temp, "ECC", "SWIR2", "Site_type_lab",
                        "ECC (%)", "Simulated BRF SWIR2", xaxis.lim = c(0,100), yaxis.lim = c(0,0.17), 
                        xaxis.break = seq(0,100,by=20), yaxis.break = seq(0,0.17,by=0.02), 
                        shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none")
  
)


pdf("graphs/simBRF_vs_ECC_bySiteType_S2A.pdf", width = 7.5, height = 7.5, pointsize = 10)
with(simBRFvsECC, multiplot(ECC.Blue, ECC.Green, ECC.Red, ECC.RE1, ECC.RE2, ECC.RE3, ECC.NIRnarrow, ECC.SWIR1, ECC.SWIR2, cols = 3))
dev.off()

# pdf("graphs/simBRF_vs_ECC_bySiteType_L8.pdf", width = 7.5, height = 5, pointsize = 10)
# with(simBRFvsECC, multiplot(ECC.Blue, ECC.Green, ECC.Red, ECC.NIR, ECC.SWIR1, ECC.SWIR2, cols = 3))
# dev.off()



## LAItrue by Site fertility OR dominant species #######################################
simBRFvsLAItrue <- list(                                # LAItrue ********************************************
  
  LAItrue.Blue = my_ggplot_ms(temp, "LAItrue", "Blue", "Site_type_lab",
                        expression('LAI'[true]), "Simulated BRF blue", xaxis.lim = c(0,6), yaxis.lim = c(0,0.045), 
                        xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.05,by=0.01), 
                        shape.col = pts.cols, shape.pch = pts.pchs, 
                        legend.pos = c(0.19, 0.25)),  # legend.pos = c(0.16, 0.25) for by Species
  
  LAItrue.Green = my_ggplot_ms(temp, "LAItrue", "Green", "Site_type_lab",
                        expression('LAI'[true]), "Simulated BRF green", xaxis.lim = c(0,6), yaxis.lim = c(0,0.1), 
                        xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.1,by=0.02), 
                        shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.Red = my_ggplot_ms(temp, "LAItrue", "Red", "Site_type_lab",
                        expression('LAI'[true]), "Simulated BRF red", xaxis.lim = c(0,6), yaxis.lim = c(0,0.05), 
                        xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.05,by=0.01), 
                        shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.RE1 = my_ggplot_ms(temp, "LAItrue", "RE1", "Site_type_lab",
                        expression('LAI'[true]), "Simulated BRF RE1", xaxis.lim = c(0,6), yaxis.lim = c(0,0.14),
                        xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.14,by=0.02),
                        shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),

  LAItrue.RE2 = my_ggplot_ms(temp, "LAItrue", "RE2", "Site_type_lab",
                        expression('LAI'[true]), "Simulated BRF RE2", xaxis.lim = c(0,6), yaxis.lim = c(0,0.5),
                        xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.5,by=0.05),
                        shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),

  LAItrue.RE3 = my_ggplot_ms(temp, "LAItrue", "RE3", "Site_type_lab",
                        expression('LAI'[true]), "Simulated BRF RE3", xaxis.lim = c(0,6), yaxis.lim = c(0,0.5),
                        xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.5,by=0.05),
                        shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),


  LAItrue.NIRnarrow = my_ggplot_ms(temp, "LAItrue", "NIRnarrow", "Site_type_lab",
                         expression('LAI'[true]), "Simulated BRF NIRn", xaxis.lim = c(0,6), yaxis.lim = c(0,0.5),
                         xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.5,by=0.05),
                         shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  # Landsat NIR
  # LAItrue.NIR = my_ggplot_ms(temp, "LAItrue", "NIR", "Site_type_lab",
  #                        expression('LAI'[true]), "Simulated BRF NIR", xaxis.lim = c(0,6), yaxis.lim = c(0,0.5),
  #                        xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.5,by=0.05),
  #                        shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  # 
  LAItrue.SWIR1 = my_ggplot_ms(temp, "LAItrue", "SWIR1", "Site_type_lab",
                         expression('LAI'[true]), "Simulated BRF SWIR1", xaxis.lim = c(0,6), yaxis.lim = c(0,0.28), 
                         xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.28,by=0.05), 
                         shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.SWIR2 = my_ggplot_ms(temp, "LAItrue", "SWIR2", "Site_type_lab",
                         expression('LAI'[true]), "Simulated BRF SWIR2", xaxis.lim = c(0,6), yaxis.lim = c(0,0.17), 
                         xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.17,by=0.02), 
                         shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none")
  
)

pdf("graphs/simBRF_vs_LAIt_bySiteType_S2A.pdf", width = 7.5, height = 7.5, pointsize = 10)
with(simBRFvsLAItrue, multiplot(LAItrue.Blue, LAItrue.Green, LAItrue.Red, LAItrue.RE1,
                                LAItrue.RE2, LAItrue.RE3, LAItrue.NIRnarrow, LAItrue.SWIR1, LAItrue.SWIR2, cols = 3))
dev.off()

# pdf("graphs/simBRF_vs_LAIt_bySiteType_L8.pdf", width = 7.5, height = 5, pointsize = 10)
# with(simBRFvsLAItrue, multiplot(LAItrue.Blue, LAItrue.Green, LAItrue.Red, 
#                                 LAItrue.NIR, LAItrue.SWIR1, LAItrue.SWIR2, cols = 3))
# dev.off()



# PARAS BRF of L8 vs S2 ---------------------------------------------------
names(paras.BRF.S2A)
names(paras.BRF.L8)

BRF.range <- list(blue = c(0,0.045), green = c(0,0.1), red = c(0,0.05),
                  nir = c(0,0.5), swir1 = c(0,0.3), swir2 = c(0,0.18))

RMSD.ls <- list(blue = round(RMSE(paras.BRF.L8$Blue, paras.BRF.S2A$Blue),3),
                green = round(RMSE(paras.BRF.L8$Green, paras.BRF.S2A$Green),3),
                red = round(RMSE(paras.BRF.L8$Red, paras.BRF.S2A$Red),3),
                nir = round(RMSE(paras.BRF.L8$NIR, paras.BRF.S2A$NIRnarrow),3),
                swir1 = round(RMSE(paras.BRF.L8$SWIR1, paras.BRF.S2A$SWIR1),3),
                swir2 = round(RMSE(paras.BRF.L8$SWIR2, paras.BRF.S2A$SWIR2),3))


x11()
par(mfrow = c(2,3), ps = 14)

plot(paras.BRF.L8$Blue, paras.BRF.S2A$Blue, 
     xlim = BRF.range$blue, ylim = BRF.range$blue,
     xlab = "L8 Blue", ylab = "S2 Blue")
abline(0,1); text(0.02, 0.04, str_c("RMSD = ", RMSD.ls$blue))

plot(paras.BRF.L8$Green, paras.BRF.S2A$Green, 
     xlim = BRF.range$green, ylim = BRF.range$green,
     xlab = "L8 Green", ylab = "S2 Green")
abline(0,1); text(0.03, 0.09, str_c("RMSD = ", RMSD.ls$green))

plot(paras.BRF.L8$Red, paras.BRF.S2A$Red, 
     xlim = BRF.range$red, ylim = BRF.range$red,
     xlab = "L8 Red", ylab = "S2 Red")
abline(0,1); text(0.02, 0.045, str_c("RMSD = ", RMSD.ls$red))

plot(paras.BRF.L8$NIR, paras.BRF.S2A$NIRnarrow, 
     xlim = BRF.range$nir, ylim = BRF.range$nir,
     xlab = "L8 NIR", ylab = "S2 NIR")
abline(0,1); text(0.15, 0.45, str_c("RMSD = ", RMSD.ls$nir))

plot(paras.BRF.L8$SWIR1, paras.BRF.S2A$SWIR1, 
     xlim = BRF.range$swir1, ylim = BRF.range$swir1,
     xlab = "L8 SWIR1", ylab = "S2 SWIR1")
abline(0,1); text(0.1, 0.27, str_c("RMSD = ", RMSD.ls$swir1))

plot(paras.BRF.L8$SWIR2, paras.BRF.S2A$SWIR2, 
     xlim = BRF.range$swir2, ylim = BRF.range$swir2,
     xlab = "L8 SWIR2", ylab = "S2 SWIR2")
abline(0,1); text(0.075, 0.16, str_c("RMSD = ", RMSD.ls$swir2))

