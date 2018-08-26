wav.S2 <- read_csv2("data/image_data/wav_S2.csv")
source("tests/my_ggplot_funs.R")
source("tests/multiplot.R")


# Read gaps data ----------------------------------------------------------

gaps <- read_csv2("results/gaps.csv") %>%  dplyr::select(-X1)

gaps <- gaps %>%  mutate(ECC = (1 - gaps1) * 100,                                                  # Data prepared for plotting ***
                         Site_type_lab = factor(Site_type_lab, c("Herb-rich", "Mesic", "Xeric")),
                         dominant_species = factor(dominant_species, c("Pine", "Spruce", "Birch", "Mixed")))

gaps %>% dplyr::select(b2:b12) %>% map_df(range)         # Check the BRF range for plot y-axis range



# Read the spectra --------------------------------------------------------

spc.BOA.25m <- read_csv2("results/kkj2TOutm34N_BOA_S2_sq25m.csv")          # BOA

# Merge gaps and spc ------------------------------------------------------

gaps.spc.BOA.25m <- left_join(gaps, spc.BOA.25m)            # BOA

# Template for plotting ---------------------------------------------------

pts.cols <- c("magenta", "chartreuse4", "dodgerblue", "dark blue", "brown", "dark orange")
pts.pchs <- c(1,7,5,6,4,2)



## LAItrue by Site fertility or dominant species #######################################
temp <- gaps.spc.BOA.25m                                    # Which spectra (extraction footprint) ?


# satBRFvsLAItrue ---------------------------------------------------------


satBRFvsLAItrue <- list(                                # LAItrue ********************************************
  
  LAItrue.b2 = my_ggplot_ms(temp, "LAItrue", "B2_490", "dominant_species",
                            "LAItrue", "Observed BRF blue", xaxis.lim = c(0,6), yaxis.lim = c(0,0.045), 
                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.05,by=0.01), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.b3 = my_ggplot_ms(temp, "LAItrue", "B3_560", "dominant_species",
                            "LAItrue", "Observed BRF green", xaxis.lim = c(0,6), yaxis.lim = c(0,0.1), 
                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.1,by=0.02), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.b4 = my_ggplot_ms(temp, "LAItrue", "B4_665", "dominant_species",
                            "LAItrue", "Observed BRF red", xaxis.lim = c(0,6), yaxis.lim = c(0,0.05), 
                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.05,by=0.01), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.b5 = my_ggplot_ms(temp, "LAItrue", "B5_705", "dominant_species",
                            "LAItrue", "Observed BRF RE1", xaxis.lim = c(0,6), yaxis.lim = c(0,0.14), 
                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.14,by=0.02), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.b6 = my_ggplot_ms(temp, "LAItrue", "B6_740", "dominant_species",
                            "LAItrue", "Observed BRF RE2", xaxis.lim = c(0,6), yaxis.lim = c(0,0.5), 
                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.5,by=0.05), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.b7 = my_ggplot_ms(temp, "LAItrue", "B7_783", "dominant_species",
                            "LAItrue", "Observed BRF RE3", xaxis.lim = c(0,6), yaxis.lim = c(0,0.5), 
                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.5,by=0.05), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  # LAItrue.b8 = my_ggplot_ms(temp, "LAItrue", "B8_842", "dominant_species",
  #                       "LAItrue", "Observed BRF NIR", xaxis.lim = c(0,6), yaxis.lim = c(0,0.5), 
  #                       xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.5,by=0.05), 
  #                       shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.b8a = my_ggplot_ms(temp, "LAItrue", "B8a_865", "dominant_species",
                             "LAItrue", "Observed BRF NIRn", xaxis.lim = c(0,6), yaxis.lim = c(0,0.5), 
                             xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.5,by=0.05), 
                             shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.b11 = my_ggplot_ms(temp, "LAItrue", "B11_1610", "dominant_species",
                             "LAItrue", "Observed BRF SWIR1", xaxis.lim = c(0,6), yaxis.lim = c(0,0.28), 
                             xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.28,by=0.05), 
                             shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.b12 = my_ggplot_ms(temp, "LAItrue", "B12_2190", "dominant_species",
                             "LAItrue", "Observed BRF SWIR2", xaxis.lim = c(0,6), yaxis.lim = c(0,0.17), 
                             xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.17,by=0.02), 
                             shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none")
  
)

pdf("graphs/kkj2TOutm34N_satBRF25m_vs_LAIt_bySpecies_BOA.pdf", width = 7.5, height = 7.5, pointsize = 10)
with(satBRFvsLAItrue, multiplot(LAItrue.b2, LAItrue.b3, LAItrue.b4, LAItrue.b5, LAItrue.b6, LAItrue.b7, LAItrue.b8a, LAItrue.b11, LAItrue.b12, cols = 3))
dev.off()





# Correlation sat BRF vs LAItrue ------------------------------------------
gaps.spc.BOA.25m %>% dplyr::select(B2_490:B12_2190) %>% map_df(function(x) round(cor(x, gaps.spc.BOA.25m$LAItrue), 3)) %>% write.csv2("results/correlation_satBRFvsLAItrue_25m_kkj2TOutm34N.csv")
gaps.spc.BOA.25m %>% dplyr::select(B2_490:B12_2190) %>% map_df(function(x) round(cor(x, gaps.spc.BOA.25m$LAIeff), 3)) %>% write.csv2("results/correlation_satBRFvsLAIeff_25m_kkj2TOutm34N.csv")




# By site fertility -------------------------------------------------------




satBRFvsLAItrue <- list(                                # LAItrue ********************************************
  
  LAItrue.b2 = my_ggplot_ms(temp, "LAItrue", "B2_490", "Site_type_lab",
                            "LAItrue", "Observed BRF blue", xaxis.lim = c(0,6), yaxis.lim = c(0,0.045), 
                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.05,by=0.01), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.b3 = my_ggplot_ms(temp, "LAItrue", "B3_560", "Site_type_lab",
                            "LAItrue", "Observed BRF green", xaxis.lim = c(0,6), yaxis.lim = c(0,0.1), 
                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.1,by=0.02), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.b4 = my_ggplot_ms(temp, "LAItrue", "B4_665", "Site_type_lab",
                            "LAItrue", "Observed BRF red", xaxis.lim = c(0,6), yaxis.lim = c(0,0.05), 
                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.05,by=0.01), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.b5 = my_ggplot_ms(temp, "LAItrue", "B5_705", "Site_type_lab",
                            "LAItrue", "Observed BRF RE1", xaxis.lim = c(0,6), yaxis.lim = c(0,0.14), 
                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.14,by=0.02), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.b6 = my_ggplot_ms(temp, "LAItrue", "B6_740", "Site_type_lab",
                            "LAItrue", "Observed BRF RE2", xaxis.lim = c(0,6), yaxis.lim = c(0,0.5), 
                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.5,by=0.05), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.b7 = my_ggplot_ms(temp, "LAItrue", "B7_783", "Site_type_lab",
                            "LAItrue", "Observed BRF RE3", xaxis.lim = c(0,6), yaxis.lim = c(0,0.5), 
                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.5,by=0.05), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  # LAItrue.b8 = my_ggplot_ms(temp, "LAItrue", "B8_842", "Site_type_lab",
  #                       "LAItrue", "Observed BRF NIR", xaxis.lim = c(0,6), yaxis.lim = c(0,0.5), 
  #                       xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.5,by=0.05), 
  #                       shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.b8a = my_ggplot_ms(temp, "LAItrue", "B8a_865", "Site_type_lab",
                             "LAItrue", "Observed BRF NIRn", xaxis.lim = c(0,6), yaxis.lim = c(0,0.5), 
                             xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.5,by=0.05), 
                             shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.b11 = my_ggplot_ms(temp, "LAItrue", "B11_1610", "Site_type_lab",
                             "LAItrue", "Observed BRF SWIR1", xaxis.lim = c(0,6), yaxis.lim = c(0,0.28), 
                             xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.28,by=0.05), 
                             shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.b12 = my_ggplot_ms(temp, "LAItrue", "B12_2190", "Site_type_lab",
                             "LAItrue", "Observed BRF SWIR2", xaxis.lim = c(0,6), yaxis.lim = c(0,0.17), 
                             xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.17,by=0.02), 
                             shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none")
  
)

pdf("graphs/kkj2TOutm34N_satBRF25m_vs_LAIt_byFertile_BOA.pdf", width = 7.5, height = 7.5, pointsize = 10)
with(satBRFvsLAItrue, multiplot(LAItrue.b2, LAItrue.b3, LAItrue.b4, LAItrue.b5, LAItrue.b6, LAItrue.b7, LAItrue.b8a, LAItrue.b11, LAItrue.b12, cols = 3))
dev.off()


# Plot simulated BRF vs satellite BRF by Species -------------------------------------
spc.BOA.25m <- read_csv2("results/kkj2toutm34N_BOA_S2_sq25m.csv")    

spc.TOA.25m <- read_csv2("results/kkj2toutm34N_TOA_S2_sq25m.csv")

sim.sat.BOA.25m <- left_join(paras.BRF.res, spc.BOA.25m)         # b2 = simulated; B2_490 = satellite        # BOA 

sim.sat.TOA.25m <- left_join(paras.BRF.res, spc.TOA.25m)         # b2 = simulated; B2_490 = satellite        # or TOA    



temp <- sim.sat.TOA.25m                                         # Which extraction footprint ?
 

temp2 <- temp %>%  dplyr::select(b2:b12)
temp3 <- temp %>%  dplyr::select(B2_490:B12_2190)
print(map_df(temp2, range), n = Inf, width = Inf)  # Check range
print(map_df(temp3, range), n = Inf, width = Inf)  # Check range

simBRF.vs.satBRF.bySpecies <- list(
  
  b2 = my_ggplot_1to1_group_ms(temp, "b2", "B2_490", group = "dominant_species", "Simulated blue", "Observed blue", 
                               xaxis.lim = c(0,0.042), yaxis.lim = c(0,0.042),  xaxis.break = seq(0,0.042,by=0.01), yaxis.break = seq(0,0.042,by=0.01),
                               shape.pch = pts.pchs, shape.col = pts.cols),
  
  b3 = my_ggplot_1to1_group_ms(temp, "b3", "B3_560", group = "dominant_species", "Simulated green", "Observed green", 
                               xaxis.lim = c(0,0.09), yaxis.lim = c(0,0.09),  xaxis.break = seq(0,0.09,by=0.02), yaxis.break = seq(0,0.09,by=0.02),
                               shape.pch = pts.pchs, shape.col = pts.cols),
  
  b4 = my_ggplot_1to1_group_ms(temp, "b4", "B4_665", group = "dominant_species", "Simulated red", "Observed red", 
                               xaxis.lim = c(0,0.045), yaxis.lim = c(0,0.045),  xaxis.break = seq(0,0.045,by=0.01), yaxis.break = seq(0,0.045,by=0.01),
                               shape.pch = pts.pchs, shape.col = pts.cols),
  
  b5 = my_ggplot_1to1_group_ms(temp, "b5", "B5_705", group = "dominant_species", "Simulated RE1", "Observed RE1", 
                               xaxis.lim = c(0,0.14), yaxis.lim = c(0,0.14),  xaxis.break = seq(0,0.14,by=0.04), yaxis.break = seq(0,0.14,by=0.04),
                               shape.pch = pts.pchs, shape.col = pts.cols),
  
  b6 = my_ggplot_1to1_group_ms(temp, "b6", "B6_740", group = "dominant_species", "Simulated RE2", "Observed RE2", 
                               xaxis.lim = c(0,0.42), yaxis.lim = c(0,0.42),  xaxis.break = seq(0,0.42,by=0.1), yaxis.break = seq(0,0.42,by=0.1),
                               shape.pch = pts.pchs, shape.col = pts.cols),
  
  b7 = my_ggplot_1to1_group_ms(temp, "b7", "B7_783", group = "dominant_species", "Simulated RE3", "Observed RE3", 
                               xaxis.lim = c(0,0.5), yaxis.lim = c(0,0.5),  xaxis.break = seq(0,0.5,by=0.1), yaxis.break = seq(0,0.5,by=0.1),
                               shape.pch = pts.pchs, shape.col = pts.cols),
  
  b8 = my_ggplot_1to1_group_ms(temp, "b8", "B8_842", group = "dominant_species", "Simulated NIR", "Observed NIR", 
                               xaxis.lim = c(0,0.5), yaxis.lim = c(0,0.5),  xaxis.break = seq(0,0.5,by=0.1), yaxis.break = seq(0,0.5,by=0.1),
                               shape.pch = pts.pchs, shape.col = pts.cols),
  
  b8a = my_ggplot_1to1_group_ms(temp, "b8a", "B8a_865", group = "dominant_species", "Simulated NIRn", "Observed NIRn", 
                                xaxis.lim = c(0,0.5), yaxis.lim = c(0,0.5),  xaxis.break = seq(0,0.5,by=0.1), yaxis.break = seq(0,0.5,by=0.1),
                                shape.pch = pts.pchs, shape.col = pts.cols),
  
  b11 = my_ggplot_1to1_group_ms(temp, "b11", "B11_1610", group = "dominant_species", "Simulated SWIR1", "Observed SWIR1", 
                                xaxis.lim = c(0,0.28), yaxis.lim = c(0,0.28),  xaxis.break = seq(0,0.28,by=0.05), yaxis.break = seq(0,0.28,by=0.05),
                                shape.pch = pts.pchs, shape.col = pts.cols),
  
  b12 = my_ggplot_1to1_group_ms(temp, "b12", "B12_2190", group = "dominant_species", "Simulated SWIR2", "Observed SWIR2", 
                                xaxis.lim = c(0,0.17), yaxis.lim = c(0,0.17),  xaxis.break = seq(0,0.17,by=0.05), yaxis.break = seq(0,0.17,by=0.05),
                                shape.pch = pts.pchs, shape.col = pts.cols)
  
)


# TOA needs different range -----------------------------------------------

simBRF.vs.satBRF.bySpecies <- list(
  
  b2 = my_ggplot_1to1_group_ms(temp, "b2", "B2_490", group = "dominant_species", "Simulated blue", "Observed blue", 
                               xaxis.lim = c(0,0.085), yaxis.lim = c(0,0.085),  xaxis.break = seq(0,0.085,by=0.02), yaxis.break = seq(0,0.085,by=0.02),
                               shape.pch = pts.pchs, shape.col = pts.cols),
  
  b3 = my_ggplot_1to1_group_ms(temp, "b3", "B3_560", group = "dominant_species", "Simulated green", "Observed green", 
                               xaxis.lim = c(0,0.09), yaxis.lim = c(0,0.09),  xaxis.break = seq(0,0.09,by=0.02), yaxis.break = seq(0,0.09,by=0.02),
                               shape.pch = pts.pchs, shape.col = pts.cols),
  
  b4 = my_ggplot_1to1_group_ms(temp, "b4", "B4_665", group = "dominant_species", "Simulated red", "Observed red", 
                               xaxis.lim = c(0,0.045), yaxis.lim = c(0,0.045),  xaxis.break = seq(0,0.045,by=0.01), yaxis.break = seq(0,0.045,by=0.01),
                               shape.pch = pts.pchs, shape.col = pts.cols),
  
  b5 = my_ggplot_1to1_group_ms(temp, "b5", "B5_705", group = "dominant_species", "Simulated RE1", "Observed RE1", 
                               xaxis.lim = c(0,0.14), yaxis.lim = c(0,0.14),  xaxis.break = seq(0,0.14,by=0.04), yaxis.break = seq(0,0.14,by=0.04),
                               shape.pch = pts.pchs, shape.col = pts.cols),
  
  b6 = my_ggplot_1to1_group_ms(temp, "b6", "B6_740", group = "dominant_species", "Simulated RE2", "Observed RE2", 
                               xaxis.lim = c(0,0.42), yaxis.lim = c(0,0.42),  xaxis.break = seq(0,0.42,by=0.1), yaxis.break = seq(0,0.42,by=0.1),
                               shape.pch = pts.pchs, shape.col = pts.cols),
  
  b7 = my_ggplot_1to1_group_ms(temp, "b7", "B7_783", group = "dominant_species", "Simulated RE3", "Observed RE3", 
                               xaxis.lim = c(0,0.5), yaxis.lim = c(0,0.5),  xaxis.break = seq(0,0.5,by=0.1), yaxis.break = seq(0,0.5,by=0.1),
                               shape.pch = pts.pchs, shape.col = pts.cols),
  
  b8 = my_ggplot_1to1_group_ms(temp, "b8", "B8_842", group = "dominant_species", "Simulated NIR", "Observed NIR", 
                               xaxis.lim = c(0,0.5), yaxis.lim = c(0,0.5),  xaxis.break = seq(0,0.5,by=0.1), yaxis.break = seq(0,0.5,by=0.1),
                               shape.pch = pts.pchs, shape.col = pts.cols),
  
  b8a = my_ggplot_1to1_group_ms(temp, "b8a", "B8a_865", group = "dominant_species", "Simulated NIRn", "Observed NIRn", 
                                xaxis.lim = c(0,0.5), yaxis.lim = c(0,0.5),  xaxis.break = seq(0,0.5,by=0.1), yaxis.break = seq(0,0.5,by=0.1),
                                shape.pch = pts.pchs, shape.col = pts.cols),
  
  b11 = my_ggplot_1to1_group_ms(temp, "b11", "B11_1610", group = "dominant_species", "Simulated SWIR1", "Observed SWIR1", 
                                xaxis.lim = c(0,0.28), yaxis.lim = c(0,0.28),  xaxis.break = seq(0,0.28,by=0.05), yaxis.break = seq(0,0.28,by=0.05),
                                shape.pch = pts.pchs, shape.col = pts.cols),
  
  b12 = my_ggplot_1to1_group_ms(temp, "b12", "B12_2190", group = "dominant_species", "Simulated SWIR2", "Observed SWIR2", 
                                xaxis.lim = c(0,0.17), yaxis.lim = c(0,0.17),  xaxis.break = seq(0,0.17,by=0.05), yaxis.break = seq(0,0.17,by=0.05),
                                shape.pch = pts.pchs, shape.col = pts.cols)
  
)





# pdf("graphs/kkj2TOutm34N_simVSsatBRF_25m_bySpecies.pdf", width = 7.5, height = 7.5, pointsize = 10)
pdf("graphs/kkj2TOutm34N_simVSsatBRF_25m_bySpecies_toa.pdf", width = 7.5, height = 7.5, pointsize = 10)

with(simBRF.vs.satBRF.bySpecies, multiplot(b2, b3, b4, b5, b6, b7, b8a, b11, b12, cols = 3))
dev.off()


# RMSD sat BRF vs sim BRF ------------------------------------------
source("munge/RMSE.R")

temp <- sim.sat.TOA.25m                                          # Which extraction footprint ? BOA or TOA ?


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

# write.csv2(RMSD.temp, "results/RMSDsimsat_S2_BOA_25m_kkj2TOutm34N.csv")
write.csv2(RMSD.temp, "results/RMSDsimsat_S2_TOA_25m_kkj2TOutm34N.csv")


