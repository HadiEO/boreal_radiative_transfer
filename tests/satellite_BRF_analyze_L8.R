
# Landsat 8 bands center --------------------------------------------------
temp <- get.sensor.characteristics("Landsat8")



# Read gaps data ----------------------------------------------------------

gaps <- read_csv2("results/gaps.csv") %>%  dplyr::select(-X1)

gaps <- gaps %>%  mutate(ECC = (1 - gaps1) * 100,                                                  # Data prepared for plotting ***
                         Site_type_lab = factor(Site_type_lab, c("Herb-rich", "Mesic", "Xeric")),
                         dominant_species = factor(dominant_species, c("Pine", "Spruce", "Birch", "Mixed")))




# Read the spectra --------------------------------------------------------

spc.TOA.25m <- read_csv2("results/TOA_L8_sq25m.csv")         # TOA from Landsat 8
spc.TOA.30m <- read_csv2("results/TOA_L8_sq30m.csv")
spc.TOA.60m <- read_csv2("results/TOA_L8_sq60m.csv")

spc.BOA.25m <- read_csv2("results/BOA_L8_sq25m_2014_wrongcoords.csv")         # TOA from Landsat 8


# Merge gaps and spc ------------------------------------------------------

gaps.spc.TOA.25m <- left_join(gaps, spc.TOA.25m)            # TOA from Landsat 8
gaps.spc.TOA.30m <- left_join(gaps, spc.TOA.30m)
gaps.spc.TOA.60m <- left_join(gaps, spc.TOA.60m)

gaps.spc.BOA.25m <- left_join(gaps, spc.BOA.25m)            # BOA from Landsat 8


# Template for plotting ---------------------------------------------------

pts.cols <- c("magenta", "chartreuse4", "dodgerblue", "dark blue", "brown", "dark orange")
pts.pchs <- c(1,7,5,6,4,2)



## LAItrue by Site fertility or dominant species #######################################
temp <- gaps.spc.BOA.25m                                    # Which spectra (extraction footprint) ?
# temp <- gaps.spc.TOA.30m
# temp <- gaps.spc.TOA.60m

temp %>% dplyr::select(BOA_B2_blue:BOA_B7_swir2) %>% map_df(range)         # Check the BRF range for plot y-axis range



# satBRFvsLAItrue ---------------------------------------------------------

satBRFvsLAItrue <- list(                                # LAItrue ********************************************
  
  LAItrue.b2 = my_ggplot_ms(temp, "LAItrue", "TOA_B2_blue", "dominant_species",
                            "LAItrue", "Observed BRF blue", xaxis.lim = c(0,6), yaxis.lim = c(0,0.065), 
                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.065,by=0.01), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.b3 = my_ggplot_ms(temp, "LAItrue", "TOA_B3_green", "dominant_species",
                            "LAItrue", "Observed BRF green", xaxis.lim = c(0,6), yaxis.lim = c(0,0.055), 
                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.055,by=0.01), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.b4 = my_ggplot_ms(temp, "LAItrue", "TOA_B4_red", "dominant_species",
                            "LAItrue", "Observed BRF red", xaxis.lim = c(0,6), yaxis.lim = c(0,0.04), 
                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.04,by=0.01), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.b5 = my_ggplot_ms(temp, "LAItrue", "TOA_B5_nir", "dominant_species",
                            "LAItrue", "Observed BRF NIR", xaxis.lim = c(0,6), yaxis.lim = c(0,0.2), 
                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.2,by=0.05), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.b6 = my_ggplot_ms(temp, "LAItrue", "TOA_B6_swir1", "dominant_species",
                            "LAItrue", "Observed BRF SWIR1", xaxis.lim = c(0,6), yaxis.lim = c(0,0.11), 
                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.11,by=0.02), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.b7 = my_ggplot_ms(temp, "LAItrue", "TOA_B7_swir2", "dominant_species",
                            "LAItrue", "Observed BRF SWIR2", xaxis.lim = c(0,6), yaxis.lim = c(0,0.047), 
                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.047,by=0.01), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none")
  
)



# BOA needs different range -----------------------------------------------

satBRFvsLAItrue <- list(                                # LAItrue ********************************************
  
  LAItrue.b2 = my_ggplot_ms(temp, "LAItrue", "BOA_B2_blue", "dominant_species",
                            "LAItrue", "Observed BRF blue", xaxis.lim = c(0,6), yaxis.lim = c(0,0.065), 
                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.065,by=0.01), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.b3 = my_ggplot_ms(temp, "LAItrue", "BOA_B3_green", "dominant_species",
                            "LAItrue", "Observed BRF green", xaxis.lim = c(0,6), yaxis.lim = c(0,0.063), 
                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.063,by=0.01), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.b4 = my_ggplot_ms(temp, "LAItrue", "BOA_B4_red", "dominant_species",
                            "LAItrue", "Observed BRF red", xaxis.lim = c(0,6), yaxis.lim = c(0,0.055), 
                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.055,by=0.01), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.b5 = my_ggplot_ms(temp, "LAItrue", "BOA_B5_nir", "dominant_species",
                            "LAItrue", "Observed BRF NIR", xaxis.lim = c(0,6), yaxis.lim = c(0,0.32), 
                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.32,by=0.1), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.b6 = my_ggplot_ms(temp, "LAItrue", "BOA_B6_swir1", "dominant_species",
                            "LAItrue", "Observed BRF SWIR1", xaxis.lim = c(0,6), yaxis.lim = c(0,0.21), 
                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.21,by=0.05), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAItrue.b7 = my_ggplot_ms(temp, "LAItrue", "BOA_B7_swir2", "dominant_species",
                            "LAItrue", "Observed BRF SWIR2", xaxis.lim = c(0,6), yaxis.lim = c(0,0.12), 
                            xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,0.12,by=0.02), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none")
  
)



# pdf("graphs/satBRF25m_vs_LAIt_bySpecies_TOA_L8.pdf", width = 7.5, height = 5, pointsize = 10)
# pdf("graphs/satBRF30m_vs_LAIt_bySpecies_TOA_L8.pdf", width = 7.5, height = 5, pointsize = 10)
# pdf("graphs/satBRF60m_vs_LAIt_bySpecies_TOA_L8.pdf", width = 7.5, height = 5, pointsize = 10)
pdf("graphs/satBRF25m_vs_LAIt_bySpecies_BOA_L8_2014_wrongcoords.pdf", width = 7.5, height = 5, pointsize = 10)


with(satBRFvsLAItrue, multiplot(LAItrue.b2, LAItrue.b3, LAItrue.b4, LAItrue.b5, LAItrue.b6, LAItrue.b7, cols = 3))
dev.off()

# satBRFvsLAIeff ---------------------------------------------------------

satBRFvsLAIeff <- list(                                # LAIeff ********************************************
  
  LAIeff.b2 = my_ggplot_ms(temp, "LAIeff", "TOA_B2_blue", "dominant_species",
                            "LAIeff", "Observed BRF blue", xaxis.lim = c(0,5), yaxis.lim = c(0,0.065), 
                            xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.065,by=0.01), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAIeff.b3 = my_ggplot_ms(temp, "LAIeff", "TOA_B3_green", "dominant_species",
                            "LAIeff", "Observed BRF green", xaxis.lim = c(0,5), yaxis.lim = c(0,0.055), 
                            xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.055,by=0.01), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAIeff.b4 = my_ggplot_ms(temp, "LAIeff", "TOA_B4_red", "dominant_species",
                            "LAIeff", "Observed BRF red", xaxis.lim = c(0,5), yaxis.lim = c(0,0.04), 
                            xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.04,by=0.01), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAIeff.b5 = my_ggplot_ms(temp, "LAIeff", "TOA_B5_nir", "dominant_species",
                            "LAIeff", "Observed BRF NIR", xaxis.lim = c(0,5), yaxis.lim = c(0,0.2), 
                            xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.2,by=0.05), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAIeff.b6 = my_ggplot_ms(temp, "LAIeff", "TOA_B6_swir1", "dominant_species",
                            "LAIeff", "Observed BRF SWIR1", xaxis.lim = c(0,5), yaxis.lim = c(0,0.11), 
                            xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.11,by=0.02), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAIeff.b7 = my_ggplot_ms(temp, "LAIeff", "TOA_B7_swir2", "dominant_species",
                            "LAIeff", "Observed BRF SWIR2", xaxis.lim = c(0,5), yaxis.lim = c(0,0.047), 
                            xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.047,by=0.01), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none")
  
)

# pdf("graphs/satBRF60m_vs_LAIe_bySpecies_TOA_L8.pdf", width = 7.5, height = 5, pointsize = 10)
pdf("graphs/satBRF25m_vs_LAIe_bySpecies_TOA_L8.pdf", width = 7.5, height = 5, pointsize = 10)

with(satBRFvsLAIeff, multiplot(LAIeff.b2, LAIeff.b3, LAIeff.b4, LAIeff.b5, LAIeff.b6, LAIeff.b7, cols = 3))
dev.off()
