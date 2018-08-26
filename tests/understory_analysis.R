# Read understory data -----------------------------------------
ustory.R <- read_csv2("data/understory/understory_Refl.csv")
# ! ustory.R differ for each plot

# Read understory type ----------------------------------------------------

ustory.type <- read_csv2("data/understory/understory_transects.csv")    
ustory.sites <- read_csv2("data/understory/understory_sites.csv")


# Resample spectra --------------------------------------------------------

wav.hypers.ustory <- ustory.R$Wl   

ustory.R.S2 <- ustory.R %>%  dplyr::select(-Wl) %>%    
  map(spc_resample, wav = wav.hypers.ustory, sensor = "Sentinel2")

# ustory.R.S2.df.temp <- ustory.R.S2.df
ustory.R.S2.df <- ldply(ustory.R.S2) %>% 
  dplyr::select(-c(b1, b9, b10)) %>% # Remove non-land wavelengths (60-m b1, b9, b10)
  left_join(ustory.type, by = c(".id" = "Transect_id")) # Add the site fertility 

# Add column of band center wavelength, add after melt
wav.S2 <- read_csv2("data/image_data/wav_S2.csv")


# Plot understory by transect ---------------------------------------------

require(reshape)
myPlot.Rg <- function(df, legend.position = c(0.85,0.85), pt.shp = 1, text.label) {
  
    melt(df, id.vars = c(".id", "Site_fertility_code", "Site_fertility_class", "Site_id"), variable_name = "band") %>% left_join(wav.S2, by = "band") %>% 
    mutate(Site_fertility_class = factor(Site_fertility_class, level = c("HerbRich", "Mesic", "Xeric"))) %>% 
    ggplot(aes(x = wav_center, y = value, group = .id, col = Site_id)) + geom_line(size = 0.3) + geom_point() + 
    scale_x_continuous(name = "Wavelength (nm)", limits = c(400,2300), breaks = seq(400,2300,200), expand = c(0,0)) +
    scale_y_continuous(name = "Understory reflectance", limits = c(0,0.6), breaks = seq(0,0.6,0.1), expand = c(0,0)) +
    geom_text(x = 1300, y = 0.05, label = text.label, cex = 3, col = "black") +
    theme_bw(base_size = 8) +
    theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
          axis.text = element_text(size = 7), axis.title.x = element_text(size = 8, face = "bold"), axis.title.y = element_text(size = 8, face = "bold"),
          legend.text = element_text(size = 7), legend.title = element_text(size = 7),
          legend.background = element_rect(fill = "transparent"),
          legend.position = legend.position) + guides(colour = guide_legend(override.aes = list(size = 1))) +
   scale_color_manual(name = "", values = c("black", "grey70")) 
  
}  

temp1 <- dplyr::filter(ustory.R.S2.df, Site_fertility_class == "HerbRich") %>%  myPlot.Rg(text.label = "Herb-rich")
temp2 <- dplyr::filter(ustory.R.S2.df, Site_fertility_class == "Mesic") %>%  myPlot.Rg(text.label = "Mesic")
temp3 <- dplyr::filter(ustory.R.S2.df, Site_fertility_class == "Xeric") %>%  myPlot.Rg(text.label = "Xeric")


pdf("graphs/Rg_meanByTransect_3Panel.pdf", width = 7.5, height = 2.5, pointsize = 10)
multiplot(temp1, temp2, temp3, cols = 3)
dev.off()


# Understory by Site (mean of transects) ----------------------------------
ustory.R.S2.df.bySite <- ddply(ustory.R.S2.df, .(Site_id), numcolwise(mean)) %>% left_join(ustory.sites)
# ustory.R.S2.df %>% dplyr::filter(Site_id == "H3") %>% dplyr::select(b2:b12) %>%  colMeans() # Checked OK

myPlot.Rg.bySites <- function(df, legend.position = c(0.85,0.85), pt.shp = 1) {
  
  melt(df, id.vars = c("Site_id", "Site_fertility_code", "Site_fertility_class"), variable_name = "band") %>% left_join(wav.S2, by = "band") %>% 
    mutate(Site_fertility_class = factor(Site_fertility_class, level = c("HerbRich", "Mesic", "Xeric"))) %>% 
    ggplot(aes(x = wav_center, y = value, group = Site_id, col = Site_fertility_class, shape = Site_fertility_class)) + geom_line(size = 0.3) + geom_point() + 
    scale_x_continuous(name = "Wavelength (nm)", limits = c(400,2300), breaks = seq(400,2300,200), expand = c(0,0)) +
    scale_y_continuous(name = "Understory reflectance", limits = c(0,0.6), breaks = seq(0,0.6,0.1), expand = c(0,0)) +
    theme_bw(base_size = 8) +
    theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
          axis.text = element_text(size = 7), axis.title.x = element_text(size = 8, face = "bold"), axis.title.y = element_text(size = 8, face = "bold"),
          legend.text = element_text(size = 7), legend.title = element_text(size = 7),
          legend.background = element_rect(fill = "transparent"),
          legend.position = legend.position) + guides(colour = guide_legend(override.aes = list(size = 1))) +
    scale_shape_manual(name = "", values = c(1,0,2)) + scale_color_manual(name = "", values = c("blue", "magenta", "green")) 
  
}



# Understory by site fertility --------------------------------------------

# ustory.R.3cl <- tibble(
#   wav = ustory.R$Wl,
#   Herb_rich = apply(cbind(ustory.R$A1a_AVG, ustory.R$A1b_AVG, ustory.R$H3a_AVG, ustory.R$H3b_AVG, ustory.R$H3c_AVG, ustory.R$H3d_AVG), 1, mean),
#   Mesic = apply(cbind(ustory.R$A2a_AVG, ustory.R$A2b_AVG, ustory.R$A2c_AVG, ustory.R$U26a_AVG, ustory.R$U26b_AVG), 1, mean),
#   Xeric = apply(cbind(ustory.R$A3a_AVG, ustory.R$A3b_AVG), 1, mean)
#  )
# This is not the same as mean by 3 classes calculated in excel, cause round-up? Yes, should be first average in a site, then average the sites
# For now just use ready .csv
ustory.R.3Class <- read_csv2("data/understory/understory_Refl_3class.csv")

# OK RESAMPLE TO SENTINEL-2, SO SAVE COLUMNS Rg.b1, Rg.b2, ... , Rg.b13
source("munge/spc_resample.R")                              # Resample wL to intended sensor
wav <- ustory.R.3Class$Wl
Rg.HerbRich.S2 <- spc_resample(ustory.R.3Class$Herb_rich, wav, "Sentinel2") # Named Rg for short
Rg.Mesic.S2 <- spc_resample(ustory.R.3Class$Mesic, wav, "Sentinel2")
Rg.Xeric.S2 <- spc_resample(ustory.R.3Class$Xeric, wav, "Sentinel2")   

( Rg.3Class.S2 <- bind_rows(Rg.HerbRich.S2, Rg.Mesic.S2, Rg.Xeric.S2) %>% 
  mutate(class = c("HerbRich", "Mesic", "Xeric")) )

# Remove non-land wavelengths (60-m b1, b9, b10)
Rg.3Class.S2 <- dplyr::select(Rg.3Class.S2, -c(b1, b9, b10))

write.csv2(Rg.3Class.S2, "data/understory/Rg_3Class_S2.csv")

myPlot.Rg.3Class <- function(df, legend.position = c(0.8,0.8), pt.shp = 1) {
  
  melt(df, id.vars = "class", variable_name = "band") %>% left_join(wav.S2, by = "band") %>% 
    mutate(class = factor(class, level = c("HerbRich", "Mesic", "Xeric"))) %>% 
    ggplot(aes(x = wav_center, y = value, col = class, shape = class)) + geom_line(size = 0.3) + geom_point() + 
    scale_x_continuous(name = "Wavelength (nm)", limits = c(400,2300), breaks = seq(400,2300,200), expand = c(0,0)) +
    scale_y_continuous(name = "Understory reflectance", limits = c(0,0.6), breaks = seq(0,0.6,0.1), expand = c(0,0)) +
    theme_bw(base_size = 8) +
    theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
          axis.text = element_text(size = 7), axis.title.x = element_text(size = 8, face = "bold"), axis.title.y = element_text(size = 8, face = "bold"),
          legend.text = element_text(size = 7), legend.title = element_text(size = 7),
          legend.background = element_rect(fill = "transparent"),
          legend.position = legend.position) + guides(colour = guide_legend(override.aes = list(size = 1))) +
    scale_shape_manual(name = "", values = c(1,0,2)) + scale_color_manual(name = "", values = c("blue", "magenta", "green")) 
  
}

source("tests/multiplot.R")                                                                  # Source multiplot function

pdf("graphs/Rg_meanBySite_Class.pdf", width = 7.5, height = 3, pointsize = 10)

temp1 <- myPlot.Rg.bySites(ustory.R.S2.df.bySite)
temp2 <- myPlot.Rg.3Class(Rg.3Class.S2, legend.position = c(0.85,0.85))
multiplot(temp1, temp2, cols = 2)

dev.off()






