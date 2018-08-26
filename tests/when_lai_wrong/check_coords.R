lai.lauri <- read_csv2("data/forest/LAI_fromLauri.csv")                         # LAI2000-09

gaps.join.lai.lauri <- inner_join(gaps, lai.lauri, by = c("ID" = "ValeriID")) 

temp <- gaps.join.lai.lauri
write.csv2(temp, "data/forest/gaps_join_laiLauri.csv")

temp %>% dplyr::select(ID)

c <- plot(temp$LAIeff, temp$LAI2000, xlim = c(0,6), ylim = c(0,6), xlab = "LAIeff Hadi", ylab = "LAIeff Lauri"); abline(0,1)
# LAI differs alot!
plot(temp$gaps1, temp$Gaps1, xlim = c(0,1), ylim = c(0,1), xlab = "gaps1 Hadi", ylab = "gaps1 Lauri"); abline(0,1)
plot(temp$gaps2, temp$Gaps2, xlim = c(0,1), ylim = c(0,1), xlab = "gaps2 Hadi", ylab = "gaps2 Lauri"); abline(0,1)
plot(temp$gaps3, temp$Gaps3, xlim = c(0,1), ylim = c(0,1), xlab = "gaps3 Hadi", ylab = "gaps3 Lauri"); abline(0,1)
plot(temp$gaps4, temp$Gaps4, xlim = c(0,1), ylim = c(0,1), xlab = "gaps4 Hadi", ylab = "gaps4 Lauri"); abline(0,1)
plot(temp$gaps5, temp$Gaps5, xlim = c(0,1), ylim = c(0,1), xlab = "gaps5 Hadi", ylab = "gaps5 Lauri"); abline(0,1)



a <- plot(temp$WGSE.x, temp$WGSE.y, xlab = "WGSE Hadi", ylab = "WGSE Lauri"); abline(0,1)
b <- plot(temp$WGSN.x, temp$WGSN.y, xlab = "WGSN Hadi", ylab = "WGSN Lauri"); abline(0,1)
# So WGS coords are the same

d <- plot(temp$prop_pine, temp$`Pine%`, xlab = "%Pine Hadi", ylab = "%Pine Lauri"); abline(0,1)
e <- plot(temp$prop_spruce, temp$`Spruce%`, xlab = "%Spruce Hadi", ylab = "%Spruce Lauri"); abline(0,1)
f <- plot(temp$prop_birch, temp$`Decid%`, xlab = "%Birch Hadi", ylab = "%Birch Lauri"); abline(0,1)
# Proportion also differ for some plots

g <- plot(temp$Site_type, temp$site, xlab = "Site Hadi", ylab = "Site Lauri"); abline(0,1)



# Plotting
pdf("graphs/check_lai.pdf", width = 9, height = 6, pointsize = 10)

par(mfrow = c(2,4))
plot(temp$WGSE.x, temp$WGSE.y, xlab = "WGSE Hadi", ylab = "WGSE Lauri"); abline(0,1)
plot(temp$WGSN.x, temp$WGSN.y, xlab = "WGSN Hadi", ylab = "WGSN Lauri"); abline(0,1)
plot(temp$LAIeff, temp$LAI2000, xlim = c(0,6), ylim = c(0,6), xlab = "LAIeff Hadi", ylab = "LAIeff Lauri"); abline(0,1)
plot(temp$prop_pine, temp$`Pine%`, xlab = "%Pine Hadi", ylab = "%Pine Lauri"); abline(0,1)
plot(temp$prop_spruce, temp$`Spruce%`, xlab = "%Spruce Hadi", ylab = "%Spruce Lauri"); abline(0,1)
plot(temp$prop_birch, temp$`Decid%`, xlab = "%Birch Hadi", ylab = "%Birch Lauri"); abline(0,1)
plot(temp$Site_type, temp$site, xlab = "Site Hadi", ylab = "Site Lauri"); abline(0,1)
dev.off()





# Site is exactly same

# Check LAI in original excel
temp %>%  dplyr::select(ID, LAIeff) %>%  View
# Correct

# Check redownload excel from Miina
# OK!

# Check last year's image



# Try plot Sentinel-2 spectra vs Lauri's LAI
spc <- read_csv2("results/BOA_S2_sq25m.csv")
temp2 <- left_join(spc, lai.lauri, by = c("ID" = "ValeriID"))

# Add dominant species column (> 70% dominant, < 70% mixed)
temp2 <- temp2 %>% 
  mutate( dominant_species = if_else(.$`Pine%` >= 0.7, "Pine", 
                                     if_else(.$`Spruce%` >= 0.7, "Spruce",
                                             if_else(.$`Decid%` >= 0.7, "Birch", "Mixed"))) )

# satBRFvsLAI2000 ----------------------------------------------------------
satBRFvsLAI2000 <- list(                                # LAI2000 ********************************************
  
  LAI2000.b2 = my_ggplot_ms(temp2, "LAI2000", "B2_490", "dominant_species",
                           "LAI2000", "Observed BRF blue", xaxis.lim = c(0,5), yaxis.lim = c(0,0.045), 
                           xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.05,by=0.01), 
                           shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAI2000.b3 = my_ggplot_ms(temp2, "LAI2000", "B3_560", "dominant_species",
                           "LAI2000", "Observed BRF green", xaxis.lim = c(0,5), yaxis.lim = c(0,0.1), 
                           xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.1,by=0.02), 
                           shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAI2000.b4 = my_ggplot_ms(temp2, "LAI2000", "B4_665", "dominant_species",
                           "LAI2000", "Observed BRF red", xaxis.lim = c(0,5), yaxis.lim = c(0,0.05), 
                           xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.05,by=0.01), 
                           shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAI2000.b5 = my_ggplot_ms(temp2, "LAI2000", "B5_705", "dominant_species",
                           "LAI2000", "Observed BRF RE1", xaxis.lim = c(0,5), yaxis.lim = c(0,0.14), 
                           xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.14,by=0.02), 
                           shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAI2000.b6 = my_ggplot_ms(temp2, "LAI2000", "B6_740", "dominant_species",
                           "LAI2000", "Observed BRF RE2", xaxis.lim = c(0,5), yaxis.lim = c(0,0.5), 
                           xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.5,by=0.05), 
                           shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAI2000.b7 = my_ggplot_ms(temp2, "LAI2000", "B7_783", "dominant_species",
                           "LAI2000", "Observed BRF RE3", xaxis.lim = c(0,5), yaxis.lim = c(0,0.5), 
                           xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.5,by=0.05), 
                           shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  # LAI2000.b8 = my_ggplot_ms(temp2, "LAI2000", "B8_842", "dominant_species",
  #                       "LAI2000", "Observed BRF NIR", xaxis.lim = c(0,5), yaxis.lim = c(0,0.5), 
  #                       xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.5,by=0.05), 
  #                       shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAI2000.b8a = my_ggplot_ms(temp2, "LAI2000", "B8a_865", "dominant_species",
                            "LAI2000", "Observed BRF NIRn", xaxis.lim = c(0,5), yaxis.lim = c(0,0.5), 
                            xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.5,by=0.05), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAI2000.b11 = my_ggplot_ms(temp2, "LAI2000", "B11_1610", "dominant_species",
                            "LAI2000", "Observed BRF SWIR1", xaxis.lim = c(0,5), yaxis.lim = c(0,0.28), 
                            xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.28,by=0.05), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAI2000.b12 = my_ggplot_ms(temp2, "LAI2000", "B12_2190", "dominant_species",
                            "LAI2000", "Observed BRF SWIR2", xaxis.lim = c(0,5), yaxis.lim = c(0,0.17), 
                            xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.17,by=0.02), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none")
  
)


pdf("graphs/satBRF25m_vs_LAIeLauri_BOA.pdf", width = 7.5, height = 7.5, pointsize = 10)
with(satBRFvsLAI2000, multiplot(LAI2000.b2, LAI2000.b3, LAI2000.b4, LAI2000.b5, LAI2000.b6, LAI2000.b7, LAI2000.b8a, LAI2000.b11, LAI2000.b12, cols = 3))
dev.off()



# Check field comments ----------------------------------------------------

comments <- read_csv2("data/forest/titta_field_comments.csv")



# LAIeff vs sat BRF, with comments --------------------------------------
gaps <- read_csv2("results/gaps.csv") %>%  dplyr::select(-X1)

gaps <- gaps %>%  mutate(ECC = (1 - gaps1) * 100,                                                  # Data prepared for plotting ***
                         Site_type_lab = factor(Site_type_lab, c("Herb-rich", "Mesic", "Xeric")),
                         dominant_species = factor(dominant_species, c("Pine", "Spruce", "Birch", "Mixed")))

spc.BOA.25m <- read_csv2("results/BOA_S2_sq25m.csv")          # BOA
gaps.spc.BOA.25m <- left_join(gaps, spc.BOA.25m)            # BOA

temp4 <- left_join(gaps.spc.BOA.25m, comments, by = c("ID" = "ID"))


temp4.plot <- list(                               
  
  LAIeff.b2 = my_ggplot_ms(temp4, "LAIeff", "B2_490", "Comments",
                            "LAIeff", "Observed BRF blue", xaxis.lim = c(0,5), yaxis.lim = c(0,0.045), 
                            xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.05,by=0.01), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAIeff.b3 = my_ggplot_ms(temp4, "LAIeff", "B3_560", "Comments",
                            "LAIeff", "Observed BRF green", xaxis.lim = c(0,5), yaxis.lim = c(0,0.1), 
                            xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.1,by=0.02), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAIeff.b4 = my_ggplot_ms(temp4, "LAIeff", "B4_665", "Comments",
                            "LAIeff", "Observed BRF red", xaxis.lim = c(0,5), yaxis.lim = c(0,0.05), 
                            xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.05,by=0.01), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAIeff.b5 = my_ggplot_ms(temp4, "LAIeff", "B5_705", "Comments",
                            "LAIeff", "Observed BRF RE1", xaxis.lim = c(0,5), yaxis.lim = c(0,0.14), 
                            xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.14,by=0.02), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAIeff.b6 = my_ggplot_ms(temp4, "LAIeff", "B6_740", "Comments",
                            "LAIeff", "Observed BRF RE2", xaxis.lim = c(0,5), yaxis.lim = c(0,0.5), 
                            xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.5,by=0.05), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAIeff.b7 = my_ggplot_ms(temp4, "LAIeff", "B7_783", "Comments",
                            "LAIeff", "Observed BRF RE3", xaxis.lim = c(0,5), yaxis.lim = c(0,0.5), 
                            xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.5,by=0.05), 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  # LAIeff.b8 = my_ggplot_ms(temp4, "LAIeff", "B8_842", "Comments",
  #                       "LAIeff", "Observed BRF NIR", xaxis.lim = c(0,5), yaxis.lim = c(0,0.5), 
  #                       xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.5,by=0.05), 
  #                       shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAIeff.b8a = my_ggplot_ms(temp4, "LAIeff", "B8a_865", "Comments",
                             "LAIeff", "Observed BRF NIRn", xaxis.lim = c(0,5), yaxis.lim = c(0,0.5), 
                             xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.5,by=0.05), 
                             shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAIeff.b11 = my_ggplot_ms(temp4, "LAIeff", "B11_1610", "Comments",
                             "LAIeff", "Observed BRF SWIR1", xaxis.lim = c(0,5), yaxis.lim = c(0,0.28), 
                             xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.28,by=0.05), 
                             shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAIeff.b12 = my_ggplot_ms(temp4, "LAIeff", "B12_2190", "Comments",
                             "LAIeff", "Observed BRF SWIR2", xaxis.lim = c(0,5), yaxis.lim = c(0,0.17), 
                             xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.17,by=0.02), 
                             shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none")
  
)


pdf("graphs/satBRF25m_vs_LAIeff_BOA_comments.pdf", width = 7.5, height = 7.5, pointsize = 10)
with(temp4.plot, multiplot(LAIeff.b2, LAIeff.b3, LAIeff.b4, LAIeff.b5, LAIeff.b6, LAIeff.b7, LAIeff.b8a, LAIeff.b11, LAIeff.b12, cols = 3))
dev.off()

# LAIeff Hadi vs Lauri, with comments -------------------------------------
temp5 <- left_join(temp4, lai.lauri, by = c("ID" = "ValeriID"))

temp5 %>%  dplyr::select(ID, LAIeff, LAI2000, cut_after_2010, Comments) %>% View

temp6 <- temp5 %>%  dplyr::select(ID, LAIeff, LAI2000, cut_after_2010, Comments)


pdf("graphs/checklai_fieldcomments.pdf", width = 5, height = 3, pointsize = 8)
ggplot(temp6, aes(LAIeff, LAI2000, col = Comments, shape = cut_after_2010)) + geom_point() +
  geom_abline(slope = 1, intercept = 0) + scale_x_continuous(name = "LAIeff Hadi", limits = c(0,6)) + scale_y_continuous(name = "LAIeff Lauri", limits = c(0,6)) + 
  theme_bw(base_size = 8) + # base_family
  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.text=element_text(size=8), axis.title.x=element_text(size=10,face="bold"), # size = 7; size = 8
        axis.title.y=element_text(size=10,face="bold"), # size = 8
        aspect.ratio=1, legend.text=element_text(size=8), legend.title=element_text(size=7), # size = 6; legend.text=element_text(size=8)
        legend.background=element_rect(fill="transparent"),
        legend.position="right")
  
dev.off()



temp7 <- left_join(temp6, temp4)

pdf("graphs/satBRFvsLAIeff_fieldcomments_swir.pdf", width = 5, height = 3, pointsize = 8)

ggplot(temp7, aes(LAIeff, B11_1610, col = Comments, shape = cut_after_2010)) + geom_point() +
  scale_x_continuous(name = "LAIeff 2015", limits = c(0,6)) + scale_y_continuous(name = "BRF SWIR1", limits = c(0,0.28)) + 
  theme_bw(base_size = 8) + # base_family
  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.text=element_text(size=8), axis.title.x=element_text(size=10,face="bold"), # size = 7; size = 8
        axis.title.y=element_text(size=10,face="bold"), # size = 8
        aspect.ratio=1, legend.text=element_text(size=8), legend.title=element_text(size=7), # size = 6; legend.text=element_text(size=8)
        legend.background=element_rect(fill="transparent"),
        legend.position="right")

dev.off()





