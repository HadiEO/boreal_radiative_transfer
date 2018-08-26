
# Plot LAIe vs LAIt -------------------------------------------------------
pdf("graphs/LAIe_vs_LAIt.pdf", width = 3.5, height = 3.5, pointsize = 10)
my_ggplot_1to1_group_ms(gaps, "LAIeff", "LAItrue", group = "dominant_species", "LAIeff", "LAItrue", 
                        xaxis.lim = c(0,6), yaxis.lim = c(0,6),  xaxis.break = seq(0,6,by=1), yaxis.break = seq(0,6,by=1),
                        shape.pch = pts.pchs, shape.col = pts.cols)
dev.off()



# Range of measured forest structure -------------------------------------------------------------------
names(gaps)
# gaps %>% group_by(dominant_species == "Pine") %>% summarise(DBH_mean = mean(DBH_mean),
#                                                             Height_mean = mean(Height_mean),
#                                                             Basal_area_sum = mean(Basal_area_sum))

gaps %>% group_by(dominant_species) %>% summarise(LAIe.mean = round(mean(LAIeff), 2),
                                                  LAIe.min = round(min(LAIeff), 2), 
                                                  LAIe.max = round(max(LAIeff), 2))

gaps %>% group_by(dominant_species) %>% summarise(LAIt.mean = round(mean(LAItrue), 2),
                                                  LAIt.min = round(min(LAItrue), 2), 
                                                  LAIt.max = round(max(LAItrue), 2))
round(mean(gaps$LAItrue), 2)
round(min(gaps$LAItrue), 2) 
round(max(gaps$LAItrue), 2)

round(mean(gaps$LAIeff), 2)
round(min(gaps$LAIeff), 2) 
round(max(gaps$LAIeff), 2)


# No. of site type by species ---------------------------------------------

gaps %>% filter(dominant_species == "Pine") %>% select(Site_type_lab) %>% table
gaps %>% filter(dominant_species == "Spruce") %>% select(Site_type_lab) %>% table
gaps %>% filter(dominant_species == "Birch") %>% select(Site_type_lab) %>% table
gaps %>% filter(dominant_species == "Mixed") %>% select(Site_type_lab) %>% table
gaps %>% select(Site_type_lab) %>% table







