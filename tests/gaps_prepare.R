# Read gaps data
gaps <- read_csv2("data/forest/Hyytiala_LAI_2015_LauriFix.csv")


# Prepare gaps data for PARAS simulation ----------------------------------

# Repace NA proportion to 0, ok cause only basal area columns have 0
gaps[is.na(gaps)] <- 0

# Add species Proportion based on basal area
gaps <- gaps %>% mutate(Basal_area_total = Basal_area_pine + Basal_area_spruce + Basal_area_birch) 
gaps <- gaps %>% mutate(Prop_pine = Basal_area_pine / Basal_area_total,
                        Prop_spruce = Basal_area_spruce / Basal_area_total,
                        Prop_birch = Basal_area_birch / Basal_area_total)

# Add dominant species column (> 70% dominant, < 70% mixed)
gaps <- gaps %>% 
  mutate( Dominant_species = if_else(Prop_pine >= 0.7, "Pine", 
                                     if_else(Prop_spruce >= 0.7, "Spruce",
                                             if_else(Prop_birch >= 0.7, "Birch", "Mixed"))))

gaps <- gaps %>%  mutate(Dominant_species = factor(Dominant_species, c("Pine", "Spruce", "Birch", "Mixed")) )

# Add site fertility label
# 1-2 Herb-rich, 3 Mesic, 4-7 Xeric
gaps <- gaps %>% 
  mutate( Site_type_lab = if_else(Site_type %in% c(1,2), "Herb-rich", 
                                     if_else(Site_type == 3, "Mesic",
                                             if_else(Site_type %in% c(4,5,6,7), "Xeric", "Undetermined"))) )

gaps <- gaps %>%  mutate(Site_type_lab = factor(Site_type_lab, c("Herb-rich", "Mesic", "Xeric")))


# Add ECC
gaps <- gaps %>%  mutate(ECC = (1 - gaps1) * 100)

# Add crown ratio = ratio of height of living crown to tree height
gaps <- gaps %>%  mutate(Crown_ratio = (Height_mean - Crown_base_height_mean) / Height_mean)


# Write the processed gaps to disk
write.csv2(gaps, "results/gaps.csv")

# Further process gaps data for Global Sensitivity Analysis ----------------------------------

STAR.pine <- 0.147 # see ref in Majasalmi et al. (2016)
STAR.spruce <- 0.161 # see ref in Majasalmi et al. (2016)
Beta.pine <- 4*STAR.pine
Beta.spruce <- 4*STAR.spruce
Beta.birch <- 1


gaps2 <- gaps %>% 
  mutate(CI.shoot = Beta.pine * Prop_pine + Beta.spruce * Prop_spruce + Beta.birch * Prop_birch)


# Merge with Nilson's model PAI estimate
temp <- read_csv2("results/gaps_and_testNilson.csv") 
# gaps2 <- dplyr::left_join(gaps2, temp, by = c("ID" = "ID"))   # There's a bug? Columns names duplicated with suffix
gaps2$CI.crown <- temp$LAIeff / temp$PAI_estimate


# Write the processed gaps to disk
write.csv2(gaps2, "results/gaps_for_GSA.csv")

