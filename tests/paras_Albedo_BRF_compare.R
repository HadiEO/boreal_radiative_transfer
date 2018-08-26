
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

# Read PARAS albedo ---------------------------------------------------------- 

paras.Albedo.S2A <- read_csv2("results/paras_Albedo_fixedInputs_S2A_extended.csv")      # Test scaled albedo
paras.Albedo.L8 <- read_csv2("results/paras_albedo_fixedInputs_L8_extended.csv") 

# Read PARAS BRF ---------------------------------------------------------- 

paras.BRF.S2A <- read_csv2("results/paras_BRF_fixedInputs_S2A_extended.csv") 
paras.BRF.L8 <- read_csv2("results/paras_BRF_fixedInputs_L8_extended.csv") 

# Merge gaps, satellite BRF, and paras BRF ------------------------------------------------

gaps.sat.S2A <- left_join(gaps, S2.BOA.w12.5, by = c("ID" = "ID"))
gaps.sat.L8 <- left_join(gaps, L8.BOA.w12.5, by = c("ID" = "ID"))

gaps.sat.parasBRF.S2A <- left_join(gaps.sat.S2A, paras.BRF.S2A, by = c("ID" = "ID"))
gaps.sat.parasBRF.L8 <- left_join(gaps.sat.L8, paras.BRF.L8, by = c("ID" = "ID"))

# Merge gaps, satellite BRF, and paras Albedo ------------------------------------------------

gaps.sat.parasAlbedo.S2A <- left_join(gaps.sat.S2A, paras.Albedo.S2A, by = c("ID" = "ID"))
gaps.sat.parasAlbedo.L8 <- left_join(gaps.sat.L8, paras.Albedo.L8, by = c("ID" = "ID"))


# Plot --------------------------------------------------------------------

# Sentinel-2A

# Todo: use ggplot! Just need to shape the data

gaps.sat.parasAlbedo.S2A %>% select(Blue:SWIR2) %>% map_dbl(max)

Albedo.range <- list(blue = c(0,0.05), green = c(0,0.1), red = c(0,0.05),
                     re1 = c(0,0.15), re2 = c(0,0.5), re3 = c(0,0.6),
                  nir = c(0,0.6), swir1 = c(0,0.35), swir2 = c(0,0.2))

RMSD.ls <- list(blue = round(RMSE(gaps.sat.parasBRF.S2A$Blue, gaps.sat.parasAlbedo.S2A$Blue),3),
                green = round(RMSE(gaps.sat.parasBRF.S2A$Green, gaps.sat.parasAlbedo.S2A$Green),3),
                red = round(RMSE(gaps.sat.parasBRF.S2A$Red, gaps.sat.parasAlbedo.S2A$Red),3),
                re1 = round(RMSE(gaps.sat.parasBRF.S2A$RE1, gaps.sat.parasAlbedo.S2A$RE1),3),
                re2 = round(RMSE(gaps.sat.parasBRF.S2A$RE2, gaps.sat.parasAlbedo.S2A$RE2),3),
                re3 = round(RMSE(gaps.sat.parasBRF.S2A$RE3, gaps.sat.parasAlbedo.S2A$RE3),3),
                nir = round(RMSE(gaps.sat.parasBRF.S2A$NIRnarrow, gaps.sat.parasAlbedo.S2A$NIRnarrow),3),
                swir1 = round(RMSE(gaps.sat.parasBRF.S2A$SWIR1, gaps.sat.parasAlbedo.S2A$SWIR1),3),
                swir2 = round(RMSE(gaps.sat.parasBRF.S2A$SWIR2, gaps.sat.parasAlbedo.S2A$SWIR2),3))
# x11()
par(mfrow = c(3,3), ps = 14)

plot(gaps.sat.parasBRF.S2A$Blue, gaps.sat.parasAlbedo.S2A$Blue,
     xlim = Albedo.range$blue, ylim = Albedo.range$blue,
     xlab = "Sim. BRF Blue", ylab = "Sim. Albedo Blue")
abline(0,1); text(0.01, 0.04, str_c("RMSD = ", RMSD.ls$blue))

plot(gaps.sat.parasBRF.S2A$Green, gaps.sat.parasAlbedo.S2A$Green,
     xlim = Albedo.range$green, ylim = Albedo.range$green,
     xlab = "Sim. BRF Green", ylab = "Sim. Albedo Green")
abline(0,1); text(0.02, 0.08, str_c("RMSD = ", RMSD.ls$green))

plot(gaps.sat.parasBRF.S2A$Red, gaps.sat.parasAlbedo.S2A$Red,
     xlim = Albedo.range$red, ylim = Albedo.range$red,
     xlab = "Sim. BRF Red", ylab = "Sim. Albedo Red")
abline(0,1); text(0.01, 0.04, str_c("RMSD = ", RMSD.ls$red))

plot(gaps.sat.parasBRF.S2A$RE1, gaps.sat.parasAlbedo.S2A$RE1,
     xlim = Albedo.range$re1, ylim = Albedo.range$re1,
     xlab = "Sim. BRF RE1", ylab = "Sim. Albedo RE1")
abline(0,1); text(0.03, 0.12, str_c("RMSD =  ", RMSD.ls$re1))

plot(gaps.sat.parasBRF.S2A$RE2, gaps.sat.parasAlbedo.S2A$RE2,
     xlim = Albedo.range$re2, ylim = Albedo.range$re2,
     xlab = "Sim. BRF RE2", ylab = "Sim. Albedo RE2")
abline(0,1); text(0.1, 0.4, str_c("RMSD = ", RMSD.ls$re2))

plot(gaps.sat.parasBRF.S2A$RE3, gaps.sat.parasAlbedo.S2A$RE3,
     xlim = Albedo.range$re3, ylim = Albedo.range$re3,
     xlab = "Sim. BRF RE3", ylab = "Sim. Albedo RE3")
abline(0,1); text(0.15, 0.5, str_c("RMSD = ", RMSD.ls$re3))

plot(gaps.sat.parasBRF.S2A$NIRnarrow, gaps.sat.parasAlbedo.S2A$NIRnarrow,
     xlim = Albedo.range$nir, ylim = Albedo.range$nir,
     xlab = "Sim. BRF NIRnarrow", ylab = "Sim. Albedo NIRnarrow")
abline(0,1); text(0.1, 0.5, str_c("RMSD = ", RMSD.ls$nir))

plot(gaps.sat.parasBRF.S2A$SWIR1, gaps.sat.parasAlbedo.S2A$SWIR1,
     xlim = Albedo.range$swir1, ylim = Albedo.range$swir1,
     xlab = "Sim. BRF SWIR1", ylab = "Sim. Albedo SWIR1")
abline(0,1); text(0.07, 0.3, str_c("RMSD = ", RMSD.ls$swir1))

plot(gaps.sat.parasBRF.S2A$SWIR2, gaps.sat.parasAlbedo.S2A$SWIR2,
     xlim = Albedo.range$swir2, ylim = Albedo.range$swir2,
     xlab = "Sim. BRF SWIR2", ylab = "Sim. Albedo SWIR2")
abline(0,1); text(0.04, 0.17, str_c("RMSD = ", RMSD.ls$swir2))

