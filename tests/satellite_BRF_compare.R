S2.BOA.w12.5 <- read_csv2("data/image_extracted_spectra/S2_BOA_w12_5.csv")
L8.BOA.w12.5 <- read_csv2("data/image_extracted_spectra/L8_BOA_w12_5.csv")

# Doesn't exist yet
# S2.TOA.w12.5 <- read_csv2("data/image_extracted_spectra/S2_TOA_w12_5.csv")
# L8.TOA.w12.5 <- read_csv2("data/image_extracted_spectra/L8_TOA_w12_5.csv")

# Rename bands to "colour"
names(S2.BOA.w12.5) <- c("X1", "ID",
                         "Sat.Blue", "Sat.Green", "Sat.Red", "Sat.RE1", "Sat.RE2", "Sat.RE3",
                         "Sat.NIRnarrow", "Sat.SWIR1", "Sat.SWIR2")

names(L8.BOA.w12.5) <- c("X1", "ID",
                         "Sat.Blue", "Sat.Green", "Sat.Red", 
                         "Sat.NIR", "Sat.SWIR1", "Sat.SWIR2")


BRF.range <- list(blue = c(0,0.03), green = c(0,0.06), red = c(0,0.04),
                  nir = c(0,0.4), swir1 = c(0,0.2), swir2 = c(0,0.1))

RMSD.ls <- list(blue = round(RMSE(L8.BOA.w12.5$Sat.Blue, S2.BOA.w12.5$Sat.Blue),3),
                green = round(RMSE(L8.BOA.w12.5$Sat.Green, S2.BOA.w12.5$Sat.Green),3),
                red = round(RMSE(L8.BOA.w12.5$Sat.Red, S2.BOA.w12.5$Sat.Red),3),
                nir = round(RMSE(L8.BOA.w12.5$Sat.NIR, S2.BOA.w12.5$Sat.NIRnarrow),3),
                swir1 = round(RMSE(L8.BOA.w12.5$Sat.SWIR1, S2.BOA.w12.5$Sat.SWIR1),3),
                swir2 = round(RMSE(L8.BOA.w12.5$Sat.SWIR2, S2.BOA.w12.5$Sat.SWIR2),3))


# pdf("graphs/satBRF_L8vsS2A_boa.pdf", width = 7.5, height = 7.5, pointsize = 10)
# par(mfrow = c(2,3), oma = c(1,1,1,1), mai = c(2, 2, 0.1, 0.1),
#     ps = 12, mgp = c(2.5, 0.7, 0), mar =c(3, 2, 2, 1))
x11()
par(mfrow = c(2,3), ps = 14)

plot(L8.BOA.w12.5$Sat.Blue, S2.BOA.w12.5$Sat.Blue, 
     xlim = BRF.range$blue, ylim = BRF.range$blue,
     xlab = "L8 Blue", ylab = "S2 Blue")
abline(0,1); text(0.004, 0.025, str_c("RMSD = ", RMSD.ls$blue))

plot(L8.BOA.w12.5$Sat.Green, S2.BOA.w12.5$Sat.Green, 
     xlim = BRF.range$green, ylim = BRF.range$green,
     xlab = "L8 Green", ylab = "S2 Green")
abline(0,1); text(0.01, 0.05, str_c("RMSD = ", RMSD.ls$green))

plot(L8.BOA.w12.5$Sat.Red, S2.BOA.w12.5$Sat.Red, 
     xlim = BRF.range$red, ylim = BRF.range$red,
     xlab = "L8 Red", ylab = "S2 Red")
abline(0,1); text(0.01, 0.035, str_c("RMSD = ", RMSD.ls$red))

plot(L8.BOA.w12.5$Sat.NIR, S2.BOA.w12.5$Sat.NIRnarrow, 
     xlim = BRF.range$nir, ylim = BRF.range$nir,
     xlab = "L8 NIR", ylab = "S2 NIR")
abline(0,1); text(0.05, 0.35, str_c("RMSD = ", RMSD.ls$nir))

plot(L8.BOA.w12.5$Sat.SWIR1, S2.BOA.w12.5$Sat.SWIR1, 
     xlim = BRF.range$swir1, ylim = BRF.range$swir1,
     xlab = "L8 SWIR1", ylab = "S2 SWIR1")
abline(0,1); text(0.03, 0.17, str_c("RMSD = ", RMSD.ls$swir1))

plot(L8.BOA.w12.5$Sat.SWIR2, S2.BOA.w12.5$Sat.SWIR2, 
     xlim = BRF.range$swir2, ylim = BRF.range$swir2,
     xlab = "L8 SWIR2", ylab = "S2 SWIR2")
abline(0,1); text(0.015, 0.09, str_c("RMSD = ", RMSD.ls$swir2))

# dev.off()







