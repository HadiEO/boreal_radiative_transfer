# Read PARAS BRF ----------------------------------------------------------

paras.BRF.base <- read_csv2("results/paras_BRF_fixedInputs_L8_extended_Nilson.csv")
paras.BRF.onlySunexp <- read_csv2("results/paras_BRF_fixedInputs_L8_extended_Nilson_onlySunExposedLeaves.csv")


paras.BRF.noCurrent <- read_csv2("results/Spruce_simBRF_noCurrentYear_L8_Nilson.csv") 
paras.BRF.onlyCurrent <- read_csv2("results/Spruce_simBRF_onlyCurrentYear_L8_Nilson.csv")


# Plot effect of sun-exposed --------------------------------------------------------------------

axis.range <- list(blue = c(0,0.05), green = c(0,0.1), red = c(0,0.05),
                   nir = c(0,0.6), swir1 = c(0,0.35), swir2 = c(0,0.2))

RMSD.ls <- list(blue = round(RMSE(paras.BRF.base$Blue, paras.BRF.onlySunexp$Blue),3),
                green = round(RMSE(paras.BRF.base$Green, paras.BRF.onlySunexp$Green),3),
                red = round(RMSE(paras.BRF.base$Red, paras.BRF.onlySunexp$Red),3),
                nir = round(RMSE(paras.BRF.base$NIR, paras.BRF.onlySunexp$NIR),3),
                swir1 = round(RMSE(paras.BRF.base$SWIR1, paras.BRF.onlySunexp$SWIR1),3),
                swir2 = round(RMSE(paras.BRF.base$SWIR2, paras.BRF.onlySunexp$SWIR2),3))
x11()
par(mfrow = c(2,3), ps = 14)

plot(paras.BRF.base$Blue, paras.BRF.onlySunexp$Blue,
     xlim = axis.range$blue, ylim = axis.range$blue,
     xlab = "BRF Blue", ylab = "BRF Blue (sun-exposed)")
abline(0,1); text(0.01, 0.04, str_c("RMSD = ", RMSD.ls$blue))

plot(paras.BRF.base$Green, paras.BRF.onlySunexp$Green,
     xlim = axis.range$green, ylim = axis.range$green,
     xlab = "BRF Green", ylab = "BRF Green (sun-exposed)")
abline(0,1); text(0.02, 0.08, str_c("RMSD = ", RMSD.ls$green))

plot(paras.BRF.base$Red, paras.BRF.onlySunexp$Red,
     xlim = axis.range$red, ylim = axis.range$red,
     xlab = "BRF Red", ylab = "BRF Red (sun-exposed)")
abline(0,1); text(0.01, 0.04, str_c("RMSD = ", RMSD.ls$red))

plot(paras.BRF.base$NIR, paras.BRF.onlySunexp$NIR,
     xlim = axis.range$nir, ylim = axis.range$nir,
     xlab = "BRF NIR", ylab = "BRF NIR (sun-exposed)")
abline(0,1); text(0.1, 0.5, str_c("RMSD = ", RMSD.ls$nir))

plot(paras.BRF.base$SWIR1, paras.BRF.onlySunexp$SWIR1,
     xlim = axis.range$swir1, ylim = axis.range$swir1,
     xlab = "BRF SWIR1", ylab = "BRF SWIR1 (sun-exposed)")
abline(0,1); text(0.07, 0.3, str_c("RMSD = ", RMSD.ls$swir1))

plot(paras.BRF.base$SWIR2, paras.BRF.onlySunexp$SWIR2,
     xlim = axis.range$swir2, ylim = axis.range$swir2,
     xlab = "BRF SWIR2", ylab = "BRF SWIR2 (sun-exposed)")
abline(0,1); text(0.04, 0.17, str_c("RMSD = ", RMSD.ls$swir2))

# Plot effect of spruce needle cohort --------------------------------------------------------------------

axis.range <- list(blue = c(0,0.05), green = c(0,0.1), red = c(0,0.05),
                   nir = c(0,0.6), swir1 = c(0,0.35), swir2 = c(0,0.2))

RMSD.ls <- list(blue = round(RMSE(paras.BRF.noCurrent$Blue, paras.BRF.onlyCurrent$Blue),3),
                green = round(RMSE(paras.BRF.noCurrent$Green, paras.BRF.onlyCurrent$Green),3),
                red = round(RMSE(paras.BRF.noCurrent$Red, paras.BRF.onlyCurrent$Red),3),
                nir = round(RMSE(paras.BRF.noCurrent$NIR, paras.BRF.onlyCurrent$NIR),3),
                swir1 = round(RMSE(paras.BRF.noCurrent$SWIR1, paras.BRF.onlyCurrent$SWIR1),3),
                swir2 = round(RMSE(paras.BRF.noCurrent$SWIR2, paras.BRF.onlyCurrent$SWIR2),3))
# x11()
par(mfrow = c(2,3), ps = 14)

plot(paras.BRF.noCurrent$Blue, paras.BRF.onlyCurrent$Blue,
     xlim = axis.range$blue, ylim = axis.range$blue,
     xlab = "BRF Blue (only E1)", ylab = "BRF Blue (only E0)")
abline(0,1); text(0.01, 0.04, str_c("RMSD = ", RMSD.ls$blue))

plot(paras.BRF.noCurrent$Green, paras.BRF.onlyCurrent$Green,
     xlim = axis.range$green, ylim = axis.range$green,
     xlab = "BRF Green (only E1)", ylab = "BRF Green (only E0)")
abline(0,1); text(0.02, 0.08, str_c("RMSD = ", RMSD.ls$green))

plot(paras.BRF.noCurrent$Red, paras.BRF.onlyCurrent$Red,
     xlim = axis.range$red, ylim = axis.range$red,
     xlab = "BRF Red (only E1)", ylab = "BRF Red (only E0)")
abline(0,1); text(0.01, 0.04, str_c("RMSD = ", RMSD.ls$red))

plot(paras.BRF.noCurrent$NIR, paras.BRF.onlyCurrent$NIR,
     xlim = axis.range$nir, ylim = axis.range$nir,
     xlab = "BRF NIR (only E1)", ylab = "BRF NIR (only E0)")
abline(0,1); text(0.1, 0.5, str_c("RMSD = ", RMSD.ls$nir))

plot(paras.BRF.noCurrent$SWIR1, paras.BRF.onlyCurrent$SWIR1,
     xlim = axis.range$swir1, ylim = axis.range$swir1,
     xlab = "BRF SWIR1 (only E1)", ylab = "BRF SWIR1 (only E0)")
abline(0,1); text(0.07, 0.3, str_c("RMSD = ", RMSD.ls$swir1))

plot(paras.BRF.noCurrent$SWIR2, paras.BRF.onlyCurrent$SWIR2,
     xlim = axis.range$swir2, ylim = axis.range$swir2,
     xlab = "BRF SWIR2 (only E1)", ylab = "BRF SWIR2 (only E0)")
abline(0,1); text(0.04, 0.17, str_c("RMSD = ", RMSD.ls$swir2))