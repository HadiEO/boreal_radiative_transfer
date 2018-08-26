# Case 5 (all data pooled)
sobolSI.case5.VI <- read_rds("results/GSA/sobolSI_case5_allPooled_VI.rds")
sobolSamples.case5.VI <- read_rds("results/GSA/sobolsamp_case5_allPooled_VI.rds")

# Column names
varnames <-  c("LAIe", "i0", "cgf.view", "CI",
               "wL.b2", "wL.b3", "wL.b4", "wL.b5", "wL.b6", "wL.b7", "wL.b8a", "wL.b11", "wL.b12", 
               "Rg.b2", "Rg.b3", "Rg.b4", "Rg.b5", "Rg.b6", "Rg.b7", "Rg.b8a", "Rg.b11", "Rg.b12")

# Rename columns
sobolSI.case5.VI <- sobolSI.case5.VI$Tfct %>%                              
  as_tibble() %>% set_colnames(varnames)

# Label for x-axis
wav.label <- c("NDVI", "SR", "EVI", "IRECI", "S2REP", "TCW", "NBR", "NDMI", "RSR", "TCARI2")
wav <- 1:10

rownames(sobolSI.case5.VI) <- wav.label

# I see, as VI are constructed from multiple bands, its variance is contributed from variance in spectral inputs
# i.e. wL and Rg in several corresponding bands as well!
# So, we sum the SI across bands.
sobolSI.case5.VI <- sobolSI.case5.VI %>% 
  mutate(wL.sum = wL.b2 + wL.b3 + wL.b4 + wL.b5 + wL.b6 + wL.b7 + wL.b8a + wL.b11 + wL.b12,
         Rg.sum = Rg.b2 + Rg.b3 + Rg.b4 + Rg.b5 + Rg.b6 + Rg.b7 + Rg.b8a + Rg.b11 + Rg.b12)


# Plot
pdf("graphs/sobolSI_case5_VI.pdf", width = 3, height = 3, pointsize = 8)

par(mfrow = c(1,1), oma = c(0, 0, 0, 0), mai = c(0.45, 0.35, 0.1, 0.1),
    ps = 8, mgp = c(1.7, 0.5, 0), mar = c(2.7, 2.5, 1.5, 0.5))

temp <- sobolSI.case5.VI * 100
plot(wav, temp$LAIe, type = 'b', col = 'dodgerblue', ylim = c(0, 100), lwd = 1,  xaxt = "n",            #  xaxp = c(400,2400,10),
     main = "All plots", xlab = "Spectral band", ylab = "Contribution (%)")
points(wav, temp$i0, col = 'magenta', lwd = 1, type = "b")
points(wav, temp$cgf.view, col = 'brown', lwd = 1, type = "b")
points(wav, c(temp$wL.sum[1], temp$wL.sum[2], temp$wL.sum[3], temp$wL.sum[4], temp$wL.sum[5], temp$wL.sum[6], temp$wL.sum[7], temp$wL.sum[8], temp$wL.sum[9], temp$wL.sum[10]),           # wL
       col = 'chartreuse4', lwd = 1, lty = 2, type = "b", pch = 2)
points(wav, c(temp$Rg.sum[1], temp$Rg.sum[2], temp$Rg.sum[3], temp$Rg.sum[4], temp$Rg.sum[5], temp$Rg.sum[6], temp$Rg.sum[7], temp$Rg.sum[8], temp$Rg.sum[9], temp$Rg.sum[10]),            # Rg
       col = 'grey40', lwd = 1, lty = 2, type = "b", pch = 2)
points(wav, temp$CI, col = 'dark orange', lwd = 1, type = "b")   # Add CI
axis(1, at = 1:10, labels = wav.label, srt = 45, cex.axis = 0.65)

# Legend
par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
cols <- c("dodgerblue", "magenta", "brown", "dark orange",
          "chartreuse4", "grey40")
legend("topleft", xpd = TRUE, 
       legend = c(expression(LAI[eff]), expression(paste(i[0], " = 1 - ", cgf(theta[sun]), "    ")), expression(paste(cgf(theta[view]), " = ECC")), "CI", expression(omega[L]), expression(rho[g])),
       col = cols, pch = c(rep(1,4), 2, 2),
       lty = c(rep(1, 4), 2, 2),  lwd = rep(1,6), ncol = 2, cex = 1.2, inset = c(0.13,0.05), bty = "n", x.intersp = 0.5) # inset = c(0.13,-0.001)


dev.off()
