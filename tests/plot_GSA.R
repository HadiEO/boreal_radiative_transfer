# Case 1
sobolSI.case1.PINE <- readRDS("results/GSA/sobolSI_case1_PINE.rds")
sobolSI.case1.SPRUCE <- readRDS("results/GSA/sobolSI_case1_SPRUCE.rds")
sobolSI.case1.BIRCH <- readRDS("results/GSA/sobolSI_case1_BIRCH.rds")

sobolSI.case1.PINEnSPRUCE <- readRDS("results/GSA/sobolSI_case1_PINEnSPRUCE.rds")

# Case 0
sobolSI.case0.LAIt.1.6to2 <- readRDS("results/GSA/sobolSI_case0_LAItless2.rds")
sobolSI.case0.LAIt.1.6to3 <- readRDS("results/GSA/sobolSI_case0_LAItless3.rds")
sobolSI.case0.LAIt.3to4 <- readRDS("results/GSA/sobolSI_case0_LAIt3to4.rds")
sobolSI.case0.LAIt.4to5 <- readRDS("results/GSA/sobolSI_case0_LAIt4to5.rds")
sobolSI.case0.LAIt.5to6 <- readRDS("results/GSA/sobolSI_case0_LAIt5to6.rds")
sobolSI.case0.LAIt.6to7.6 <- readRDS("results/GSA/sobolSI_case0_LAItmore6.rds")

# Case 3a
sobolSI.case3a.PINE <- readRDS("results/GSA/sobolSI_case3a_PINE.rds")
sobolSI.case3a.SPRUCE <- readRDS("results/GSA/sobolSI_case3a_SPRUCE.rds")
sobolSI.case3a.BIRCH <- readRDS("results/GSA/sobolSI_case3a_BIRCH.rds")

# Case 4
sobolSI.case4.LAIt.1.6to2 <- readRDS("results/GSA/sobolSI_case4_LAItless2.rds")
sobolSI.case4.LAIt.1.6to3 <- readRDS("results/GSA/sobolSI_case4_LAItless3.rds")
sobolSI.case4.LAIt.3to4 <- readRDS("results/GSA/sobolSI_case4_LAIt3to4.rds")
sobolSI.case4.LAIt.4to5 <- readRDS("results/GSA/sobolSI_case4_LAIt4to5.rds")
sobolSI.case4.LAIt.5to6 <- readRDS("results/GSA/sobolSI_case4_LAIt5to6.rds")
sobolSI.case4.LAIt.6to7.6 <- readRDS("results/GSA/sobolSI_case4_LAItmore6.rds")

sobolSI.case4.LAIt.4to5.inclHerbRich <- readRDS("results/GSA/sobolSI_case4_LAIt4to5_inclHerbRich.rds")


# Case 5 (all data pooled)
sobolSI.case5 <- readRDS("results/GSA/sobolSI_case5_allPooled.rds")


# Case 6 (otaniemi2016 leaf albedo, separately for conifer and broadleaf)
sobolSI.case6.CONIFER <- readRDS("results/GSA/sobolSI_case6_CONIFER.rds")
sobolSI.case6.BROADLEAF <- readRDS("results/GSA/sobolSI_case6_BROADLEAF.rds")
sobolSI.case6.CONIFERNoLarx <- readRDS("results/GSA/sobolSI_case6_CONIFERNoLarx.rds")

# Case 7 (dark ground = 0.0001)
sobolSI.case7 <- readRDS("results/GSA/sobolSI_case7_darkGround.rds")

# case 5 (all data pooled, ground term combined)
sobolSI.case5.groundTerm <- read_rds("results/GSA/sobolSI_case5_allPooled_groundTermCombined.rds")

# Case 5, rescale output BRF with MD of pooled data
sobolSI.case5.scaled <- read_rds("results/GSA/sobolSI_case5_allPooled_scaledByPooledMD.rds")

# Case 8, intraspecific using Atherthon et al. (2017) leaf constituents data
sobolSI.case8.PINE <- read_rds("results/GSA/sobolSI_case8_PINE.rds")
sobolSI.case8.SPRUCE <- read_rds("results/GSA/sobolSI_case8_SPRUCE.rds")
sobolSI.case8.BIRCH <- read_rds("results/GSA/sobolSI_case8_BIRCH.rds")

# Case 9, intraspecific sun-exposed vs sunlit
sobolSI.case9.PINE <- read_rds("results/GSA/sobolSI_case9_PINE.rds")
sobolSI.case9.SPRUCE <- read_rds("results/GSA/sobolSI_case9_SPRUCE.rds")
sobolSI.case9.BIRCH <- read_rds("results/GSA/sobolSI_case9_BIRCH.rds")


# Case 9, intraspecific sun-exposed vs sunlit, spruce10/90
sobolSI.case9.PINE.spruce1090 <- read_rds("results/GSA/sobolSI_case9_spruce1090_PINE.rds")
sobolSI.case9.SPRUCE.spruce1090 <- read_rds("results/GSA/sobolSI_case9_spruce1090_SPRUCE.rds")
sobolSI.case9.BIRCH.spruce1090 <- read_rds("results/GSA/sobolSI_case9_spruce1090_BIRCH.rds")


# Case 10, intraspecific 50/50:sun-exposed/sunlit vs sunlit, spruce10/90
sobolSI.case10.PINE.spruce1090 <- read_rds("results/GSA/sobolSI_case10_spruce1090_PINE.rds")
sobolSI.case10.SPRUCE.spruce1090 <- read_rds("results/GSA/sobolSI_case10_spruce1090_SPRUCE.rds")
sobolSI.case10.BIRCH.spruce1090 <- read_rds("results/GSA/sobolSI_case10_spruce1090_BIRCH.rds")



# Column names
varnames <-  c("LAIe", "i0", "cgf.view", "CI",
               "wL.b2", "wL.b3", "wL.b4", "wL.b5", "wL.b6", "wL.b7", "wL.b8a", "wL.b11", "wL.b12", 
               "Rg.b2", "Rg.b3", "Rg.b4", "Rg.b5", "Rg.b6", "Rg.b7", "Rg.b8a", "Rg.b11", "Rg.b12")

# Rename columns
# Case 1
sobolSI.case1.PINE <- sobolSI.case1.PINE$Tfct %>% 
    as_tibble() %>% set_colnames(varnames)

sobolSI.case1.SPRUCE <- sobolSI.case1.SPRUCE$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

sobolSI.case1.BIRCH <- sobolSI.case1.BIRCH$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

sobolSI.case1.PINEnSPRUCE <- sobolSI.case1.PINEnSPRUCE$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)


# Case 1, first order effect
sobolSI.case1.1st.PINE <- sobolSI.case1.PINE$Sfct %>% 
  as_tibble() %>% set_colnames(varnames)

sobolSI.case1.1st.SPRUCE <- sobolSI.case1.SPRUCE$Sfct %>% 
  as_tibble() %>% set_colnames(varnames)

sobolSI.case1.1st.BIRCH <- sobolSI.case1.BIRCH$Sfct %>% 
  as_tibble() %>% set_colnames(varnames)


# Case 0
sobolSI.case0.LAIt.1.6to2 <- sobolSI.case0.LAIt.1.6to2$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

sobolSI.case0.LAIt.1.6to3 <- sobolSI.case0.LAIt.1.6to3$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

sobolSI.case0.LAIt.3to4 <- sobolSI.case0.LAIt.3to4$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

sobolSI.case0.LAIt.4to5 <- sobolSI.case0.LAIt.4to5$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

sobolSI.case0.LAIt.5to6 <- sobolSI.case0.LAIt.5to6$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

sobolSI.case0.LAIt.6to7.6 <- sobolSI.case0.LAIt.6to7.6$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

# Case 3a
sobolSI.case3a.PINE <- sobolSI.case3a.PINE$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

sobolSI.case3a.SPRUCE <- sobolSI.case3a.SPRUCE$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

sobolSI.case3a.BIRCH <- sobolSI.case3a.BIRCH$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)


# Case 4
sobolSI.case4.LAIt.1.6to2 <- sobolSI.case4.LAIt.1.6to2$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

sobolSI.case4.LAIt.1.6to3 <- sobolSI.case4.LAIt.1.6to3$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

sobolSI.case4.LAIt.3to4 <- sobolSI.case4.LAIt.3to4$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

sobolSI.case4.LAIt.4to5 <- sobolSI.case4.LAIt.4to5$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

sobolSI.case4.LAIt.5to6 <- sobolSI.case4.LAIt.5to6$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

sobolSI.case4.LAIt.6to7.6 <- sobolSI.case4.LAIt.6to7.6$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)


sobolSI.case4.LAIt.4to5.inclHerbRich <- sobolSI.case4.LAIt.4to5.inclHerbRich$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)


# Case 5
sobolSI.case5 <- sobolSI.case5$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

# Case 5, first-order effect
sobolSI.case5.1st <- sobolSI.case5$Sfct %>% 
  as_tibble() %>% set_colnames(varnames)



# Case 6
sobolSI.case6.CONIFER <- sobolSI.case6.CONIFER$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

sobolSI.case6.BROADLEAF <- sobolSI.case6.BROADLEAF$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

sobolSI.case6.CONIFERNoLarx <- sobolSI.case6.CONIFERNoLarx$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)


# Case 7
sobolSI.case7 <- sobolSI.case7$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)


# Case 5, ground term combined
varnames.groundTerm <-  c("LAIe", "i0", "CI",
               "wL.b2", "wL.b3", "wL.b4", "wL.b5", "wL.b6", "wL.b7", "wL.b8a", "wL.b11", "wL.b12", 
               "gr.contr.b2", "gr.contr.b3", "gr.contr.b4", "gr.contr.b5", "gr.contr.b6", "gr.contr.b7", "gr.contr.b8a", "gr.contr.b11", "gr.contr.b12")

sobolSI.case5.groundTerm <- sobolSI.case5.groundTerm$Tfct %>% 
  as_tibble() %>% set_colnames(varnames.groundTerm)


# Case 5, rescale output BRF with MD of pooled data
sobolSI.case5.scaled <- sobolSI.case5.scaled$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)


# Case 8
sobolSI.case8.PINE <- sobolSI.case8.PINE$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

sobolSI.case8.SPRUCE <- sobolSI.case8.SPRUCE$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

sobolSI.case8.BIRCH <- sobolSI.case8.BIRCH$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)


# Case 9
sobolSI.case9.PINE <- sobolSI.case9.PINE$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

sobolSI.case9.SPRUCE <- sobolSI.case9.SPRUCE$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

sobolSI.case9.BIRCH <- sobolSI.case9.BIRCH$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

# Case 9, spruce10/90
sobolSI.case9.PINE.spruce1090 <- sobolSI.case9.PINE.spruce1090$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

sobolSI.case9.SPRUCE.spruce1090 <- sobolSI.case9.SPRUCE.spruce1090$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

sobolSI.case9.BIRCH.spruce1090 <- sobolSI.case9.BIRCH.spruce1090$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

# Case 10, spruce10/90
sobolSI.case10.PINE.spruce1090 <- sobolSI.case10.PINE.spruce1090$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

sobolSI.case10.SPRUCE.spruce1090 <- sobolSI.case10.SPRUCE.spruce1090$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)

sobolSI.case10.BIRCH.spruce1090 <- sobolSI.case10.BIRCH.spruce1090$Tfct %>% 
  as_tibble() %>% set_colnames(varnames)



# Difference between first-order and total-order effect
diff <- abs(sobolSI.case5.1st - sobolSI.case5)
max(diff)
diff[diff < 0.05] <- NA

diff <- abs(sobolSI.case1.1st.SPRUCE - sobolSI.case1.SPRUCE)
diff[diff < 0.05] <- NA
# BIRCH 0.13201411 0.11736037


# Label for x-axis
wav.label <- c("B", "G", "R", "RE1", "RE2", "RE3", "NIRn", "SWIR1", "SWIR2")
wav <- 1:9 

# If not show SWIR2
# wav.label <- c("B", "G", "R", "RE1", "RE2", "RE3", "NIRn", "SWIR1")
# wav <- 1:8

# Plot
# pdf("graphs/sobolSI_case1.pdf", width = 7.5, height = 3, pointsize = 10)
# pdf("graphs/sobolSI_case0.pdf", width = 7.5, height = 6, pointsize = 10)
# pdf("graphs/sobolSI_case3a.pdf", width = 7.5, height = 3, pointsize = 10)
# pdf("graphs/sobolSI_case4.pdf", width = 7.5, height = 6, pointsize = 10)
# pdf("graphs/sobolSI_case4_LAIt4to5_withANDwithoutHerbRich.pdf", width = 7.5, height = 3, pointsize = 10)
# pdf("graphs/sobolSI_case5_case1.pdf", width = 6, height = 6, pointsize = 10)
# pdf("graphs/sobolSI_case6.pdf", width = 7.5, height = 3, pointsize = 10)
# pdf("graphs/sobolSI_case6_CONIFERNoLarx.pdf", width = 3, height = 3, pointsize = 10)
# pdf("graphs/sobolSI_case7_darkGround.pdf", width = 3, height = 3, pointsize = 10)
# pdf("graphs/sobolSI_case1_PINEnSPRUCE.pdf", width = 3, height = 3, pointsize = 10)
# pdf("graphs/sobolSI_case5_groundTermCombined.pdf", width = 3, height = 2.5, pointsize = 10)
# pdf("graphs/sobolSI_final_A.pdf", width = 6, height = 5, pointsize = 10)     # field data plots
# pdf("graphs/sobolSI_final_B.pdf", width = 6, height = 2.5, pointsize = 10)     # otaniemi2016 species
# pdf("graphs/sobolSI_final_C.pdf", width = 3, height = 2.5, pointsize = 10)    # ground term combined
# pdf("graphs/sobolSI_case5_scaledByPooledMD.pdf", width = 3, height = 2.5, pointsize = 10)    # ground term combined
# pdf("graphs/sobolSI_case8_intraspecific.pdf", width = 6, height = 2.5, pointsize = 10)
# pdf("graphs/sobolSI_case9_intraspecific.pdf", width = 6, height = 2.5, pointsize = 10)
# pdf("graphs/sobolSI_case9_intraspecific_spruce1090.pdf", width = 6, height = 2.5, pointsize = 10)
# pdf("graphs/sobolSI_case10_intraspecific_spruce1090.pdf", width = 6, height = 2.5, pointsize = 10)
# pdf("graphs/sobolSI_final_A_1stOrderEffect.pdf", width = 6, height = 5, pointsize = 10)     # field data plots
pdf("graphs/sobolSI_final_A_revised_2nd_round.pdf", width = 7, height = 6, pointsize = 12)     # field data plots


# store default par
# par.default <- par()


# For 2x2 fig
# par(mfrow = c(2,2), oma = c(0, 0, 0, 0), mai = c(0.35, 0.35, 0.1, 0.1),   # mai = c(0.45, 0.35, 0.1, 0.1)
#     ps = 10, mgp = c(1.5, 0.5, 0), mar = c(2, 2.5, 1.5, 0.5))   # mgp = c(1.7, 0.5, 0), mar = c(2.7, 2.5, 1.5, 0.5)

# # Figure for RSL editorial revision
# par(mfrow = c(2,2), oma = c(0, 0, 0, 0), mai = c(0.5, 0.5, 0.5, 0.5),   # mai = c(0.35, 0.35, 0.1, 0.1)
#     ps = 12, mgp = c(1.5, 0.5, 0), mar = c(2, 2.5, 1.5, 0.5),
#     las = 1) # axis label always horizontal  

par(mfrow = c(2,2), ps = 12, 
    mar = c(2.6, 2.6, 1.3, 0.4),    # mar = c(3, 3.5, 1.5, 0.5)
    # mai = c(0.5, 0.5, 0.2, 0.2),
    mgp = c(1.5, 0.5, 0))   # las = 1   mgp = c(2, 0.7, 0)


# For 1x2 fig
# par(mfrow = c(1,3), oma = c(0, 0, 0, 0), mai = c(0.35, 0.35, 0.1, 0.1),   # mai = c(0.45, 0.35, 0.1, 0.1)
#     ps = 8, mgp = c(1.5, 0.5, 0), mar = c(2, 2.5, 1.5, 0.5))   # mgp = c(1.7, 0.5, 0), mar = c(2.7, 2.5, 1.5, 0.5)

# For Case 5, ground term combined
# par(mfrow = c(1,1), oma = c(0, 0, 0, 0), mai = c(0.35, 0.35, 0.1, 0.1),
#     ps = 10, mgp = c(1.5, 0.5, 0), mar = c(2.5, 2.5, 0.5, 0.5)) 

# Sub-plot 1
temp <- sobolSI.case5 * 100
plot(wav, temp$LAIe, type = 'b', col = 'dodgerblue', ylim = c(0, 100), lwd = 1,  xaxt = "n", yaxt = "n",            #  xaxp = c(400,2400,10),
     main = "All plots", xlab = "", ylab = "Contribution (%)") # main = "All plots"
axis(1, at = 1:9, labels = FALSE, tck = -0.02)
points(wav, temp$i0, col = 'magenta', lwd = 1, type = "b")
points(wav, temp$cgf.view, col = 'brown', lwd = 1, type = "b")
points(wav, c(temp$wL.b2[1], temp$wL.b3[2], temp$wL.b4[3], temp$wL.b5[4], temp$wL.b6[5], temp$wL.b7[6], temp$wL.b8a[7], temp$wL.b11[8], temp$wL.b12[9]),           # wL
       col = 'chartreuse4', lwd = 1, lty = 2, type = "b", pch = 2)
points(wav, c(temp$Rg.b2[1], temp$Rg.b3[2], temp$Rg.b4[3], temp$Rg.b5[4], temp$Rg.b6[5], temp$Rg.b7[6], temp$Rg.b8a[7], temp$Rg.b11[8], temp$Rg.b12[9]),            # Rg
       col = 'grey40', lwd = 1, lty = 2, type = "b", pch = 2)
# Not show SWIR2
# points(wav, c(temp$wL.b2[1], temp$wL.b3[2], temp$wL.b4[3], temp$wL.b5[4], temp$wL.b6[5], temp$wL.b7[6], temp$wL.b8a[7], temp$wL.b11[8]),           # wL
#        col = 'chartreuse4', lwd = 1, lty = 2, type = "b", pch = 2)
# points(wav, c(temp$Rg.b2[1], temp$Rg.b3[2], temp$Rg.b4[3], temp$Rg.b5[4], temp$Rg.b6[5], temp$Rg.b7[6], temp$Rg.b8a[7], temp$Rg.b11[8]),            # Rg
#        col = 'grey40', lwd = 1, lty = 2, type = "b", pch = 2)

points(wav, temp$CI, col = 'dark orange', lwd = 1, type = "b")   # Add CI
# axis(1, at = 1:9, labels = wav.label, cex.axis = 0.8, srt = 45, tck = -0.02) # cex.axis = 0.65
text(1:9, par("usr")[3] - 6, srt = 45, adj = 1,
     labels = wav.label, xpd = TRUE, cex = 0.8)
axis(2, at = seq(0,100,20), labels = as.character(seq(0,100,20)), cex.axis = 0.8, las = 1, tck = -0.02)   # cex.axis = 0.65

# axis(1, at = 1:8, labels = wav.label, srt = 45, cex.axis = 0.65)   # cex.axis = 0.65     # if not show swir2
# text(9, 100, expression(bold(italic("(a)"))))
# text(8, 100, expression(bold("(a)")))                                                    # if not show swir2


# Add legend in subplot 1
cols <- c("dodgerblue", "magenta", "brown", "dark orange",
          "chartreuse4", "grey40")

# legend("topleft", xpd = TRUE,
#        legend = c(expression(LAI[eff]), expression(paste(i[0], " = 1 - ", cgf(theta[sun]))), expression(paste(cgf(theta[view]), " = 1 - ECC      ")), "CI", expression(omega[L]), expression(rho[g])),
#        col = cols, pch = c(rep(1,4), 2, 2),
#        lty = c(rep(1, 4), 2, 2),  lwd = rep(1,6), ncol = 2, cex = 1.05, bty = "n")  # remove inset and x.intersp
# cex = 1.2 for 2x2 fig

# Legend italic symbol
legend("topleft", xpd = TRUE,
       legend = c(expression(LAI[eff]),
                  expression(paste(italic(i[0]), " = 1 - ", italic(cgf(theta[sun])))),
                  expression(paste(italic(cgf(theta[view])), " = 1 - ECC")),
                  "CI",
                  expression(italic(omega[L])), expression(italic(rho[g]))),
       col = cols, pch = c(rep(1,4), 2, 2),
       lty = c(rep(1, 4), 2, 2),  lwd = rep(1,6), ncol = 2, cex = 1.2, bty = "n")  # cex = 1.05,  remove inset and x.intersp




#
#
# Sub-plot 2
temp <- sobolSI.case1.BIRCH * 100
plot(wav, temp$LAIe, type = 'b', col = 'dodgerblue', ylim = c(0, 100),  lwd = 1, xaxt = "n", yaxt = "n",    # xaxp = c(400,2400,10),
     main = "Birch", xlab = "", ylab = "Contribution (%)")
axis(1, at = 1:9, labels = FALSE, tck = -0.02)
points(wav, temp$i0, col = 'magenta', lwd = 1, type = "b")
points(wav, temp$cgf.view, col = 'brown', lwd = 1, type = "b")
points(wav, c(temp$wL.b2[1], temp$wL.b3[2], temp$wL.b4[3], temp$wL.b5[4], temp$wL.b6[5], temp$wL.b7[6], temp$wL.b8a[7], temp$wL.b11[8], temp$wL.b12[9]),           # wL
       col = 'chartreuse4', lwd = 1, lty = 2, type = "b", pch = 2)
points(wav, c(temp$Rg.b2[1], temp$Rg.b3[2], temp$Rg.b4[3], temp$Rg.b5[4], temp$Rg.b6[5], temp$Rg.b7[6], temp$Rg.b8a[7], temp$Rg.b11[8], temp$Rg.b12[9]),            # Rg
       col = 'grey40', lwd = 1, lty = 2, type = "b", pch = 2)
# Not show SWIR2
# points(wav, c(temp$wL.b2[1], temp$wL.b3[2], temp$wL.b4[3], temp$wL.b5[4], temp$wL.b6[5], temp$wL.b7[6], temp$wL.b8a[7], temp$wL.b11[8]),           # wL
#        col = 'chartreuse4', lwd = 1, lty = 2, type = "b", pch = 2)
# points(wav, c(temp$Rg.b2[1], temp$Rg.b3[2], temp$Rg.b4[3], temp$Rg.b5[4], temp$Rg.b6[5], temp$Rg.b7[6], temp$Rg.b8a[7], temp$Rg.b11[8]),            # Rg
#        col = 'grey40', lwd = 1, lty = 2, type = "b", pch = 2)

points(wav, temp$CI, col = 'dark orange', lwd = 1, type = "b")   # Add CI
# axis(1, at = 1:9, labels = wav.label, cex.axis = 0.8, srt = 45, tck = -0.02) # cex.axis = 0.65
text(1:9, par("usr")[3] - 6, srt = 45, adj = 1,
     labels = wav.label, xpd = TRUE, cex = 0.8)
axis(2, at = seq(0,100,20), labels = as.character(seq(0,100,20)), cex.axis = 0.8, las = 1, tck = -0.02)   # cex.axis = 0.65
# axis(1, at = 1:8, labels = wav.label, srt = 45, cex.axis = 0.65)   # cex.axis = 0.65     # if not show swir2
# text(9, 100, expression(bold(italic("(b)"))))
# text(8, 100, expression(bold("(b)")))                                                    # if not show swir2

# Sub-plot 3
temp <- sobolSI.case1.PINE * 100
plot(wav, temp$LAIe, type = 'b', col = 'dodgerblue', ylim = c(0, 100),  lwd = 1, xaxt = "n", yaxt = "n",    # xaxp = c(400,2400,10),
     main = "Pine", xlab = "", ylab = "Contribution (%)")   # main = "Pine"
axis(1, at = 1:9, labels = FALSE, tck = -0.02)
points(wav, temp$i0, col = 'magenta', lwd = 1, type = "b")
points(wav, temp$cgf.view, col = 'brown', lwd = 1, type = "b")
points(wav, c(temp$wL.b2[1], temp$wL.b3[2], temp$wL.b4[3], temp$wL.b5[4], temp$wL.b6[5], temp$wL.b7[6], temp$wL.b8a[7], temp$wL.b11[8], temp$wL.b12[9]),           # wL
       col = 'chartreuse4', lwd = 1, lty = 2, type = "b", pch = 2)
points(wav, c(temp$Rg.b2[1], temp$Rg.b3[2], temp$Rg.b4[3], temp$Rg.b5[4], temp$Rg.b6[5], temp$Rg.b7[6], temp$Rg.b8a[7], temp$Rg.b11[8], temp$Rg.b12[9]),            # Rg
       col = 'grey40', lwd = 1, lty = 2, type = "b", pch = 2)
points(wav, temp$CI, col = 'dark orange', lwd = 1, type = "b")   # Add CI
# axis(1, at = 1:9, labels = wav.label, cex.axis = 0.8, srt = 45, tck = -0.02) # cex.axis = 0.65
text(1:9, par("usr")[3] - 6, srt = 45, adj = 1,
     labels = wav.label, xpd = TRUE, cex = 0.8)
axis(2, at = seq(0,100,20), labels = as.character(seq(0,100,20)), cex.axis = 0.8, las = 1, tck = -0.02)   # cex.axis = 0.65
# text(9, 100, expression(bold(italic("(c)"))))

# Sub-plot 4a
temp <- sobolSI.case1.SPRUCE * 100
plot(wav, temp$LAIe, type = 'b', col = 'dodgerblue', ylim = c(0, 100), lwd = 1,    xaxt = "n",   yaxt = "n",       # xaxp = c(400,2400,10)
     main = "Spruce", xlab = "", ylab = "Contribution (%)")
axis(1, at = 1:9, labels = FALSE, tck = -0.02)
points(wav, temp$i0, col = 'magenta', lwd = 1, type = "b")
points(wav, temp$cgf.view, col = 'brown', lwd = 1, type = "b")
points(wav, c(temp$wL.b2[1], temp$wL.b3[2], temp$wL.b4[3], temp$wL.b5[4], temp$wL.b6[5], temp$wL.b7[6], temp$wL.b8a[7], temp$wL.b11[8], temp$wL.b12[9]),           # wL
       col = 'chartreuse4', lwd = 1, lty = 2, type = "b", pch = 2)
points(wav, c(temp$Rg.b2[1], temp$Rg.b3[2], temp$Rg.b4[3], temp$Rg.b5[4], temp$Rg.b6[5], temp$Rg.b7[6], temp$Rg.b8a[7], temp$Rg.b11[8], temp$Rg.b12[9]),            # Rg
       col = 'grey40', lwd = 1, lty = 2, type = "b", pch = 2)
points(wav, temp$CI, col = 'dark orange', lwd = 1, type = "b")   # Add CI
# axis(1, at = 1:9, labels = wav.label, cex.axis = 0.8, srt = 45, tck = -0.02) # cex.axis = 0.65
text(1:9, par("usr")[3] - 6, srt = 45, adj = 1,
     labels = wav.label, xpd = TRUE, cex = 0.8)
axis(2, at = seq(0,100,20), labels = as.character(seq(0,100,20)), cex.axis = 0.8, las = 1, tck = -0.02)   # cex.axis = 0.65
# text(9, 100, expression(bold(italic("(d)"))))



# # Sub-plot 4b
# temp <- sobolSI.case4.LAIt.4to5.inclHerbRich * 100
# plot(wav, temp$LAIe, type = 'b', col = 'dodgerblue', ylim = c(0, 100),  lwd = 1, xaxt = "n",     # xaxp = c(400,2400,10),
#      main = "LAIt 4-5 (site type: mesic, xeric, herb-rich)", xlab = "Spectral band", ylab = "Contribution (%)")
# points(wav, temp$i0, col = 'magenta', lwd = 1, type = "b")
# points(wav, temp$cgf.view, col = 'brown', lwd = 1, type = "b")
# points(wav, c(temp$wL.b2[1], temp$wL.b3[2], temp$wL.b4[3], temp$wL.b5[4], temp$wL.b6[5], temp$wL.b7[6], temp$wL.b8a[7], temp$wL.b11[8], temp$wL.b12[9]),           # wL
#        col = 'chartreuse4', lwd = 1, lty = 2, type = "b", pch = 2)
# points(wav, c(temp$Rg.b2[1], temp$Rg.b3[2], temp$Rg.b4[3], temp$Rg.b5[4], temp$Rg.b6[5], temp$Rg.b7[6], temp$Rg.b8a[7], temp$Rg.b11[8], temp$Rg.b12[9]),            # Rg
#        col = 'grey40', lwd = 1, lty = 2, type = "b", pch = 2)
# points(wav, temp$CI, col = 'dark orange', lwd = 1, type = "b")   # Add CI
# axis(1, at = 1:9, labels = wav.label, srt = 45, cex.axis = 0.65)



# # Sub-plot 5
# temp <- sobolSI.case4.LAIt.5to6 * 100
# plot(wav, temp$LAIe, type = 'b', col = 'dodgerblue', ylim = c(0, 100),  lwd = 1, xaxt = "n",     # xaxp = c(400,2400,10),
#      main = "LAIt 5-6", xlab = "Spectral band", ylab = "Contribution (%)")
# points(wav, temp$i0, col = 'magenta', lwd = 1, type = "b")
# points(wav, temp$cgf.view, col = 'brown', lwd = 1, type = "b")
# points(wav, c(temp$wL.b2[1], temp$wL.b3[2], temp$wL.b4[3], temp$wL.b5[4], temp$wL.b6[5], temp$wL.b7[6], temp$wL.b8a[7], temp$wL.b11[8], temp$wL.b12[9]),           # wL
#        col = 'chartreuse4', lwd = 1, lty = 2, type = "b", pch = 2)
# points(wav, c(temp$Rg.b2[1], temp$Rg.b3[2], temp$Rg.b4[3], temp$Rg.b5[4], temp$Rg.b6[5], temp$Rg.b7[6], temp$Rg.b8a[7], temp$Rg.b11[8], temp$Rg.b12[9]),            # Rg
#        col = 'grey40', lwd = 1, lty = 2, type = "b", pch = 2)
# points(wav, temp$CI, col = 'dark orange', lwd = 1, type = "b")   # Add CI
# axis(1, at = 1:9, labels = wav.label, srt = 45, cex.axis = 0.65)
# 
# # Sub-plot 6
# temp <- sobolSI.case4.LAIt.6to7.6 * 100
# plot(wav, temp$LAIe, type = 'b', col = 'dodgerblue', ylim = c(0, 100),  lwd = 1, xaxt = "n",     # xaxp = c(400,2400,10),
#      main = "LAIt 6-7.6", xlab = "Spectral band", ylab = "Contribution (%)")
# points(wav, temp$i0, col = 'magenta', lwd = 1, type = "b")
# points(wav, temp$cgf.view, col = 'brown', lwd = 1, type = "b")
# points(wav, c(temp$wL.b2[1], temp$wL.b3[2], temp$wL.b4[3], temp$wL.b5[4], temp$wL.b6[5], temp$wL.b7[6], temp$wL.b8a[7], temp$wL.b11[8], temp$wL.b12[9]),           # wL
#        col = 'chartreuse4', lwd = 1, lty = 2, type = "b", pch = 2)
# points(wav, c(temp$Rg.b2[1], temp$Rg.b3[2], temp$Rg.b4[3], temp$Rg.b5[4], temp$Rg.b6[5], temp$Rg.b7[6], temp$Rg.b8a[7], temp$Rg.b11[8], temp$Rg.b12[9]),            # Rg
#        col = 'grey40', lwd = 1, lty = 2, type = "b", pch = 2)
# points(wav, temp$CI, col = 'dark orange', lwd = 1, type = "b")   # Add CI
# axis(1, at = 1:9, labels = wav.label, srt = 45, cex.axis = 0.65)

# # Sub-plot for Case 5, ground term combined
# temp <- sobolSI.case5.groundTerm * 100
# plot(wav, temp$LAIe, type = 'b', col = 'dodgerblue', ylim = c(0, 100), lwd = 1,  xaxt = "n",            #  xaxp = c(400,2400,10),
#      main = "", xlab = "Spectral band", ylab = "Contribution (%)")
# points(wav, temp$i0, col = 'magenta', lwd = 1, type = "b")
# # points(wav, temp$cgf.view, col = 'brown', lwd = 1, type = "b")
# points(wav, c(temp$wL.b2[1], temp$wL.b3[2], temp$wL.b4[3], temp$wL.b5[4], temp$wL.b6[5], temp$wL.b7[6], temp$wL.b8a[7], temp$wL.b11[8], temp$wL.b12[9]),           # wL
#        col = 'chartreuse4', lwd = 1, lty = 2, type = "b", pch = 2)
# points(wav, c(temp$gr.contr.b2[1], temp$gr.contr.b3[2], temp$gr.contr.b4[3], temp$gr.contr.b5[4], temp$gr.contr.b6[5], temp$gr.contr.b7[6], temp$gr.contr.b8a[7], temp$gr.contr.b11[8], temp$gr.contr.b12[9]),            # gr.contr
#        col = 'grey40', lwd = 1, lty = 3, type = "b", pch = 0)   # pch
# points(wav, temp$CI, col = 'dark orange', lwd = 1, type = "b")   # Add CI
# axis(1, at = 1:9, labels = wav.label, srt = 45, cex.axis = 0.5)   # cex.axis = 0.65

# Legend
# par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
# plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
# cols <- c("dodgerblue", "magenta", "brown", "dark orange",
#           "chartreuse4", "grey40")
# 
# legend("topleft", xpd = TRUE,
#        legend = c(expression(LAI[eff]), expression(paste(i[0], " = 1 - ", cgf(theta[sun]))), expression(paste(cgf(theta[view]), " = 1 - ECC      ")), "CI", expression(omega[L]), expression(rho[g])),
#        col = cols, pch = c(rep(1,4), 2, 2),
#        lty = c(rep(1, 4), 2, 2),  lwd = rep(1,6), ncol = 2, cex = 1.2, inset = c(0.13,0.05), bty = "n", x.intersp = 0.5) # inset = c(0.13,-0.001)

# # Legend for Case 5, ground term combined
# par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
# plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
# cols <- c("dodgerblue", "magenta", "dark orange",
#           "chartreuse4", "grey40")
# 
# legend("topleft", xpd = TRUE,
#        legend = c(expression(LAI[eff]), expression(paste(i[0], " = 1 - ", cgf(theta[sun]))), "CI", expression(omega[L]), "Ground contribution"),
#        col = cols, pch = c(rep(1,3), 2, 0),
#        lty = c(rep(1, 3), 2, 3),  lwd = rep(1,5), ncol = 2, cex = 0.8, inset = c(0.13,0.01), bty = "n", x.intersp = 0.5) # inset = c(0.13,-0.001)  # c(0.13,0.07)



dev.off()
