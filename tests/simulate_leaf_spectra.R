# Interpreted from Fig in Atherton et al. (2017), Fig 3
# Top canopy leaves, because at canopy level PARAS simulation shows minor difference (except maybe in NIR, RMSD
# between 50/50 E/S and only E is 0.025) compared to scatter in Landsat BRF in Hadi et al. (2016)
# Also not sure how to average PROSPECT simulation for many samples, for E and S leaf constituents


# TC = total chlorophyll (Cab) in microgram/cm2
# SLA = specific leaf area in cm2/g
# WC = water content in no unit (unit*100=%unit)
true.data.TC <- read_csv2("graphs/jpg/atherton2017_total_chlorophyll.csv")
true.data.SLA <- read_csv2("graphs/jpg/atherton2017_specific_leaf_area.csv")
true.data.WC <- read_csv2("graphs/jpg/atherton2017_water_content.csv")


temp <- tibble(ID=c("B1", "B2", "B3",
            "P1", "P2", "P3",
            "S1", "S2", "S3"),
       TC=true.data.TC$y,
       SLA=true.data.SLA$y,
       WC=true.data.WC$y)

# PROSPECT units
# Cab = TC in microgram/cm2
# Cw = WC in g/cm2 or cm-1
# Cm = 1/SLA in g/cm2
temp <- temp %>% mutate(Cab=TC,
                        Cm=1/SLA,
                        WC.percent=WC*100,
                        Cw=Cm*WC.percent*0.01*(100/(100-WC.percent)))   # Cw conversion see Hadi et al. (2017)



# Uniform distribution ----------------------------------------------------

# Function to simulate copula: uniform distribution with covariance
library(MASS)
library(tidyverse)
library(mvtnorm)
library(psych)


# Input var1=c(min1, max1)
sim.copula <- function(n=2500,  
                      var1, var2, var3,     # 1.Cab, 2.Cw, 3.Cm
                      cor12, cor23, cor13,
                      var.names) {
  
  
  min1 <- var1[1]
  max1 <- var1[2]
  
  min2 <- var2[1]
  max2 <- var2[2]
  
  min3 <- var3[1]
  max3 <- var3[2]
  
  set.seed(123)
  prosp_mvr_emp <- mvrnorm(n,   # This is the start i.e., multivariate normal distributions with mean 0              
                           mu=c(0, 0, 0),              # Mean vector 0
                           Sigma=cbind(c(1,cor12,cor13), c(cor12,1,cor23), c(cor13,cor23,1)),     # Correlation matrix, instead of covariance matrix      
                           empirical=T)
  
  u <- pnorm(prosp_mvr_emp)                   # This is the trick to transform normal distr -> uniform           
  
  # Choose the marginals
  x1 <- qunif(u[,1], min1, max1)
  x2 <- qunif(u[,2], min2, max2)
  x3 <- qunif(u[,3], min3, max3)
  
  df <- cbind(x1, x2, x3)
  colnames(df) <- var.names # c("Cab", "Cw", "Cm")
  
  #  Need to remove negative values 
  posnev <- df[,1]*df[,2]*df[,3]
  df_pos <- df[which(posnev > 0), ]
  
  df_pos <- as_tibble(df_pos)
  return(df_pos)
}

# Run the function
# We use the pooled data correlations (interpreted roughly based on the legend colour in Fig 6)
cor12 <- -0.75  # 1.Cab, 2.Cw, 3.Cm
cor23 <- -0.85
cor13 <- 0.95
  
# Run the function for birch
temp.birch <- temp %>% dplyr::filter(ID %in% c("B1", "B2", "B3")) %>% 
  dplyr::select(Cab, Cw, Cm) %>% summarise_all(funs(min, max))

copula.birch <- sim.copula(n=2500, 
                     var1=c(temp.birch$Cab_min, temp.birch$Cab_max),          
                     var2=c(temp.birch$Cw_min, temp.birch$Cw_max),
                     var3=c(temp.birch$Cm_min, temp.birch$Cm_max),
                     cor12=cor12, cor23=cor23, cor13=cor13,
                     var.names=c("Cab", "Cw", "Cm"))

plot(copula.birch$Cab, copula.birch$Cw, main=paste("r = ", cor(copula.birch$Cab, copula.birch$Cw)))
plot(copula.birch$Cw, copula.birch$Cm, main=paste("r = ", cor(copula.birch$Cw, copula.birch$Cm)))
plot(copula.birch$Cab, copula.birch$Cm, main=paste("r = ", cor(copula.birch$Cab, copula.birch$Cm)))


# Run the function for pine
temp.pine <- temp %>% dplyr::filter(ID %in% c("P1", "P2", "P3")) %>% 
  dplyr::select(Cab, Cw, Cm) %>% summarise_all(funs(min, max))

copula.pine <- sim.copula(n=2500, 
                           var1=c(temp.pine$Cab_min, temp.pine$Cab_max),          
                           var2=c(temp.pine$Cw_min, temp.pine$Cw_max),
                           var3=c(temp.pine$Cm_min, temp.pine$Cm_max),
                           cor12=cor12, cor23=cor23, cor13=cor13,
                           var.names=c("Cab", "Cw", "Cm"))


# Run the function for spruce
temp.spruce <- temp %>% dplyr::filter(ID %in% c("S1", "S2", "S3")) %>% 
  dplyr::select(Cab, Cw, Cm) %>% summarise_all(funs(min, max))

copula.spruce <- sim.copula(n=2500, 
                           var1=c(temp.spruce$Cab_min, temp.spruce$Cab_max),          
                           var2=c(temp.spruce$Cw_min, temp.spruce$Cw_max),
                           var3=c(temp.spruce$Cm_min, temp.spruce$Cm_max),
                           cor12=cor12, cor23=cor23, cor13=cor13,
                           var.names=c("Cab", "Cw", "Cm"))



# Normal distribution -----------------------------------------------------
# Input var1 = c(mu1, sd1)
sim.normal <- function(n=2500, 
                       var1, var2, var3,
                       cor12, cor23, cor13,
                       var.names) {
  

  # Mean and sd 1.Cab, 2.Cw, 3.Cm
  mu1 <- var1[1]; sigma1 <- var1[2]
  mu2 <- var2[1]; sigma2 <- var2[2]
  mu3 <- var3[1]; sigma3 <- var3[2]
  
  # Correlations and covariances
  cov12 <- cor12*sigma1*sigma2
  cov23 <- cor23*sigma2*sigma3
  cov13 <- cor13*sigma1*sigma3
  
  set.seed(123)
  prosp_mvr_emp <- mvrnorm(n, mu=c(mu1, mu2, mu3), 
                           Sigma=cbind(c(sigma1^2,cov12,cov13), c(cov12,sigma2^2,cov23), c(cov13,cov23,sigma3^2)),
                           empirical=T)
  
  prosp_mvr_emp <- as_tibble(prosp_mvr_emp)
  colnames(prosp_mvr_emp) <- var.names
  
  # Need to remove negative values 
  posnev <- prosp_mvr_emp[,1]*prosp_mvr_emp[,2]*prosp_mvr_emp[,3]
  prosp_mvr_emp_pos <- prosp_mvr_emp[which(posnev > 0), ]
  
  return(prosp_mvr_emp_pos)
}


# Run the function
# We use the pooled data correlations (interpreted roughly based on the legend colour in Fig 6)
cor12 <- -0.75  # 1.Cab, 2.Cw, 3.Cm
cor23 <- -0.85
cor13 <- 0.95

# Run the function for birch
temp.birch <- temp %>% dplyr::filter(ID %in% c("B1", "B2", "B3")) %>% 
  dplyr::select(Cab, Cw, Cm) %>% summarise_all(funs(mean, sd))

normal.birch <- sim.normal(n=2500, 
                           var1=c(temp.birch$Cab_mean, temp.birch$Cab_sd),          
                           var2=c(temp.birch$Cw_mean, temp.birch$Cw_sd),
                           var3=c(temp.birch$Cm_mean, temp.birch$Cm_sd),
                           cor12=cor12, cor23=cor23, cor13=cor13,
                           var.names=c("Cab", "Cw", "Cm"))

plot(normal.birch$Cab, normal.birch$Cw, main=paste("r = ", cor(normal.birch$Cab, normal.birch$Cw)))
plot(normal.birch$Cw, normal.birch$Cm, main=paste("r = ", cor(normal.birch$Cw, normal.birch$Cm)))
plot(normal.birch$Cab, normal.birch$Cm, main=paste("r = ", cor(normal.birch$Cab, normal.birch$Cm)))

# Check min and max
temp %>% dplyr::filter(ID %in% c("B1", "B2", "B3")) %>% 
  dplyr::select(Cab, Cw, Cm) %>% summarise_all(funs(min, max))

normal.birch %>% summarise_all(funs(min, max))
hist(normal.birch$Cab)
# Min and max are beyond the range, maybe because std of 3 samples are problematic



# So copula or normal? ----------------------------------------------------
# First test with copula, if BRF is not sensitive to wL, 
# we can expect BRF is even less sensitive to wL with normal


# PROSPECT simulation with many samples -----------------------------------------
# Old code, could be improved, a bit slow cause loop. Maybe list column?
my_prospect_many <- function(samples, N=1.7) {
  # Initialize output
  out <- list()
  n <- nrow(samples)
  out$Wavelength <- read_csv2("data/leaf/prospect_wav.csv") %>% .[["x"]]
  out$Reflectance <- array(NA, c(length(out$Wavelength),n))
  out$Transmittance <- array(NA, c(length(out$Wavelength),n))
  out$Albedo <- array(NA, c(length(out$Wavelength),n))
  
  for(i in 1:n){
    prospect_out <- prospect4(N=N, Cab=samples[[i,"Cab"]], Cw=samples[[i,"Cw"]], Cm=samples[[i,"Cm"]])  # Double [[]] to access value out of tibble
    
    out$Reflectance[,i] <- prospect_out[,2]
    out$Transmittance[,i] <- prospect_out[,3]
    out$Albedo[,i] <- prospect_out[,2] + prospect_out[,3]
  }
  
  return(out)
}



# Run PROSPECT simulation for copula --------------------------------------
# Fixed N for fresh leaves
# wL.copula.BIRCH <- my_prospect_many(samples=copula.birch, N=1.7)
# write_rds(wL.copula.BIRCH, "results/PROSPECT/leaf_sim_copula_BIRCH.rds") # Save it it takes long time!
# 
# wL.copula.PINE <- my_prospect_many(samples=copula.pine, N=1.7)
# write_rds(wL.copula.PINE, "results/PROSPECT/leaf_sim_copula_PINE.rds")
# 
# wL.copula.SPRUCE <- my_prospect_many(samples=copula.spruce, N=1.7)
# write_rds(wL.copula.SPRUCE, "results/PROSPECT/leaf_sim_copula_SPRUCE.rds")

# Maybe instead of min-max, use range 2*sd ********************************************************************
wL.copula.BIRCH <- read_rds("results/PROSPECT/leaf_sim_copula_BIRCH.rds")
wL.copula.PINE <- read_rds("results/PROSPECT/leaf_sim_copula_PINE.rds")
wL.copula.SPRUCE <- read_rds("results/PROSPECT/leaf_sim_copula_SPRUCE.rds")


# Visualize with speclib
wL.copula.BIRCH.speclib <- speclib(spectra=t(wL.copula.BIRCH$Albedo), wavelength=wL.copula.BIRCH$Wavelength)
wL.copula.PINE.speclib <- speclib(spectra=t(wL.copula.PINE$Albedo), wavelength=wL.copula.PINE$Wavelength)
wL.copula.SPRUCE.speclib <- speclib(spectra=t(wL.copula.SPRUCE$Albedo), wavelength=wL.copula.SPRUCE$Wavelength)

par(mfrow=c(1,3), ps=18)
plot(wL.copula.BIRCH.speclib, ylim=c(0,1), ylab="Albedo", main="BIRCH")
plot(wL.copula.BIRCH.speclib, new=FALSE, FUN="min", lty=4)
plot(wL.copula.BIRCH.speclib, new=FALSE, FUN="max", lty=4)

plot(wL.copula.PINE.speclib, ylim=c(0,1), ylab="Albedo", main="PINE")
plot(wL.copula.PINE.speclib, new=FALSE, FUN="min", lty=4)
plot(wL.copula.PINE.speclib, new=FALSE, FUN="max", lty=4)

plot(wL.copula.SPRUCE.speclib, ylim=c(0,1), ylab="Albedo", main="SPRUCE")
plot(wL.copula.SPRUCE.speclib, new=FALSE, FUN="min", lty=4)
plot(wL.copula.SPRUCE.speclib, new=FALSE, FUN="max", lty=4)
# Watch out, the level in NIR is too low for pine and spruce ! *********************************
# Thus, only take the range (min-max, relative to mean) ? *********************************************

# Just checking variance --------------------------------------------------

temp %>% dplyr::filter(ID %in% c("P1", "P2", "P3")) %>% 
  dplyr::select(Cab, Cw, Cm) %>% summarise_all(funs(min, max, mean))
x1.min <- prospect4(N=1.7, Cab=77.16814, Cw=0.03793123, Cm=0.03609113); x1.min$Albedo <- x1.min$Reflectance + x1.min$Transmittance
x1.max <- prospect4(N=1.7, Cab=109.3805, Cw=0.04165208, Cm=0.04168975); x1.max$Albedo <- x1.max$Reflectance + x1.max$Transmittance
x1.mean <- prospect4(N=1.7, Cab=89.55752, Cw=0.03945591, Cm=0.03843735); x1.mean$Albedo <- x1.mean$Reflectance + x1.mean$Transmittance

x2.min <- prospect4(N=1.7, Cab=77.16814, Cw=0.03793123, Cm=0.5*0.03609113); x2.min$Albedo <- x2.min$Reflectance + x2.min$Transmittance
x2.max <- prospect4(N=1.7, Cab=109.3805, Cw=0.04165208, Cm=0.5*0.04168975); x2.max$Albedo <- x2.max$Reflectance + x2.max$Transmittance
x2.mean <- prospect4(N=1.7, Cab=89.55752, Cw=0.03945591, Cm=0.5*0.03843735); x2.mean$Albedo <- x2.mean$Reflectance + x2.mean$Transmittance

plot(x1.min$Wavelength, x1.min$Albedo, type='l', col="red", ylim=c(0,1))
lines(x1.max$Wavelength, x1.max$Albedo, col="red")
lines(x1.mean$Wavelength, x1.mean$Albedo, col="red")
lines(x2.min$Wavelength, x2.min$Albedo, col="blue")
lines(x2.max$Wavelength, x2.max$Albedo, col="blue")
lines(x2.mean$Wavelength, x2.mean$Albedo, col="blue")

plot(x1.min$Wavelength, 100*x1.min$Albedo/x1.mean$Albedo, type='l', col="red", ylim=c(50,150))
lines(x1.max$Wavelength, 100*x1.max$Albedo/x1.mean$Albedo, col="red")
lines(x2.min$Wavelength, 100*x2.min$Albedo/x2.mean$Albedo, col="blue")
lines(x2.max$Wavelength, 100*x2.max$Albedo/x2.mean$Albedo, col="blue")

plot(x1.min$Wavelength, abs((100*x1.min$Albedo/x1.mean$Albedo)-(100*x2.min$Albedo/x2.mean$Albedo)), type='l', ylim=c(0,5))
lines(x1.max$Wavelength, abs((100*x1.max$Albedo/x1.mean$Albedo)-(100*x2.max$Albedo/x2.mean$Albedo)), lty=2)
# OK, yes min and max value relative to mean differs little when the min, max, and mean value are scaled upward (i.e. Cm halved to increase NIR leaf albedo)



# Function to get summary statistics of PROSPECT simulations -------------------------------------------------------------

wL.samples.summary <- function(wL.samples) {
  
  out <-tibble(wav=wL.samples$Wavelength,                                                            # Wavelength
                 
                 wL.mean=apply(wL.samples$Albedo, 1, mean),                                            # Albedo
                 wL.sd=apply(wL.samples$Albedo, 1, sd),                                             
                 wL.min=apply(wL.samples$Albedo, 1, min),
                 wL.max=apply(wL.samples$Albedo, 1, max),
                 wL.median=apply(wL.samples$Albedo, 1, median))
  
  out <- out %>% mutate(wL.meanminsd=wL.mean-wL.sd,
                        wL.meanplussd=wL.mean+wL.sd,
                        wL.meanmin2sd=wL.mean-2*wL.sd,
                        wL.meanplus2sd=wL.mean+2*wL.sd,
                        
                        wL.medianminsd=wL.median-wL.sd,
                        wL.medianplussd=wL.median+wL.sd,
                        wL.medianmin2sd=wL.median-2*wL.sd,
                        wL.medianplus2sd=wL.median+2*wL.sd)
                 
  return(out)
}


# Get mean, std, etc. of PROSPECT simulations -----------------------------
wL.copula.BIRCH.summary <- wL.samples.summary(wL.copula.BIRCH)
wL.copula.PINE.summary <- wL.samples.summary(wL.copula.PINE)
wL.copula.SPRUCE.summary <- wL.samples.summary(wL.copula.SPRUCE)




# Resample to Sentinel-2 bands --------------------------------------------------
# Use the function that accepts speclib as input, so convert input to speclib object
wL.copula.BIRCH.speclib <- speclib(spectra=t(wL.copula.BIRCH.summary[,-1]), 
                                   wavelength=wL.copula.BIRCH.summary$wav, 
                                   SI=data.frame(Name=names(wL.copula.BIRCH.summary)[-1]))

wL.copula.PINE.speclib <- speclib(spectra=t(wL.copula.PINE.summary[,-1]), 
                                   wavelength=wL.copula.PINE.summary$wav, 
                                   SI=data.frame(Name=names(wL.copula.PINE.summary)[-1]))

wL.copula.SPRUCE.speclib <- speclib(spectra=t(wL.copula.SPRUCE.summary[,-1]), 
                                   wavelength=wL.copula.SPRUCE.summary$wav, 
                                   SI=data.frame(Name=names(wL.copula.SPRUCE.summary)[-1]))


wL.copula.BIRCH.S2A <- my_spectralResampling_speclib(spectra=wL.copula.BIRCH.speclib, sensor="Sentinel2A")
write_rds(wL.copula.BIRCH.S2A, "results/PROSPECT/wL_copula_BIRCH_S2A.rds")

wL.copula.PINE.S2A <- my_spectralResampling_speclib(spectra=wL.copula.PINE.speclib, sensor="Sentinel2A")
write_rds(wL.copula.PINE.S2A, "results/PROSPECT/wL_copula_PINE_S2A.rds")

wL.copula.SPRUCE.S2A <- my_spectralResampling_speclib(spectra=wL.copula.SPRUCE.speclib, sensor="Sentinel2A")
write_rds(wL.copula.SPRUCE.S2A, "results/PROSPECT/wL_copula_SPRUCE_S2A.rds")

# Plot to check. OK
par(mfrow=c(1,3), ps=18)
# BIRCH
plot(subset(wL.copula.BIRCH.S2A, Name=="wL.mean"), ylim=c(0,1), type="b", main="BIRCH", ylab="Albedo")
# plot(subset(wL.copula.SPRUCE.S2A, Name=="wL.median"), new=FALSE, type="b", col="cyan") # Median is the same as mean for all species, in this case
plot(subset(wL.copula.BIRCH.S2A, Name=="wL.min"), new=FALSE, type="b", col="red")
plot(subset(wL.copula.BIRCH.S2A, Name=="wL.max"), new=FALSE, type="b", col="red")
plot(subset(wL.copula.BIRCH.S2A, Name=="wL.meanplussd"), new=FALSE, type="b", col="blue")
plot(subset(wL.copula.BIRCH.S2A, Name=="wL.meanminsd"), new=FALSE, type="b", col="blue")
# plot(subset(wL.copula.BIRCH.S2A, Name=="wL.meanplus2sd"), new=FALSE, type="b", col="green")  # 2sd doesn't make sense, the range is beyond min-max
# plot(subset(wL.copula.BIRCH.S2A, Name=="wL.meanmin2sd"), new=FALSE, type="b", col="green")

# PINE
plot(subset(wL.copula.PINE.S2A, Name=="wL.mean"), ylim=c(0,1), type="b", main="PINE", ylab="Albedo")
plot(subset(wL.copula.PINE.S2A, Name=="wL.min"), new=FALSE, type="b", col="red")
plot(subset(wL.copula.PINE.S2A, Name=="wL.max"), new=FALSE, type="b", col="red")
plot(subset(wL.copula.PINE.S2A, Name=="wL.meanplussd"), new=FALSE, type="b", col="blue")
plot(subset(wL.copula.PINE.S2A, Name=="wL.meanminsd"), new=FALSE, type="b", col="blue")

# SPRUCE
plot(subset(wL.copula.SPRUCE.S2A, Name=="wL.mean"), ylim=c(0,1), type="b", main="SPRUCE", ylab="Albedo")
plot(subset(wL.copula.SPRUCE.S2A, Name=="wL.min"), new=FALSE, type="b", col="red")
plot(subset(wL.copula.SPRUCE.S2A, Name=="wL.max"), new=FALSE, type="b", col="red")
plot(subset(wL.copula.SPRUCE.S2A, Name=="wL.meanplussd"), new=FALSE, type="b", col="blue")
plot(subset(wL.copula.SPRUCE.S2A, Name=="wL.meanminsd"), new=FALSE, type="b", col="blue")


# How much intraspecific variation then? ---------------------------------------
temp <- rbind(round(100 * spectra(subset(wL.copula.BIRCH.S2A, Name=="wL.min")) / spectra(subset(wL.copula.BIRCH.S2A, Name=="wL.mean")), 1),
      round(100 * spectra(subset(wL.copula.BIRCH.S2A, Name=="wL.max")) / spectra(subset(wL.copula.BIRCH.S2A, Name=="wL.mean")), 1),
      
      round(100 * spectra(subset(wL.copula.PINE.S2A, Name=="wL.min")) / spectra(subset(wL.copula.PINE.S2A, Name=="wL.mean")), 1),
      round(100 * spectra(subset(wL.copula.PINE.S2A, Name=="wL.max")) / spectra(subset(wL.copula.PINE.S2A, Name=="wL.mean")), 1),
      
      round(100 * spectra(subset(wL.copula.SPRUCE.S2A, Name=="wL.min")) / spectra(subset(wL.copula.SPRUCE.S2A, Name=="wL.mean")), 1),
      round(100 * spectra(subset(wL.copula.SPRUCE.S2A, Name=="wL.max")) / spectra(subset(wL.copula.SPRUCE.S2A, Name=="wL.mean")), 1))

colnames(temp) <- c("Blue", "Green", "Red", "RE1", "RE2", "RE3", "NIRnarrow", "SWIR1", "SWIR2")
temp <- as_tibble(temp) %>% mutate(name=c("Birch_min", "Birch_max", "Pine_min", "Pine_max", "Spruce_min", "Spruce_max"))

write.csv2(temp, "results/PROSPECT/Intraspecific_variation_Atherton17.csv")

# You know what, now I see the albedo variabiity in NIR and SWIR in different stand structure (light environment)
# is smaller than interspecific difference, so logically applying this intraspecific variation in monospecific stands
# will result in that BRF is not sensitive to leaf albedo. 



