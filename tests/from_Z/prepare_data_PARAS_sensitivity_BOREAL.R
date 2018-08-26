require(hsdar)
require(randtoolbox)   
require(sensitivity)
source("R_GSA/script/sobolMultOut.R")
require(plyr)
require(tidyverse)


# Preparation -------------------------------------------------------------

# Run 2 cases: (1) Just high LAI to be comparable to the tropics data
#              (2) All LAI variation
# Tree leaf : just one species, either spruce or pine
# Rg vary between 3 site fertility types (xeric, mesic, herb-rich)

# Preprocessing leaf spectra:                                                  # Need to adjust PARAS function to use wL as input
# (1) Albedo for each sample, leaf side : reflectance + transmittance
# (2) Albedo for each sample : average leaf side (adaxial & abaxial)
# (3) Mean, sd, min, max Albedo from all samples

# Select species = Norway spruce
boreal.LAI <- read_csv2("R_GSA/data/finland_LAI_1000.csv")
boreal.LAI %>% dplyr::select(dom_species) %>% table # 301 spruce & 653 pine, 139 birch, 1 logdepole pine
boreal.LAI %>% group_by(dom_species) %>% 
  dplyr::summarize(LAI.mean = mean(LAI_effective), LAI.std = sd(LAI_effective),
                   LAI.min = min(LAI_effective), LAI.max = max(LAI_effective)) 
#      dom_species LAI.mean   LAI.std LAI.min LAI.max
# <chr>    <dbl>     <dbl>   <dbl>   <dbl>
# 1          Birch 1.500863 1.2300804    0.00    4.52
# 2 Logdepole pine 2.190000       NaN    2.19    2.19
# 3  Norway spruce 2.406711 0.8255799    0.15    4.26
# 4     Scots pine 1.493522 0.6676926    0.00    3.58
# Spruce has widest forest structure variation

boreal.LAI.sp <- dplyr::filter(boreal.LAI, dom_species == "Norway spruce")
boreal.LAI.sp <- boreal.LAI.sp %>%  dplyr::filter(LAI_effective > 2.15)                 # <================== Restrict LAIe *********************


# Calculate PARAS structural inputs --------------------------------------------------
# LAIe, CI, DIFN, i0, cgf.view                      # Note : CI range taken from another study 
                                                    #        sun position same as tropics ring 2
boreal.paras.input <- boreal.LAI.sp %>% 
  transmute(LAIe = LAI_effective, 
            DIFN = 0.066*GAPS1 + 0.189*GAPS2 + 0.247*GAPS3 + 0.249*GAPS4 +  # based on LAI-2000
              0.249*GAPS5,
            i0 = 1 - GAPS2,
            cgf.view = GAPS1)

LAIe.range <- range(boreal.paras.input$LAIe)                                          # range for forest plots
DIFN.range <- range(boreal.paras.input$DIFN)
i0.range <- range(boreal.paras.input$i0)
cgf.view.range <- range(boreal.paras.input$cgf.view)

CI.range <- c(0.5 - 2*0.05, 0.5 + 2*0.05) # beta = N(0.5, 0.05^2) in Varvia et al. (2017); LAIe = beta * LAI


# Calculate PARAS spectral inputs -----------------------------------------
# Preprocessing leaf spectra:
# (1) Albedo for each sample, leaf side : reflectance + transmittance
# (2) Albedo for each sample : average leaf side (adaxial & abaxial)
# (3) Mean, sd, min, max Albedo from all samples

spruce.albedo <- read_csv2("R_GSA/data/spruce_albedo.csv")                                  # Leaf albedo

spruce.albedo.ETM <- spruce.albedo %>%  dplyr::select(-one_of("Wl")) %>%                     # Resample to ETM+
  map(function(x) {
    spclib = hsdar::speclib(x, spruce.albedo$Wl)
    to.ETM = hsdar::spectralResampling(spclib, "Landsat7")           
    spc.ETM = to.ETM@spectra
    wL = c(b1 = spc.ETM[1,1], b2 = spc.ETM[1,2], b3 = spc.ETM[1,3], b4 = spc.ETM[1,4], 
               b5 = spc.ETM[1,5], b7 = spc.ETM[1,6])
    return(wL)
})


wL.b1.range <- c(spruce.albedo.ETM$Min[1], spruce.albedo.ETM$Max[1])
wL.b2.range <- c(spruce.albedo.ETM$Min[2], spruce.albedo.ETM$Max[2])
wL.b3.range <- c(spruce.albedo.ETM$Min[3], spruce.albedo.ETM$Max[3])
wL.b4.range <- c(spruce.albedo.ETM$Min[4], spruce.albedo.ETM$Max[4])
wL.b5.range <- c(spruce.albedo.ETM$Min[5], spruce.albedo.ETM$Max[5])
wL.b7.range <- c(spruce.albedo.ETM$Min[6], spruce.albedo.ETM$Max[6])


# Understory reflectance
ustory.R <- read_csv2("R_GSA/data/finland_understory_HDRF.csv")                              # Understory reflectance
ustory.R.ETM <- ustory.R %>%  dplyr::select(-one_of("Wavelength")) %>%                      # Resample to ETM+
  map(function(x) {
    spclib = hsdar::speclib(x, ustory.R$Wavelength)
    to.ETM = hsdar::spectralResampling(spclib, "Landsat7")           # Resample to ETM+
    spc.ETM = to.ETM@spectra
    Rg = c(b1 = spc.ETM[1,1], b2 = spc.ETM[1,2], b3 = spc.ETM[1,3], b4 = spc.ETM[1,4], 
           b5 = spc.ETM[1,5], b7 = spc.ETM[1,6])
    return(Rg)
  }) %>% as_tibble()  

ustory.R.ETM <- ustory.R.ETM %>% mutate(Min = apply(ustory.R.ETM, 1, min), Max = apply(ustory.R.ETM, 1, max))


Rg.b1.range <- c(ustory.R.ETM$Min[1], ustory.R.ETM$Max[1])
Rg.b2.range <- c(ustory.R.ETM$Min[2], ustory.R.ETM$Max[2])
Rg.b3.range <- c(ustory.R.ETM$Min[3], ustory.R.ETM$Max[3])
Rg.b4.range <- c(ustory.R.ETM$Min[4], ustory.R.ETM$Max[4]) 
Rg.b5.range <- c(ustory.R.ETM$Min[5], ustory.R.ETM$Max[5])
Rg.b7.range <- c(ustory.R.ETM$Min[6], ustory.R.ETM$Max[6])


inputs.range <- tibble(
  Var = c("LAIe", "CI", "DIFN", "i0", "cgf.view", "wL.b1", "wL.b2", "wL.b3", "wL.b4", "wL.b5", "wL.b7", "Rg.b1", "Rg.b2", "Rg.b3", "Rg.b4", "Rg.b5", "Rg.b7"),
  Min = c(LAIe.range[1], CI.range[1], DIFN.range[1], i0.range[1], cgf.view.range[1], 
          wL.b1.range[1], wL.b2.range[1], wL.b3.range[1], wL.b4.range[1], wL.b5.range[1], wL.b7.range[1],
          Rg.b1.range[1], Rg.b2.range[1], Rg.b3.range[1], Rg.b4.range[1], Rg.b5.range[1], Rg.b7.range[1]),
  Max = c(LAIe.range[2], CI.range[2], DIFN.range[2], i0.range[2], cgf.view.range[2], 
          wL.b1.range[2], wL.b2.range[2], wL.b3.range[2], wL.b4.range[2], wL.b5.range[2], wL.b7.range[2],
          Rg.b1.range[2], Rg.b2.range[2], Rg.b3.range[2], Rg.b4.range[2], Rg.b5.range[2], Rg.b7.range[2])
)


write.csv2(inputs.range, "R_GSA/result/GSAinputs_boreal_dense.csv")


