# source("cache/libPaths.R")
require(hsdar)
require(randtoolbox)   
require(sensitivity)
source("R_GSA/script/sobolMultOut.R")
require(plyr)
require(tidyverse)


#########################################################################################################
###################################### Main calculation from here #######################################
#########################################################################################################
require("doSNOW")                                 # <-------------------------------- Set up parallelization
require("doParallel")

detectCores()
cl <- makeCluster(7)                                     
registerDoParallel(cl)
getDoParWorkers()
clusterEvalQ(cl, library(doParallel))                      # Check if lib is installed in all cores

# Make parameter samples # ================================================================================================
source("R_GSA/script/prepare_data_PARAS_sensitivity_BOREAL.R")               # Get the parameter range, check if change 

set.seed(123); sobolsamp.paras.varymaininput <-  
  parameterSets(par.ranges = # Generate 2 * p columns for Monte Carlo SI estimation
   list(LAIe.range, CI.range, DIFN.range, i0.range, cgf.view.range,                          # left matrix = X1
        wL.b1.range, wL.b2.range, wL.b3.range, wL.b4.range, wL.b5.range, wL.b7.range,        # Now wL 
        Rg.b1.range, Rg.b2.range, Rg.b3.range, Rg.b4.range, Rg.b5.range, Rg.b7.range,
        
        LAIe.range, CI.range, DIFN.range, i0.range, cgf.view.range,                          # right matrix = X2
        wL.b1.range, wL.b2.range, wL.b3.range, wL.b4.range, wL.b5.range, wL.b7.range,
        Rg.b1.range, Rg.b2.range, Rg.b3.range, Rg.b4.range, Rg.b5.range, Rg.b7.range),
 
 samples = 20000, method = "sobol")              #<----------------------------- No. of sobol samples 


varnames <-  c("LAIe", "CI", "DIFN", "i0", "cgf.view", 
               "wL.b1", "wL.b2", "wL.b3", "wL.b4", "wL.b5", "wL.b7",
               "Rg.b1", "Rg.b2", "Rg.b3", "Rg.b4", "Rg.b5", "Rg.b7")

colnames(sobolsamp.paras.varymaininput) <- rep(varnames, 2)

# saveRDS(sobolsamp.paras.varymaininput, "R_GSA/result/sobolsamp_paras_varymaininput_20000_BOREAL_limLAI.rds") 
saveRDS(sobolsamp.paras.varymaininput, "R_GSA/result/sobolsamp_paras_varymaininput_20000_BOREAL.rds") 


source("R_GSA/script/fun_paras_varymaininput_BOREAL.R")          # source the function <==================================



sobolfun.paras.varymaininput <- function(X) {   # Function to pass to sobol's model          
  n <- dim(X)[[1]]
  feval <- foreach (i = 1:n, .combine = "rbind", .export = c("paras.varymaininput.wL"), .packages = c("hsdar", "plyr")) %dopar% {                            
    paras.varymaininput.wL(                                                               # Change the custom function accordingly
      LAIe = X[i, 1], CI= X[i, 2], DIFN = X[i, 3], i0 = X[i, 4], cgf.view = X[i, 5],
      wL.b1 = X[i, 6], wL.b2 = X[i, 7], wL.b3 = X[i, 8], wL.b4 = X[i, 9], wL.b5 = X[i, 10], wL.b7 = X[i, 11],
      Rg.b1 = X[i, 12], Rg.b2 = X[i, 13], Rg.b3 = X[i, 14], Rg.b4 = X[i, 15], Rg.b5 = X[i, 16], Rg.b7 = X[i, 17])
  }
}


# Run SI monte carlo computation # ==============================================================================================
t <- proc.time()
sobolSI.paras.varymaininput <- sobolMultOut(model = sobolfun.paras.varymaininput, q = 6,        # q = 6 bands PARAS output
                                            X1 = sobolsamp.paras.varymaininput[, 1:17],         # with wL, 17 inputs in total
                                            X2 = sobolsamp.paras.varymaininput[, (17+1):(2*17)],
                                            MCmethod="soboljansen",                                    # which sobol method ?
                                            plotFct=TRUE)                              # plotFct=TRUE to return SI for each output                       

saveRDS(sobolSI.paras.varymaininput, "R_GSA/result/sobolSI_paras_varymaininput_20000_BOREAL_limLAI.rds")
print(proc.time() - t)


# Save model intermediate outputs -----------------------------------------
source("R_GSA/script/fun_interm_paras_varymaininput_BOREAL.R") # <========================= source the function 
# out <- c(BRF, LAIt, p, q, wC, Q) 

paras.interm.varymaininput.par <- function(X) {   # <=============================== Parallelize         
  n <- dim(X)[[1]]
  feval <- foreach (i = 1:n, .combine = "rbind", .export = c("paras.interm.varymaininput.wL"), .packages = c("hsdar", "plyr")) %dopar% {                            
    paras.interm.varymaininput.wL(        # <======================== custom function (sourced) to return intermediate inputs
      LAIe = X[i, 1], CI= X[i, 2], DIFN = X[i, 3], i0 = X[i, 4], cgf.view = X[i, 5],
      wL.b1 = X[i, 6], wL.b2 = X[i, 7], wL.b3 = X[i, 8], wL.b4 = X[i, 9], wL.b5 = X[i, 10], wL.b7 = X[i, 11],
      Rg.b1 = X[i, 12], Rg.b2 = X[i, 13], Rg.b3 = X[i, 14], Rg.b4 = X[i, 15], Rg.b5 = X[i, 16], Rg.b7 = X[i, 17])
  }
}

sobol.null <- soboljansen(model = NULL, 
                          X1 = sobolsamp.paras.varymaininput[, 1:17], X2 = sobolsamp.paras.varymaininput[, (17+1):(2*17)])
y.interm.varymaininput <- paras.interm.varymaininput.par(sobol.null$X)
colnames(y.interm.varymaininput) <- c("BRF.b1", "BRF.b2", "BRF.b3", "BRF.b4", "BRF.b5", "BRF.b7",
                                      "LAIt", "p", "q", 
                                      "wC.b1", "wC.b2", "wC.b3", "wC.b4", "wC.b5", "wC.b7",
                                      "Q.b1", "Q.b2", "Q.b3", "Q.b4", "Q.b5", "Q.b7")


# saveRDS(y.interm.varymaininput, "R_GSA/result/y_interm_varymaininput_20000_BOREAL_limLAI.rds")
saveRDS(y.interm.varymaininput, "R_GSA/result/y_interm_varymaininput_20000_BOREAL.rds")

# Stop parallelization
stopCluster(cl)                                                                                # Stop parallelization
