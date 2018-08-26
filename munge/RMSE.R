
# Normalize by mean observed ----------------------------------------------

RMSD <- function(pred, obs) {
  D <- pred - obs
  sqrt(mean(D^2))
}

nRMSD <- function(pred, obs) {
  D <- pred - obs
  RMSD <- sqrt(mean(D^2)) 
  (RMSD / mean(obs)) * 100
}

MD <- function(pred, obs) {
  D <- pred - obs                               # difference = relative to observed
  mean(D)
}

nMD <- function(pred, obs) {
  D <- pred - obs
  MD <- mean(D)
  (MD / mean(obs)) * 100
}

RMSDC <- function(pred, obs) {
  D <- pred - obs
  MD <- mean(D)
  error.bc <- pred - obs - MD             # bias corrected
  sqrt(mean(error.bc^2)) 
}

nRMSDC <- function(pred, obs) {
  D <- pred - obs
  MD <- mean(D)
  error.bc <- pred - obs - MD             # bias corrected
  RMSDC <- sqrt(mean(error.bc^2))
  (RMSDC / mean(obs)) * 100
}



# Normalize by range of obs -----------------------------------------------

nrMD <- function(pred, obs) {
  D <- pred - obs
  MD <- mean(D)
  range.obs <- max(obs) - min(obs)
  (MD / range.obs) * 100
}


nrRMSD <- function(pred, obs) {
  D <- pred - obs
  RMSD <- sqrt(mean(D^2)) 
  range.obs <- max(obs) - min(obs)
  (RMSD / range.obs) * 100
}

nrRMSDC <- function(pred, obs) {
  D <- pred - obs
  MD <- mean(D)
  error.bc <- pred - obs - MD             # bias corrected
  RMSDC <- sqrt(mean(error.bc^2))
  range.obs <- max(obs) - min(obs)
  (RMSDC / range.obs) * 100
}

# Normalized by mean of simulated values, also called relative RMSE (http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0066972)
# Normalize by mean observed ----------------------------------------------

nsimRMSD <- function(pred, obs) {
  D <- pred - obs
  RMSD <- sqrt(mean(D^2)) 
  (RMSD / mean(pred)) * 100
}


nsimMD <- function(pred, obs) {
  D <- pred - obs
  MD <- mean(D)
  (MD / mean(pred)) * 100
}


nsimRMSDC <- function(pred, obs) {
  D <- pred - obs
  MD <- mean(D)
  error.bc <- pred - obs - MD             # bias corrected
  RMSDC <- sqrt(mean(error.bc^2))
  (RMSDC / mean(pred)) * 100
}


# # Each point relative to obs (?) ----------------------------------------------
# 
# relByPoint.MD <- function(pred, obs) {
#   D <- pred - obs
#   MD <- mean(D)
# 
#   (MD / range.obs) * 100
# }
# 
# 
# relByPoint.RMSD <- function(pred, obs) {
#   D <- pred - obs
#   RMSD <- sqrt(mean(D^2)) 
# 
#   (RMSD / range.obs) * 100
# }
# 
# relByPoint.RMSDC <- function(pred, obs) {
#   D <- pred - obs
#   MD <- mean(D)
#   error.bc <- pred - obs - MD             # bias corrected
#   RMSDC <- sqrt(mean(error.bc^2))
# 
#   (RMSDC / range.obs) * 100
# }

