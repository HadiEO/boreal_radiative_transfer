SEPC <- function(pred, obs) {
  bias <- mean(pred - obs)
  error <- pred - obs - bias
  sqrt(mean(error^2))
}