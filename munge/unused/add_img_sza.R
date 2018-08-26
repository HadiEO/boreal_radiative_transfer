# Add image SZA -----------------------------------------------------------

add_img_sza <- function(data) {
  
  data$img_sza <- NA
  data[data$scene.Date=="2782012", "img_sza"] <- 28.18
  data[data$scene.Date=="1122012", "img_sza"] <- 35.53
  data[data$scene.Date=="17122012", "img_sza"] <- 37.76

  return(data)
  
}

