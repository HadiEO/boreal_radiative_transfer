library(jpeg)

# img <- readJPEG("graphs/jpg/atherton2017_specific_leaf_area.JPG")
# img <- readJPEG("graphs/jpg/atherton2017_total_chlorophyll.JPG")
img <- readJPEG("graphs/jpg/atherton2017_water_content.JPG")

x11()

plot(1:2, type='n')                                             # 1:2 is the axis (x & y) values of the plot
graphics::rasterImage(img, xleft = 1, ybottom = 1, xright = 2, ytop = 2)  # Need to call plot() first

calpoints <- locator(n=4,type='p',pch=4,col='blue',lwd=2)       # Record (click) 4 calibration points, first 2 in x axis, next 2 in y axis. Avoid 0.
as.data.frame(calpoints)

data <- locator(type='p',pch=1,col='red',lwd=1.2,cex=1.2)       # Digitize (click) along the curve in Figure. Right click to stop.
as.data.frame(data)

# Define the function
calibrate = function(calpoints, data, x1, x2, y1, y2)           # Function to convert values on graphic display into true data points values needed
{
  x  <- calpoints$x[c(1,2)]
  y  <- calpoints$y[c(3,4)]
  cx <- lm(formula = c(x1,x2) ~ c(x))$coeff
  cy <- lm(formula = c(y1,y2) ~ c(y))$coeff
  data$x <- data$x*cx[2]+cx[1]
  data$y <- data$y*cy[2]+cy[1]
  return(as.data.frame(data))
}

# Run the function
true.data.SLA <- calibrate(calpoints = calpoints, data = data, x1 = 1, x2 = 11, y1 = 20, y2 = 180)
write.csv2(true.data.SLA, "graphs/jpg/atherton2017_specific_leaf_area.csv")

true.data.TC <- calibrate(calpoints = calpoints, data = data, x1 = 1, x2 = 11, y1 = 20, y2 = 80)
write.csv2(true.data.TC, "graphs/jpg/atherton2017_total_chlorophyll.csv")

true.data.WC <- calibrate(calpoints = calpoints, data = data, x1 = 2, x2 = 10, y1 = 0.48, y2 = 0.62)
write.csv2(true.data.WC, "graphs/jpg/atherton2017_water_content.csv")
