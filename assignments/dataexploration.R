install.packages("here")
require("here")
here()

file.exists(here("data", "hab.sta.csv"))

dat_habitat <- read.csv(here("data", "hab.sta.csv"))

range(dat_habitat$elev)
mean(dat_habitat$slope)

par(mfrow = c(3, 1))
hist(dat_habitat$slope, breaks = 7, main = "Histogram of sampling site slope", xlab = "Percent Slope", col = "purple")
hist(dat_habitat$elev, breaks = 5, main = "Histogram of sampling site elevation", xlab = "Elevation (m)", col = "blue")
hist(dat_habitat$aspect, main = "Histogram of sampling site aspect", xlab = "Aspect (degrees)", col = "magenta")

# Calculates the value of y for a linear function, given the coordinates
# of a known point (x1, y1) and the slope of the line.
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

#slope plot
plot(x = dat_habitat$slope, 
     y = dat_habitat$ba.tot, 
     main = "Basal Area and Slope", 
     xlab = "Percent Slope", 
     ylab = "Total Basal Area",
     cex = 0.4,
     col = "purple",
     pch = "o")
data_center_x_s = mean(dat_habitat$slope)
data_center_y = mean(dat_habitat$ba.tot)
points(x = data_center_x_s, y = data_center_y, col = "black")
curve(
  line_point_slope(
    x, 
    data_center_x_s, 
    data_center_y,
    0.3), 
  add = TRUE)

#elevation plot
plot(x = dat_habitat$elev, 
     y = dat_habitat$ba.tot, 
     main = "Basal Area and Elevation", 
     xlab = "Elevation (m)", 
     ylab = "Total Basal Area",
     cex = 0.4,
     col = "blue",
     pch = "o")
data_center_x_e = mean(dat_habitat$elev)
data_center_y = mean(dat_habitat$ba.tot)
points(x = data_center_x_e, y = data_center_y, col = "black")
curve(
  line_point_slope(
    x, 
    data_center_x_e, 
    data_center_y,
    0.06), 
  add = TRUE)

#aspect plot
plot(x = dat_habitat$aspect, 
     y = dat_habitat$ba.tot, 
     main = "Basal Area and Aspect", 
     xlab = "Aspect (degrees)", 
     ylab = "Total Basal Area",
     cex = 0.4,
     col = "magenta",
     pch = "o")
data_center_x_a = mean(dat_habitat$aspect)
data_center_y = mean(dat_habitat$ba.tot)
points(x = data_center_x_a, y = data_center_y, col = "black")
curve(
  line_point_slope(
    x, 
    data_center_x_a, 
    data_center_y,
    0.06), 
  add = TRUE)

