
library(MASS)
data(Animals)
head(Animals)

Animals$body
mean(Animals$body)
sd(Animals$body)

plot(x=Animals$body, y=Animals$brain)

data_center_x = mean(Animals$body)
data_center_y = mean(Animals$brain)
c(data_center_x, data_center_y)

plot(x = Animals$body, y = Animals$brain)
points(x = data_center_x, y = data_center_y, col = "red")

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

plot(x = Animals$body, y = Animals$brain,main = "Bonnie's Animal Scatter Plot", xlab ="Animals Body", ylab = "Animals Brain", xlim = c(0,15000) )
points(x = data_center_x, y = data_center_y, col = "red")
curve(
  line_point_slope(
    x, 
    data_center_x, 
    data_center_y,
    -0.8), 
  add = TRUE)



