#Bonnie Turek
#ECo 634 - Lab 5

#ricker function plot
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

#exponential curve with same a and b
exp_fun = function(x, a, b)
{
  return(a * exp(-b * x))
}
curve(
  exp_fun(x, 1, 1),
  from = 0, to = 10, add = FALSE, 
  main = "Exponential function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

curve(
  exp_fun(x, 2.2, 1/15), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

#create randomly generated dist.
# Seed the RNG so we can reproduce our results
set.seed(1234567)

# Specify the x-range and number of points:
n_pts = 50
x_min = 2
x_max = 10

# Generate the x-values
x_sim = runif(n_pts, min = x_min, max = x_max)

param_intercept = 2.3
param_slope = 0.67
y_pred = param_intercept + x_sim * param_slope
plot(x_sim, y_pred, main = "Simulated Data\nNo Errors", xlab = "", ylab = "")

#add normal errors 1 (constant variance)
error_mean = 0
error_sd = 0.25

y_observed = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd)
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", 
     xlab = "", ylab = "")
#add normal errors (increasing variance)
error_mean = 0
error_sd = 0.25

y_observed_2 = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd * x_sim)

par(mfrow = c(1, 2))
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", 
     xlab = "", ylab = "")
plot(x_sim, y_observed_2, main = "Normally Distributed Errors\n Increasing Variance", 
xlab = "", ylab = "")
#exponential distributed errors, smaller rate, more spread
y_observed_3 =
  y_pred +
  rexp(n_pts, rate = 1.2)
plot(x_sim, y_observed_3, main = "Exponentially Distributed Errors", 
     xlab = "", ylab = "")

par(mfrow = c(3, 1))
plot(x_sim, y_observed)
plot(x_sim, y_observed_2)
plot(x_sim, y_observed_3)

par(mfrow = c(3, 1))
hist(y_observed - y_pred, main = "sim data 1", xlab = "observed y=values")
hist(y_observed_2 - y_pred, main = "sim data 2", xlab = "observed y=values")
hist(y_observed_3 - y_pred, main = "sim data 3", xlab = "observed y=values")

#real data and lab questions below
#fitting linear, ricker, and exponential function models to data on
#dispersal of juvenile marbled salamanders from their natal ponds 
#to neighboring ponds
require(here)
dat_disp = read.csv(
  here("data", "dispersal.csv")
)
head(dat_disp)

#plot  relationship between juvenile dispersal (disp.rate.ftb) 
#and distance (dist.class).
plot(dat_disp$dist.class,dat_disp$disp.rate.ftb, xlab = "Distance Class", ylab = "Dispersal Rate")
#dispersal rate is a little higher at lower distance classes!
plot(dat_disp$disp.rate.ftb,dat_disp$dist.class, xlab = "Dispersal Rate", ylab = "Distance Class")

#Q1-4 Exponential Functions
exp_fun = function(x, a, b)
{
  return(a * exp(-b * x))
}
#curve 1: a = 1.9, b = 0.1, line color = black, line texture = solid
#curve 2: a = 1.9, b = 0.3, line color = black, line texture = dotted
#curve 3: a = 1.2, b = 0.2, line color = red, line texture = solid
#curve 4: a = 1.2, b = 0.4, line color = red, line texture = dotted
image_file = "lab_05_exp.png"
png(here("images", image_file), width = 1500, height = 1600, res = 180)

curve(
  exp_fun(x, 1.9, 0.1), 
  from = 0, to = 40, ylim = c(0,2), lwd = 3.0,
  main = "Exponential function Curves", xlab= "x", ylab= "f(x)")

curve(
  exp_fun(x, 1.9, 0.3), lwd = 3.0,
  lty = "dotted",add = TRUE,)

curve(
  exp_fun(x, 1.2, 0.2), lwd = 3.0,
  col = "red",add = TRUE,)

curve(
  exp_fun(x, 1.2, 0.4), lwd = 3.0,
  col = "red", lty = "dotted", add = TRUE,)
dev.off()
#Q5-7 Ricker function curves
#curve 1: a = 25, b = 0.1, line color = black, line texture = solid
#curve 2: a = 20, b = 0.2, line color = black, line texture = dotted
#curve 3: a = 10, b = 0.2, line color = black, line texture = dotted
#curve 4: a = 75, b = 0.3, line color = red, line texture = solid
#curve 5: a = 50, b = 0.3, line color = red, line texture = dotted
#curve 6: a = 40, b = 0.3, line color = red, line texture = dotted
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
image_file = "lab_05_ricker.png"
png(here("images", image_file), width = 1500, height = 1600, res = 180)

curve(
  ricker_fun(x, 25, 0.1), lwd = 3.0,
  from = 0, to = 70, add = FALSE, 
  main = "Ricker function curves",
  ylab = "f(x)", xlab = "x")
curve(
  ricker_fun(x, 20, 0.2), lwd = 3.0,
  from = 0, to = 70, add = TRUE, 
  lty = "dotted")
curve(
  ricker_fun(x, 10, 0.2), lwd = 3.0,
  from = 0, to = 70, add = TRUE, 
  lty="dotted")
curve(
  ricker_fun(x, 75, 0.3), lwd = 3.0,
  from = 0, to = 70, add = TRUE, 
  col="red")
curve(
  ricker_fun(x, 50, 0.3), lwd = 3.0,
  from = 0, to = 70, add = TRUE, 
  col="red", lty="dotted")
curve(
  ricker_fun(x, 40, 0.3), lwd = 3.0,
  from = 0, to = 70, add = TRUE, 
  col="red", lty="dotted")
dev.off()

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

#Q8-13 linear
image_file = "lab_05_Q9.png"
png(here("images", image_file), width = 1700, height = 1500, res = 180)

plot(dat_disp$dist.class,dat_disp$disp.rate.ftb, xlab = "Distance Class", ylab = "Dispersal Rate",
     main = "Scatterplot with fitted Linear Model",
     pch = 16, cex = 2.0)
guess_x = 800
guess_y = 0.3
guess_slope = -0.00045
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T, col = "darkblue", lwd = 3.0)
dev.off()

#exponential Q10
image_file = "lab_05_Q11.png"
png(here("images", image_file), width = 1700, height = 1600, res = 180)
plot(dat_disp$dist.class,dat_disp$disp.rate.ftb, xlab = "Distance Class", ylab = "Dispersal Rate",
     main = "Scatterplot with fitted Exponential Model",
     pch = 16, cex = 2.0)
curve(
  exp_fun(x, 75, 0.015), 
  from = 0, to = 1500, ylim = c(0,0.9), add = TRUE,lwd = 3.0,
  col = "darkblue", main = "Exponential function Curves", xlab= "x", ylab= "f(x)")
dev.off()

#ricker model Q12 and 13
image_file = "lab_05_Q13ricker.png"
png(here("images", image_file), width = 1700, height = 1600, res = 180)
plot(dat_disp$dist.class,dat_disp$disp.rate.ftb, xlab = "Distance Class", ylab = "Dispersal Rate",
     main = "Scatterplot with fitted Ricker Model",
     xlim=c(-50,1500), pch = 16, cex = 2.0)
curve(
  ricker_fun(x, 0.0065, 0.0026), 
  from = 0, to = 1500, add = TRUE, lwd = 3.0, col = "darkblue")
dev.off()

#q14 and 15 - still not sure if Residuals look right (some exp are up to -20???)
image_file = "lab_05_Q15.png"
png(here("images", image_file), width = 1500, height = 1800, res = 180)

par(mfrow = c(3,1))

dat_disp$lin_ypred = line_point_slope(dat_disp$dist.class, guess_x, guess_y, guess_slope)
dat_disp$resids_linear = dat_disp$disp.rate.ftb - dat_disp$lin_ypred
hist(dat_disp$resids_linear, main = "Linear Model", xlab = "Residuals")

dat_disp$exp_ypred = exp_fun(dat_disp$dist.class, 75, 0.015)
dat_disp$resids_exp = dat_disp$disp.rate.ftb - dat_disp$exp_ypred
hist(dat_disp$resids_exp, main = "Exponential Model", xlab = "Residuals")  

dat_disp$rick_ypred = ricker_fun(dat_disp$dist.class, 0.0065, 0.0026)
dat_disp$resids_ricker = dat_disp$disp.rate.ftb - dat_disp$rick_ypred
hist(dat_disp$resids_ricker, main = "Ricker Model", xlab = "Residuals")  
dev.off()
