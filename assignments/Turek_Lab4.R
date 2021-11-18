#Bonnie Turek
#ECo 634 - Lab 4
#09/29/21

?dnorm()
dnorm(0, mean=0, sd=1)
pnorm(0, mean=0, sd=1)

# Generate a vector of x-values
x = seq(-3, 3, length.out = 1000)
y = dnorm(x)
# mean and sd are kept at defaults of 0 and 1
plot(x, y, main = "Normal PDF", type = "l")
abline(h = 0)
?abline

require(palmerpenguins)
hist(
  penguins$body_mass_g,
  main = "Histogram of Penguin Body Mass",
  xlab = "Body Mass (g)")

mean(penguins$body_mass_g, na.rm = TRUE)
sd(penguins$body_mass_g, na.rm = TRUE)
nrow(penguins)

dat_1 = rnorm(n = 344, mean = 4202, sd = 802)
dat_2 = rnorm(n = 344, mean = 4202, sd = 802)
dat_3 = rnorm(n = 344, mean = 4202, sd = 802)
dat_4 = rnorm(n = 344, mean = 4202, sd = 802)
par(mfrow = c(2, 2))

hist(dat_1)
hist(dat_2)
hist(dat_3)
hist(dat_4)

set.seed(12)
dat_unif = runif(n = 270, min = 0, max = 4)
hist(dat_unif)

set.seed(1)
dat_unif_1 = runif(n = 270, min = 0, max = 4)
set.seed(1)
dat_unif_2 = runif(n = 270, min = 0, max = 4)

par(mfrow = c(1, 2))
hist(dat_unif_1)
hist(dat_unif_2)
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
set.seed(123)
n = 17
slope = 0.7
intcp = 0.2

guess_x = 6
guess_y = 4
guess_slope = 0.72

x = runif(n = n, min = 1, max = 10)
y = rnorm(n = n, mean = slope * x + intcp)

plot(x, y, pch = 16)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

#random nums in dataframe
set.seed(123)
n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)
guess_x = 6
guess_y = 0
guess_slope = 0.1

plot(y_observed ~ x, data = dat, pch = 8)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)
dat$y_predicted = line_point_slope(dat$x, guess_x, guess_y, guess_slope)

dat$resids = (dat$y_observed - dat$y_predicted)
sum(dat$resids)
sum(abs(dat$resids))
#The smaller the residual sum of squares, 
#the better your model fits your data; 
#the greater the residual sum of squares, 
#the poorer your model fits your data.

plot(dat$x, dat$resids)
hist(dat$resids)


#LAB QUESTIONS
#Q1
#norm_17, norm_30, norm_300, and norm_3000
set.seed(12)
my_mean = 10.4
my_sd = 2.4
norm_17 = rnorm(n = 17, mean = my_mean, sd = my_sd)
norm_30 = rnorm(n = 30, mean = my_mean, sd = my_sd)
norm_300 = rnorm(n = 300, mean = my_mean, sd = my_sd)
norm_3000 = rnorm(n = 3000, mean = my_mean, sd = my_sd)

#Q2
require(here)
image_file = "lab_04_hist_01.png"
png(here("images", image_file), width = 1500, height = 1600, res = 180)

par(mfrow = c(2, 2))
hist(norm_17, main = "Normal Distribution\n17 random values", cex = 5, col = "steelblue")
hist(norm_30, main = "Normal Distribution\n30 random values", cex = 5, col = "steelblue")
hist(norm_300, main = "Normal Distribution\n300 random values", cex = 5, col = "steelblue")
hist(norm_3000, main = "Normal Distribution\n3000 random values", cex = 5, col = "steelblue")

dev.off()

#Q7 and Q8
# Generate a vector of x-values
x = seq(-35, 35, length.out = 1000)
y = dnorm(x, mean = 10.4, sd = 2.4)
image_file = "images/norm_1.svg"
svg(filename = image_file, width = 7, height = 7)
plot(x, y, main = "Normal PDF: Mean=10.4 StDev=2.4", type = "l", xlim = c(2, 20))
abline(h = 0)
dev.off()

#Q9 and 10
image_file = "lab_04_ques10.png"
png(here("images", image_file), width = 1500, height = 1600, res = 180)

par(mfrow = c(2, 2))
set.seed(90)
n_pts = 125
my_size = 20
x = rbinom(n = n_pts, size = my_size, prob = 0.3)
dat = data.frame(x = x, y_observed = rnorm(n_pts))
plot(dat$x, dat$y_observed, xlab = "Binomial Distribution", 
     ylab = "Normal Distribution",
     main = "Scatterplot of Binomial X\n and Normal Y",
     col = adjustcolor("goldenrod", alpha.f = 0.7), cex = 1.5, pch = 10)

hist(dat$x, xlab = "X", main = "Histogram of binomial dist.\n X values", col = "purple")
hist(dat$y_observed, xlab = "Y", main = "Histogram of normal dist.\n Y values", col = "cyan")
boxplot(dat, col = "darkblue", main = "Boxplot of X and Y")
dev.off()

#Q11 and 12
image_file = "lab_04_ques12.png"
png(here("images", image_file), width = 1500, height = 1600, res = 180)

set.seed(6)
n_pts = 90
x_min = 1
x_max = 100
x = runif(n = n_pts, min = x_min, max = x_max)
dat = data.frame(x = x, y_observed = rnorm(n_pts))
plot(dat$x, dat$y_observed, pch = 8, col = "seagreen", cex = 2,
     xlab = "X random (rand. unif)", ylab = "Y random (rand. norm)", main = "Scatterplot of Random Data:\n X Uniformly Distributed\n Y Normally Distributed")
guess_x = 55
guess_y = 0
guess_slope = 0.01
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T, col = "darkblue")
dev.off()

#Q13 and 14
dat$y_predicted = line_point_slope(dat$x, guess_x, guess_y, guess_slope)
dat$resids = (dat$y_observed - dat$y_predicted)

plot(dat$y_predicted, dat$resids, xlab = "Predicted Values", ylab = "Residuals",pch = 18, 
     col = adjustcolor("seagreen", alpha.f = 0.8), 
     cex = 3.5, main = "Scatterplot of predicted values and residuals")
hist(dat$resids, xlab = "Residuals", main = "Histogram of Residuals", col = "rosybrown")

