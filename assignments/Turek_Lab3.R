#Bonnie Turek - ECo 634 - Lab 3 
install.packages("psych")
require(psych)
pairs.panels(iris)

require(here)
dat_bird = read.csv(
  here("data", "bird.sta.csv")
)
head(dat_bird)

dat_habitat = read.csv(
  here("data", "hab.sta.csv")
)
head(dat_habitat)

dat_all = merge(dat_bird, dat_habitat)
plot(ba.tot ~ elev, data = dat_all)

terrain = dat_habitat[, c(6,7,8,17)]
head(terrain)
pairs.panels(terrain, main = "Terrain variables and Basal Area")



#Fitting logistic curves
# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}


# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}

sum(dat_all$BUSH)
my_vec1 = dat_all$BUSH > 0
bush_present_absent = as.numeric(my_vec1)
plot(x = dat_all$ba.tot, y = bush_present_absent, main = "Bushtit presence based on basal area",
     ylab = "Bushtit Presence", xlab = "Basal Area (m2/ha)",
     cex = 2.5, pch = 16,col = adjustcolor("magenta",0.09))
curve(logistic_midpoint_slope(x, midpoint = 40, slope = -0.25), add = TRUE)

sum(dat_all$HAWO)
my_vec2 = dat_all$HAWO > 0
hawo_present_absent = as.numeric(my_vec2)
plot(x = dat_all$ba.tot, y = hawo_present_absent, 
     main = "Hairy Woodpecker presence based on basal area",
     ylab = "Hairy Woodpecker Presence", xlab = "Basal Area (m2/ha)",
     cex = 2.5, pch = 16,col = adjustcolor("royalblue", 0.09) )
curve(logistic_midpoint_slope(x, midpoint = 70, slope = -0.15), add = TRUE)

colors()



#gray jay questions
sum(dat_all$GRJA)

#calculate the total number of sites Gray Jays present at sites
my_vec_GJ = dat_all$GRJA > 0
grja_present_absent = as.numeric(my_vec_GJ)
sum(my_vec_GJ)
sum(grja_present_absent)
#can either use sum function on BOOLEAN True/False vector or 0/1 numeric


