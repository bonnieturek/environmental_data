#gingko data exploration
#bonnie turek

install.packages("here")
require(here)
rm(list = ls())

gingko = read.csv(
  here("data", "gingko.csv"))

boxplot(gingko$notch ~ gingko$seeds_present, ylab = "Gingko Notch Depth (mm)", xlab = "Gingko seeds present", main = "Boxplot of Gingko Notch depth
        based on seeds present")

plot(gingko$max_depth, gingko$max_width, ylab = "Maximum Leaf Width (mm)", 
     xlab = "Maximum Leaf Depth (mm)",
     main = "Maximum Gingko leaf depth vs. maximum leaf width",pch = 19,
     col = as.numeric(factor(gingko$ï..site_id)))

mean(gingko$max_width)
mean(gingko$max_width, na.rm = TRUE)

mean(gingko$p)