#Bonnie Turek - ECo 634
#Lab 6: Introduction to Inference

require(palmerpenguins)
#create standard error of the mean function

sse_mean = function(x, na.rm=FALSE) 
{
  sse = sd(x,na.rm=TRUE)/sqrt(length(!is.na(x)))
  return(sse)
}
sse_mean(penguins$bill_depth_mm)

boxplot(flipper_length_mm ~ species, data = penguins)

dat_pen = subset(penguins, species != "Gentoo")
boxplot(flipper_length_mm ~ species, data = dat_pen)

dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  par(mfrow = c(1, 2))
  boxplot(flipper_length_mm ~ species, data = penguins)
  boxplot(flipper_length_mm ~ species, data = dat_pen)
}

boxplot(flipper_length_mm ~ species, data = dat_pen)

# for reproducibility
set.seed(123)

flipper_shuffled = sample(penguins$flipper_length_mm, replace = TRUE)
par(mfrow = c(1, 2))
boxplot(flipper_length_mm ~ species, data = penguins)
boxplot(flipper_shuffled ~ penguins$species, xlab = "species")

t.test(dat_pen$flipper_length_mm ~ dat_pen$species)

#using sample() to resample gets rid of the association between two species
set.seed(1)
flipper_shuffled = sample(dat_pen$flipper_length_mm)
boxplot(flipper_shuffled ~ dat_pen$species)

t_test_1 = t.test(flipper_shuffled ~ dat_pen$species)
t_test_1
t_test = t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
t_test
t_test$estimate
diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)

agg_means = aggregate(
  flipper_length_mm ~ species, 
  data = dat_pen, 
  FUN = mean, 
  na.rm = TRUE)
diff_observed = diff(agg_means[, 2])

agg_means
diff_observed

table(dat_pen$species)

n_1 = 68
n_2 = 152

dat_1 = sample(dat_pen$flipper_length_mm, n_1, replace = TRUE)
dat_2 = sample(dat_pen$flipper_length_mm, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
print(c(observed = diff_observed, simulated = diff_simulated))

x = dat_pen$flipper_length_mm
n_1 = 68
n_2 = 152

dat_1 = sample(x, n_1, replace = TRUE)
dat_2 = sample(x, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

two_group_resample = function(x, n_1, n_2)
{
  dat_1 = sample(x, n_1, replace = TRUE)
  dat_2 = sample(x, n_2, replace = TRUE)
  
  diff_simulated = 
    mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
  return(diff_simulated)
}
  
set.seed(54321)
two_group_resample(dat_pen$flipper_length_mm, 68, 152)  


n = 2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences)
sum(abs(mean_differences) >= diff_observed)

t_test = t.test(flipper_shuffled ~ dat_pen$species)
str(t_test)
t_test$estimate
  
#LAB QUESTIONS
#q1
rm(list = ls())
sse_mean = function(x) 
{
  
  sse = sd(x,na.rm=TRUE)/sqrt(length(x)-sum(is.na(x)))
  return(sse)
}
sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)

#q2
two_group_resample = function(x, n_1, n_2)
{
  dat_1 = sample(x, n_1, replace = TRUE)
  dat_2 = sample(x, n_2, replace = TRUE)
  
  difference_in_means = 
    mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
  return(difference_in_means)
}
set.seed(54321)
two_group_resample(dat_pen$flipper_length_mm, 68, 152)  


#q4

n = 2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
image_file = "lab_06_Q4.png"
png(here("images", image_file), width = 1500, height = 1800, res = 180)

hist(mean_differences, main="Histogram of Resampled Differences of Means
     Flipper Length (mm) between Adelie and Chinstrap penguins",
     cex.main = 1.6,
     xlab="Mean Differences",
     cex.lab = 1.5, cex.axis = 1.5)
dev.off()
sum(abs(mean_differences) >= 5.8)

#q7

boxplot(dat_pen$bill_length_mm ~ dat_pen$species, main = "Boxplot of Penguin Bill Length",
        xlab = "Penguin Species", ylab = "Bill Length (mm)")

#q8 and 9
t_test = t.test(dat_pen$bill_length_mm ~ dat_pen$species)
t_test
t_test$estimate
diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)

agg_means = aggregate(
  bill_length_mm ~ species, 
  data = dat_pen, 
  FUN = mean, 
  na.rm = TRUE)
diff_crit = diff(agg_means[, 2])

agg_means
diff_crit

#q10
two_group_resample = function(x, n_1, n_2)
{
  dat_1 = sample(x, n_1, replace = TRUE)
  dat_2 = sample(x, n_2, replace = TRUE)
  
  difference_in_means = 
    mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
  return(difference_in_means)
}

n = 1000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$bill_length_mm, 68, 152)
  )
}

sum(abs(mean_differences) >= diff_crit)
require(here)
image_file = "lab_06hists.png"
png(here("images", image_file), width = 1500, height = 1800, res = 180)
hist(mean_differences, main="Histogram of Resampled Differences of Means
     Bill Length (mm) in Adelie and Chinstrap Penguins",
     cex.main = 1.6,
     xlab="Mean Differences",
     cex.lab = 1.5, cex.axis = 1.5)
dev.off()
