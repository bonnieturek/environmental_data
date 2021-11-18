#Bonnie Turek
#Lab 8

require(here)
require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))

t.test(flipper_length_mm ~ species, data = penguin_dat, alternative = "less")

install.packages("boot")
require(boot)
install.packages("simpleboot")
require(simpleboot)

pen_dat = penguin_dat[,0:5]
pen_dat2 = pen_dat[, -c(2,3,4)]
adelie_dat = droplevels(subset(pen_dat2, species != "Chinstrap"))
chinstrap_dat = droplevels(subset(pen_dat2, species != "Adelie"))
pen_boot = two.boot(sample1 = adelie_dat$flipper_length_mm, 
                    sample2 = chinstrap_dat$flipper_length_mm, 
                    FUN = mean, R = 10000, na.rm = TRUE)
print(pen_boot)
image_file = "lab_08_penboot_hist.png"
png(here("images", image_file), width = 1800, height = 1500, res = 180)
hist(pen_boot$t, main = "Histogram of Bootstrapped difference in mean flipper length 
of Chinstrap and Adelie penguins",
     xlab = "Differences in Mean Flipper Length (mm)")
dev.off()

sd(pen_boot$t, na.rm = TRUE)
mean(pen_boot$t, na.rm = TRUE)
median(pen_boot$t, na.rm = TRUE)

quantile(pen_boot$t,c(0.025, 0.975), na.rm = TRUE)


#penguin ecdf Q5-7

pen_ecdf = ecdf(pen_boot$t)
pen_ecdf(-4.5)
#mean difference of -4.5 or greater
1 - pen_ecdf(-4.5)
#empirical probability of observing the mean difference 
#predicted by the null hypothesis, i.e. 0 or greater
1 - pen_ecdf(0)

#mean difference of -8 or smaller? 
pen_ecdf(-8)


#Q9 - pine tree/veg data
veg = read.csv(here("data", "vegdata.csv"))
head(veg)

#Use droplevels and subset to keep just the control 
#and treatment levels of the treatment column
dat_tree = droplevels(subset(veg, treatment %in% c("control", "clipped")))

#wilcoxon test on two treatments on pines
wilcox.test(pine ~ treatment, data = dat_tree)

#pines - bootstrap
tree_boot = 
  two.boot(
    subset(dat_tree, treatment == "clipped")$pine,
    subset(dat_tree, treatment == "control")$pine,
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )

# sum(tree_boot$t >= 0)
# sum(tree_boot$t < 0)

boot.ci(tree_boot)
hist(tree_boot$t, main = "Bootstrap sampling distribution")

quantile(tree_boot$t,c(0.025,0.975))
mean(tree_boot$t)
#calc orig data diff in means
dat_control = droplevels(subset(dat_tree, treatment == "control"))
dat_clipped = droplevels(subset(dat_tree, treatment == "clipped"))
control_mean = mean(dat_control$pine)
clipped_mean = mean(dat_clipped$pine)
dif_means = clipped_mean - control_mean
print(dif_means)


#RESAMPLING MODEL COEFFICIENTS
dat_bird = read.csv(here("data", "bird.sub.csv"))
dat_habitat = read.csv(here("data", "hab.sub.csv"))

dat_all = merge(
  dat_bird, 
  dat_habitat,
  by = c("basin", "sub"))

head(dat_all[, c("b.sidi", "s.sidi")])

# Calculate the sample mean and sd:
b_sidi_mean = mean(dat_all$b.sidi, na.rm = TRUE)
b_sidi_sd   = sd(dat_all$b.sidi, na.rm = TRUE)

# Use the subset-by-name symbol ($) to create a 
# new column of z-standardized values.

dat_all$b.sidi.standardized = (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd
mean(dat_all$b.sidi.standardized)
sd(dat_all$b.sidi.standardized)

#standardize s.sidi now
# Calculate the sample mean and sd:
s_sidi_mean = mean(dat_all$s.sidi, na.rm = TRUE)
s_sidi_sd   = sd(dat_all$s.sidi, na.rm = TRUE)

# Use the subset-by-name symbol ($) to create a 
# new column of z-standardized values.

dat_all$s.sidi.standardized = (dat_all$s.sidi - s_sidi_mean)/s_sidi_sd
mean(dat_all$s.sidi.standardized)
sd(dat_all$s.sidi.standardized)

plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")

fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)
slope_observed = coef(fit_1)[2]
abline(fit_1)

dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))

#monte carlo resample
index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]

print(slope_resampled_i)

#plot resampled data
plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)

#resample many times to create null dist
m = 10000 
result = numeric(m) 
for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  
  
  result[i] = coef(fit_resampled_i)[2]
} 

slope_montecarlo = quantile(result, c(.05))
print(slope_montecarlo)
print(slope_observed)

image_file = "lab08_resampSlope_hist.png"
png(here("images", image_file), width = 2100, height = 1500, res = 180)

hist(result, main = "Null Distribution of Regression Slope", xlab = "Slope Parameter")
abline(v = slope_observed, lty = 1, col = "blue", lwd = 2)
abline(v = slope_montecarlo, lty = 2, col = "red", lwd = 2)
dev.off()




  