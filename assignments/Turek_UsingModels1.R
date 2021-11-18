#bonnie turek
#ECo 602 - Individual Assign: Using Models 1
#11/2/2021

require(here)
catrate = read.csv(
  here("data", "catrate.csv"))

head(catrate)
summary(catrate)
image_file = "catrate_hist.png"
png(here("images", image_file), width = 1800, height = 1500, res = 180)

hist(catrate$cat.rate, 
     main = "Catastrophic Rates of Salamander Reproduction",
     xlab = "Catastrophic Rate")
dev.off()
shapiro.test(catrate$cat.rate)
#the output of shapiro test on cat.rate provides strong evidence 
#that the cat.rate does NOT follow a normal distribution
t.test(catrate$cat.rate, mu = 2/7)

t.test(catrate$cat.rate, alternative = "greater", mu = 2/7)
t.test(catrate$cat.rate, alternative = "less", mu = 2/7)

wilcox.test(catrate$cat.rate, mu = 2 / 7)
wilcox.test(catrate$cat.rate, alternative = "greater", mu = 2 / 7)
wilcox.test(catrate$cat.rate, alternative = "less", mu = 2 / 7)
#shows similar results in that typical cat rate is greater than
#the late pond filling 0.28 value

require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
summary(penguin_dat)

boxplot(
  flipper_length_mm ~ species, 
  data = penguin_dat,
  ylab = "Flipper Length (mm)")

# Extract the Adelie and Chinstrap penguin data
dat_adelie = subset(penguin_dat, species == "Adelie")
dat_chinstrap = subset(penguin_dat, species == "Chinstrap")
#conduct normality tests
shapiro.test(dat_adelie$flipper_length_mm)
shapiro.test(dat_chinstrap$flipper_length_mm)

image_file = "usingmodels1_peng_hist.png"
png(here("images", image_file), width = 2100, height = 1200, res = 180)
par(mfrow = c(1, 2))
hist(dat_adelie$flipper_length_mm, main = "Histogram of Adelie Penguin
     Flipper Length", xlab = "Flipper Length (mm)")
hist(dat_chinstrap$flipper_length_mm, main = "Histogram of Chinstrap Penguin
     Flipper Length", xlab = "Flipper Length (mm)")
dev.off()

t.test(dat_adelie$flipper_length_mm, dat_chinstrap$flipper_length_mm)
t.test(flipper_length_mm ~ species, data = penguin_dat)
levels(penguin_dat$species)
#Adelie is the base level

wilcox.test(flipper_length_mm ~ species, data = penguin_dat)
#one way test - greater than Adelie (base level)
wilcox.test(flipper_length_mm ~ species, data = penguin_dat,
            alternative = "greater")
#one way test - less than Adelie (base)
wilcox.test(flipper_length_mm ~ species, data = penguin_dat,
            alternative = "less")





