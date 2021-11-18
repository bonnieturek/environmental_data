#in class likelihoods

require(here)
dat_bird = read.csv(here::here("data", "bird.sta.csv"))
dat_habitat = read.csv(here::here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)

summary(dat_all$WIWA)

hist(dat_all$WIWA)
hist(dat_all$WIWA, breaks = 7)
hist(dat_all$WIWA, breaks = 0:7 - .5)

par(mfrow = c(1, 2))
dat = dat_all$WIWA
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of\nWilson's Warbler counts")

dat = dat_all$GRJA
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of\nGray Jay counts")

sum(log(dpois(x = dat_all$WIWA, lambda = 1.0)))
#maximize sum of log likelihoods for observing WIWA
wiwa_counts = c(2, 6)
sum(log(dpois(x = wiwa_counts, lambda = 4.00)))

#initialize winter wren data
datWIWR = dat_all$WIWR
#plot winter wren histogram
hist(datWIWR, breaks = 0:(max(datWIWR) + 1) - 0.5, 
     main = "Histogram of\nWinter Wren counts",
     xlab = "Winter Wren Count")
# calculate Poisson log likelihood
sum(log(dpois(x = datWIWR, lambda = 1.457)))

# plug and play with n and prob in the dbimon function
sum(log(dbinom(x = datWIWR, size = 6, prob = 0.95)))

