require(here)
dat_catrate <- read.csv(here("data", "catrate.csv"))
dat_delomys <- read.csv(here("data", "delomys.csv"))
dat_rope <- read.csv(here("data", "rope.csv"))

head(dat_catrate)
head(dat_delomys)
head(dat_rope)

plot(dat_delomys$body_mass, dat_delomys$body_length, 
     main = "Striped Atlantic Forest Rat: Body Mass vs. Body Length 
     Bonnie Turek", 
     xlab = "Body Mass", ylab = "Body Length",
     cex = 0.6, col = "blue")
