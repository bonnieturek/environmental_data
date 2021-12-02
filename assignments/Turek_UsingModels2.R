#Bonnie Turek
#ECo 602 - Using Models 2
#11/22/21

#install.packages("palmerpenguins")
require(palmerpenguins)
t.test(subset(penguins, species == "Gentoo")$flipper_length_mm)

t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218
)

t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218,
  alternative = "less"
)

t.test(flipper_length_mm ~ species, data = subset(penguins, species != "Chinstrap"))
#default in one sided is "alternative = less"

#PERFORM ANOVA
#graphical exploration
par(mfrow = c(1, 2))
hist(penguins$body_mass_g, breaks = 80, main = "histogram of body mass", xlab = "body mass (g)")
plot(density(penguins$body_mass_g, na.rm = TRUE), main = "density plot of body mass")

require(palmerpenguins)
boxplot(body_mass_g ~ species, data = penguins)

#numerical exploration
dat_chinstrap = subset(penguins, species == "Chinstrap")
mean(dat_chinstrap$body_mass_g, na.rm = TRUE)
shapiro.test(dat_chinstrap$body_mass_g)
#Shapiro test null hypothesis: “The data are drawn from a normally-distributed population.”
#Chinstrap is normally distributed

#aggregate means and shapiro tests
aggregate(body_mass_g ~ species, data = penguins, FUN = mean)
aggregate(
  body_mass_g ~ species,
  data = penguins,
  FUN = function(x) shapiro.test(x)$p.value)
#adelie non-normal, chinstrap and gentoo are normal

fit_species = lm(body_mass_g ~ species, data = penguins)
summary(fit_species)
#model coeff table p-value in each row of the table is a 
#significance test for whether the coefficient in that 
#row is different from zero...

anova(fit_species)

boxplot(body_mass_g ~ sex * species,
        data = penguins,
        ylab = "Body Mass (g)",
        xlab = "",
        names = c("female\nAdelie", "male\nAdelie", "female\nChinstrap", "male\nChinstrap", "female\nGentoo", "male\nGentoo"),
        las = 2,
        main = "Boxplot of Body Mass based on Penguin Species and Sex")

#sex could also be important predictor variable (other than species)
#Two-way interactive ANOVA

#two-way, additive model
fit_additive = lm(body_mass_g ~ sex + species, data = penguins)
summary(fit_additive)

#two-way, factoral model
fit_factorial = lm(body_mass_g ~ sex * species, data = penguins)
summary(fit_factorial)

anova(fit_factorial)

lm(bill_length_mm ~ body_mass_g, data = penguins)


#LAB QUESTIONS
fit_both = lm(formula = body_mass_g ~ sex * species, data = penguins)
summary(fit_both)

aggregate(body_mass_g ~ sex * species, data = penguins, FUN = mean)

