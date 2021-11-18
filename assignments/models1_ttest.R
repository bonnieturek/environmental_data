#Bonnie Turek
#11/2/21
#ECo 602 - Models 1: T-test

require(palmerpenguins)
dat_ade = droplevels(subset(penguins, species == "Adelie"))
hist(dat_ade$body_mass_g, main = "Adelie Penguins: Body Mass", xlab = "body mass (g)")

#create Q1 boxplot
image_file = "inclass_pen_boxplot.png"
png(here("images", image_file), width = 1800, height = 1500, res = 180)
boxplot(dat_ade$body_mass_g ~ dat_ade$sex, 
        main = "Adelie Penguins Body Mass based on Sex", 
        xlab = "Sex", ylab = "Body Mass")
dev.off()

#Q2 t-test
dat_ade_female = droplevels(subset(dat_ade, sex == "female"))
dat_ade_male = droplevels(subset(dat_ade, sex == "male"))
t.test(dat_ade_female$body_mass_g, mu = 0)

#Q4 - one tail test on male body mass 
t.test(dat_ade_male$body_mass_g, alternative = c("greater"), mu = 4000)

#Q6 - two tail test
t.test(dat_ade_female$body_mass_g, dat_ade_male$body_mass_g)
t.test(dat_ade$body_mass_g ~ dat_ade$sex)
#either of those work

