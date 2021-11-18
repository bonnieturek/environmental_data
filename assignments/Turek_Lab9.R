#Bonnie Turek
#ECo 634 - Lab 9
#11/10/21

require(here)
catrate = read.csv(
  here("data", "catrate.csv"))

head(catrate)

n_success = sum(catrate$success)
n_years = sum(catrate$years)
binom.test(n_success, n_years)
binom.test(n_success, n_years, p = 5/7) 
binom.test(
  n_success,
  n_years,
  p = 5/7,
  alternative='less')

#Fisher’s F test, based on the F-statistic.
#The F-statistic represents the ratio between two variances.
#Fishers test (var.test) assumes normality though
veg = read.csv(here("data", "vegdata.csv"))
head(veg)
boxplot(pine ~ treatment, data = veg)

var.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control','clipped'))

#check if data actually normal
#p is equal to or less than 0.05, then the hypothesis of normality
#will be rejected
shapiro.test(veg$pine[veg$treatment=="control"])
shapiro.test(veg$pine[veg$treatment=="clipped"])
#control is normal ; clipped is non-normal

#non-normal then we use non-parametric fligner test
fligner.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control','clipped'))

#test of variances with more than two groups
bartlett.test(pine ~ treatment, data=veg)

fligner.test(pine ~ treatment, data = veg)

t.test(pine~treatment,data=veg,subset=treatment %in% c('control','clipped'), conf.int=TRUE)

wilcox.test(pine~treatment,data=veg,subset=treatment %in% c('control','clipped'), conf.int=TRUE)

#paired t-tests
control = veg$pine[veg$treatment=='control']
clipped = veg$pine[veg$treatment=='clipped']

t.test(control, clipped, paired=TRUE)

wilcox.test(control, clipped, paired=TRUE)


#SALAMANDER DATA
disp = read.csv(here("data", "dispersal.csv"))
disp

plot(disp$disp.rate.ftb, disp$disp.rate.eb)
#normal dist. correlation test
cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs')

#non-normal correlation Spearman test
cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs',
  method='spearman')

plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE)

plot(
  ecdf(disp$disp.rate.eb),
  verticals=TRUE,
  lty=3,
  add=TRUE)

#KS test: difference in value of the cumulative distribution 
#functions; i.e., maximum vertical difference in the curves 
#for a given value of X
ks.test(disp$disp.rate.ftb,disp$disp.rate.eb)

#sex-linked road killings for salamanders
prop.test(c(4,16),c(40,250))
#change values test
prop.test(c(8,32),c(80,500))

owls = matrix(c(16, 9, 4, 11), nrow=2)
rownames(owls) = c("present", "absent")
colnames(owls) = c("old", "young")
chisq.test(owls)

fisher.test(owls)


#bird and habitat data
birds   = read.csv(here("data", "bird.sta.csv"))
hab     = read.csv(here("data", "hab.sta.csv"))
birdhab = merge(birds, hab, by=c("basin", "sub", "sta"))

# Create a contingency table for edge/interior and brown creeper presence/absence
table(birdhab$s.edge, birdhab$BRCR > 0)

# set the presence to be in the first column
br_creeper_table = table(birdhab$s.edge, birdhab$BRCR > 0)[, 2:1]

br_creeper_table
chisq.test(br_creeper_table)

#Q3 - 5
require(palmerpenguins)
fit_fl_sp = 
  lm(
    formula = flipper_length_mm ~ species,
    data = penguins)

#q3
fit_species =   
  lm(
  formula = body_mass_g ~ species,
  data = penguins)

#q4
fit_sex =   
  lm(
    formula = body_mass_g ~ sex,
    data = penguins)
#q5
fit_both =   
  lm(
    formula = body_mass_g ~ sex * species,
    data = penguins)

#Q6 - 9
boxplot(body_mass_g ~ species,
        data = penguins,
        ylab = "Body Mass (g)",
        xlab = "Species", main = "Boxplot of Body Mass based on Penguin Species")

boxplot(body_mass_g ~ sex,
        data = penguins,
        ylab = "Body Mass (g)",
        xlab = "Sex", main = "Boxplot of Body Mass based on Penguin Sex")

boxplot(body_mass_g ~ sex * species,
        data = penguins,
        ylab = "Body Mass (g)",
        xlab = "",
        names = c("female\nAdelie", "male\nAdelie", "female\nChinstrap", "male\nChinstrap", "female\nGentoo", "male\nGentoo"),
        las = 2,
        main = "Boxplot of Body Mass based on Penguin Species and Sex")

bartlett.test(body_mass_g ~ species,
              data = penguins)

bartlett.test(body_mass_g ~ sex,
              data = penguins)

#q13 and 14
#extract bodymass by island (need by species)
datpen_species = aggregate(
  body_mass_g ~ species,
  data = penguins,
  FUN = c)
str(datpen_species)

datpen_sex = aggregate(
  body_mass_g ~ sex * species,
  data = penguins,
  FUN = c)
str(datpen_sex)

bartlett.test(body_mass_g ~ interaction(sex, species), data = penguins)


bartlett.test(datpen_sex$body_mass_g)


