#Bonnie Turek
#ECo 634 - Lab 10

require(here)
rm(list = ls())

rope = read.csv(
  here("data", "rope.csv"))


rope$rope.type <- factor(rope$rope.type)
class(rope$rope.type)
levels(rope$rope.type)

length(rope$blade)
nrow(rope)
n_obs = nrow(rope)

length(levels(rope$rope.type))
n_groups = length(levels(rope$rope.type))

#calculate the total sum of squares
#SST = Σ(yi – ybar)^2
ss_tot = sum((rope$p.cut - mean(rope$p.cut))^2)
df_tot = n_obs-1

par(mfrow= c(1,2))
boxplot(rope$p.cut, main = "Percent Rope Cut
        of all rope types", ylab = "Percent Rope Cut", xlab = "All rope types")
boxplot(rope$p.cut ~ rope$rope.type, main = "Percent Rope Cut
of different rope types", ylab = "Percent Rope Cut", xlab = "")

#define the function to get residuals
resid_fun = function(x)
  {
  (x - mean(x))
}

#create aggregate of residuals within group
agg_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = resid_fun)
str(agg_resids)

sum_sq_resid = function(x)
{
  sum((x - mean(x))^2)
}
agg_sq_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = sum_sq_resid)
str(agg_sq_resids)
  
ss_within = sum(agg_sq_resids$x)
df_within = n_obs - n_groups

ss_among = ss_tot - ss_within

df_among = n_groups - 1


ms_among  =  ss_among / (n_groups - 1)
ms_within = ss_within / (n_obs - n_groups)

# F-ratio, defined as the among-group variance divided by 
#the within-group variance.

f_ratio = ms_among / ms_within
f_pval = pf(f_ratio, (n_groups - 1), (n_obs - n_groups), lower.tail = FALSE)



fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)

anova_fit_1 = anova(fit_1)
str(anova_fit_1)

#check our sum of squared resids (ss_among and ss_within)
anova_fit_1$"Sum Sq"



#CHECK OUR ANOVA BY HAND

# number comparison tolerance
digits_check = 5

# Build the reference model using R functions
fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)
anova_fit_1 = anova(fit_1)

# Check degrees of freedom
anova_fit_1$Df == c(df_among, df_within)

# Check sums of squares
round(anova_fit_1$`Sum Sq`, digits = digits_check) == round(c(ss_among, ss_within), digits = digits_check)

# Check mean squares
round(anova_fit_1$`Mean Sq`, digits = digits_check) == round(c(ms_among, ms_within), digits = digits_check)

# Check the F-ratio
round(anova_fit_1$`F value`[1], digits = digits_check) == round(f_ratio, digits = digits_check)

# Check the F test statistic p-value
round(anova_fit_1$`Pr(>F)`[1], digits = digits_check) == round(f_pval, digits = digits_check)


#Bartlett Test
bartlett.test(p.cut ~ rope.type, data = rope)

#Q5-7
fit_rope_1 = lm(p.cut ~ rope.type, data = rope)
summary(fit_rope_1)

#check q6 and q7
mean(rope$p.cut[which(rope$rope.type == "XTC")])
mean(rope$p.cut[which(rope$rope.type == "BLAZE")])

