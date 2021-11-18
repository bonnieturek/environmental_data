#Bonnie Turek
#ECo 634 - Lab 7
#10/20/21

#tutorial
# Create simulated data
dat = matrix(1:49, nrow = 7, ncol = 7)
print(dat)
apply(dat, MARGIN = 1, FUN = min)
apply(dat, MARGIN =2, FUN = min)
apply(dat, MARGIN = 1, FUN = max)
apply(dat, MARGIN = 2, FUN = mean)
require(here)
moths = read.csv(here("data", "moths.csv"))
head(moths)
hist(moths$anst, main = "Histogram of Anisota stigma Abundance", xlab = "Abundance")

m = 10000

# numeric() creates an vector of length m with all values initialized to zero
result = numeric(m)
head(result)

for(i in 1:m)
{
  result[i] = mean(sample(moths$anst, replace=TRUE))
}
mean(result)
quantile(result,c(0.025,0.975))

install.packages("boot")
require(boot)
boot(data, statistic, R)
boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}
myboot = boot(data = moths$anst,statistic = boot_mean,R = 10000)
print(myboot)
str(myboot)
mean(moths$anst)
myboot$t0
mean(myboot$t) - myboot$t0
sd(myboot$t)

quantile(
  myboot$t,
  c(0.025, 0.975))

moth_dat = moths[,-1]
head(moth_dat)

#RUNNING BOOTSTRAP

n = nrow(moth_dat) #number of rows or sample observations

m = 100 #number of bootstrap iterations

moth_result = matrix(
  nrow = m,
  ncol = n)


# The outer loop: runs once for each bootstrap iteration.  index variable is i
for(i in 1:m)
{
  # The inner loop: simulates increasing sampling intensity
  # Sampling intensity ranges from 1 site to the complete count of sites (24)
  # index variable is j
  for(j in 1:n)
  {
    # sample the input data row indices, with replacement
    rows_j = sample(n, size = j, replace=TRUE)
    
    # Creates a new data matrix from the resampled rows.
    t1 = moth_dat[rows_j, ]
    
    # Calculates the column sums of the new data matrix.
    t2 = apply(t1, 2, sum)
    
    # Counts the number of columns in which any moths were observed
    moth_result[i, j] = sum(t2 > 0)
  }
}

head(moth_result)

#Create Function for Rarefaction Bootstrap
#DRAFT 1
rarefaction_sampler = function(input_dat, n_iterations)
{
  n = nrow(moth_dat) #number of rows or sample observations
  m = 100 #number of bootstrap iterations
  
  moth_result = matrix(
    nrow = m,
    ncol = n)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:m)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of sites (24)
    # index variable is j
    for(j in 1:n)
    {
      
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = moth_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      moth_result[i, j] = sum(t2 > 0)
    }
  }
  
  return(moth_result)
}

rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

#FUNCTION - DRAFT 2
rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    # index variable is j
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

#TESTING IT in empty console
# This clears the current R session's environment
rm(list = ls())

# Re-read my data:
moths = read.csv(here("data", "moths.csv"))
moth_dat = moths[,-1]

rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    for(j in 1:n_input_rows)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n_input_rows, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}
rarefact = rarefaction_sampler(moths[,-1], 100)
head(rarefact)


#RAREFACTION LAB Q3
#TRY WITH 10,000 iterations
# Re-read my data:
moths = read.csv(here("data", "moths.csv"))
rarefact = rarefaction_sampler(moths[,-1], 10000)
head(rarefact)
length(rarefact)

rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))

#plotting the curve
image_file = "lab_07rarefaction.png"
png(here("images", image_file), width = 1800, height = 1500, res = 180)
matplot(
  rare,
  type='l',
  xlab='Sampling intensity 
  (Number of sampling plots)',
  ylab='Number of moth species',
  col = c("black","dodgerblue","orangered"),
  lty = c(1,1,1),lwd = 3, cex = 1.5,
  main='Rarefaction Curve
  (Expected # moth species vs. sampling intensity)')

legend(
  'bottomright',
  legend=c('Mean','Lower Confidence Interval','Upper Confidence Interval' ),
  lty = c(1,1,1),col=c("black","dodgerblue","orangered"),lwd = 3, 
  cex = 1.5, inset=c(.1,.1))

dev.off()

#LAB QUESTIONS
#parametric confidence intervals
#q1
require(here)
require(palmerpenguins)

sse_mean = function(x) 
{  
  sse = sd(x,na.rm=TRUE)/sqrt(length(x)-sum(is.na(x)))
  return(sse)
}


#subsetting Gentoo penguins and bill length from total penguins data
peng = subset(penguins, species == "Gentoo")
gen_dat = peng[,1:3]
gentoo_dat = gen_dat[,-2]

#determine the value of n (the number of observations/rows)
length(na.omit(gentoo_dat$bill_length_mm))
# can also use
n = length(gentoo_dat$bill_length_mm)-sum(is.na(gentoo_dat$bill_length_mm))

#sample standard deviation
ssd = sd(gentoo_dat$bill_length_mm, na.rm = TRUE)
sample_mean = mean(gentoo_dat$bill_length_mm, na.rm = TRUE)
ssd
sample_mean

alpha = 0.05

sse = sse_mean(gentoo_dat$bill_length_mm)


# 95% confidence, Student t-dist, 5 DF, two-sided
t_lower = qt(alpha / 2, df = n-1)   
t_upper = qt(1 - (alpha/2), df = n-1)
t_crit = abs(qt(alpha / 2, df = n-1))
print(c(`Critical T: lower tail` = t_lower,
        `Critical T: upper tail` = t_upper),
      digits = 3)
#check 95% density is within the upper and lower critical values using the cumulative probability function
pt(t_upper, df = n-1)
pt(t_lower, df = n-1)
pt(t_upper, df = n-1) - pt(t_lower, df = n-1)

#construct Confidence interval
ci_radius = sse * t_crit
ci_radius
sample_mean - ci_radius
sample_mean + ci_radius



#Bootstrap CI

m = 10000
# numeric() creates an vector of length m with all values initialized to zero
result = numeric(m)
head(result)
#perform the bootstrap
for(i in 1:m)
{
  result[i] = mean(sample(gentoo_dat$bill_length_mm, replace=TRUE), na.rm = TRUE)
}

mean(result)
quantile(result,c(0.025,0.975))

require(boot)
boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}
pengboot = 
  boot(
    data = gentoo_dat$bill_length_mm,
    statistic = boot_mean,
    R = 10000)
print(pengboot)
quantile(
  pengboot$t,
  c(0.025, 0.975))


