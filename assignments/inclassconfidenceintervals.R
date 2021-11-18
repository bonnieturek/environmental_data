#Bonnie Turek
#In class confidence intervals

qnorm(c(0.025, 0.975))
qnorm(c(0.05, 0.95))

n = 11
alpha = 0.05

# 95% confidence, Student t-dist, 10 DF, two-sided
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


t_crit = abs(qt(alpha / 2, df = 473))
print(t_crit)


sse_mean = function(x) 
{  
  sse = sd(x,na.rm=TRUE)/sqrt(length(x)-sum(is.na(x)))
  return(sse)
}
# 95% confidence, Student t-dist, 5 DF, two-sided
n = 50
mean = 10
sd = 3.14
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
sse = sd/sqrt(n)
ci_radius = sse * t_crit
ci_radius
mean - ci_radius
mean + ci_radius
