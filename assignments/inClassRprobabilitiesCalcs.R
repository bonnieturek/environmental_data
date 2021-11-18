#in class r prob calculations
dpois(x = 7, lambda = 10.4)
dpois(x = 8, lambda = 10.4)

dbinom(x = 4,size = 6,prob = 2/3)

dbinom(x = 0,size = 6,prob = 2/3)

pbinom(q = 4, size = 6, prob = 2/3)

#prob of 3 or fewer
pbinom(q = 3, size = 6, prob = 2/3)
#prob of 4 or more using law of total probability
1 - pbinom(q = 3, size = 6, prob = 2/3)

#test prob of 4 or more
dbinom(x = 4,size = 6,prob = 2/3) +
dbinom(x = 5,size = 6,prob = 2/3) +
dbinom(x = 6,size = 6,prob = 2/3)