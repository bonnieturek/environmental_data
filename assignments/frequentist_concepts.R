#Bonnie Turek
#Lecture Individual Assignment
#Frequentist Concepts

#q1 - exactly 3
dbinom(x = 3,size = 4,prob = 0.75)
#q2 - 3 or fewer
pbinom(q = 3, size = 4, prob = 0.75)
#q3 - more than 3 successes (4 or more)
1 - pbinom(q = 3, size = 5, prob = 0.75)

#check prob more than 3
dbinom(x = 4,size = 5,prob = 0.75) +
dbinom(x = 5,size = 5,prob = 0.75)

#q4 - prob of less than 1.2
pnorm(q = 1.2, mean = 2, sd = 2)

#q5 - prob of greater than 1.2
1 - pnorm(q = 1.2, mean = 2, sd = 2)
#check 
pnorm(q = 1.2, mean = 2, sd = 2, lower.tail = FALSE)
pnorm(q = 3.2, mean = 2, sd = 2, lower.tail = FALSE)

#q6 - prob between 1.2 and 3.2
#prob of greater than 1.2
val1 = 1 - pnorm(q = 1.2, mean = 2, sd = 2)
#prob of greater than 3.2
val2 = 1 - pnorm(q = 3.2, mean = 2, sd = 2)
#add them up
totalP = val1 - val2
totalP
factorial(80)
factorial(81)

1328400-1312000
410*40*80
factorial(25)
sqrt(16400)