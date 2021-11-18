
here::here()
getwd()
here()
read.csv(here("data", "bird.sta.csv"))

for (i in 1:10)
{print(i)}

print_number = function(n)
{
  print(paste0("The value of the number is ", n))
}
print_number(123)

rnorm(10)
rnorm(10)
rnorm(sd = 1, mean = 15, n = 10)
?rnorm

n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)

vec_2 <- vec_1 == 3
vec_1[vec_2]

length(vec_1)
sum(vec_1 == 3)

n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))

#Q7
for (i in 1:10)
{
  print(paste0("This is loop iteration:", i))
}

#Q8
n = 3
vec_1 = sample(12, n, replace = TRUE)
for (i in 1:n)
{
  print(paste0("The element of vec_1 at index ", i, " is ", vec_1[i],"."))
}

#Q9
create_and_print_vec = function(n, min = 1, max = 10)
{
  vec_new = sample(min:max, n, replace = TRUE)
  print(vec_new)
  for (i in 1:n)
  print(paste0("The element at index ", i, " is ", vec_new[i],"."))
}
create_and_print_vec(10, 100, 2000)