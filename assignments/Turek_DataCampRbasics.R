a = "Bonnie"
class(a)

b1 = 45.6
class(b1)

b2 = "45.6"
class(b2)

b1 + b2

c1 = c(1,2,3)
c1
class(c1)

b1 + c1

v1 = c(-2:2)
v1

v2 = v1*3
v2

sum(v2)

vec_4 = c(1:12)
vec_4
mat_1 = matrix(vec_4,nrow = 3, ncol = 4, byrow = TRUE)
mat_1

mat_2 = matrix(vec_4,nrow = 3, ncol = 4, byrow = FALSE)
mat_2

my_list_1 = list("two" = 5.2, "one" = "five point two", "three" = 0:5)
my_list_1

my_list_1$three

my_list_1["one"]

my_vec = rep(1:3, 5)
my_vec

my_bool_vec = my_vec == 3
my_bool_vec

data.frame(my_vec, my_bool_vec)

my_vec[my_bool_vec == TRUE]


