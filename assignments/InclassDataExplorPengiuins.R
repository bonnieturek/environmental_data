#In class data exploration 2 activity - Palmer Penguins

install.packages("palmerpenguins")
require(palmerpenguins)
require(here)
class(penguins)
penguins = data.frame(penguins)
mean(penguins$body_mass_g)
head(penguins)
mean(penguins$body_mass_g, na.rm = TRUE)
summary(penguins)

#Boxplot example
par(mfrow = c(1, 2))
boxplot(penguins$bill_depth_mm, ylab = "Bill Depth (mm)")
boxplot(bill_depth_mm ~ sex, data = penguins, ylab = "Bill Depth (mm)", xlab = "Sex")


#Coplot example
coplot(body_mass_g ~ bill_length_mm | sex, data = penguins, xlab="Bill Length (mm)", ylab="Body mass (g)")
#checking the range of the flipper length conditioning variable
range(penguins$flipper_length_mm, na.rm = TRUE)


#Clear plot console using dev.off() in the CONSOLE
#you might have to do this two times
plot(penguins$body_mass_g, penguins$bill_depth_mm)