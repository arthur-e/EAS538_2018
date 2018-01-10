setwd("~/Desktop/STATS")
### EXERCISE 1
x<-Rays_SP$ERA
x
list("Krithika", "Sampath", "SNRE", 40726384)
matrix(data= c(31:60), nrow=3, ncol= 10, byrow = FALSE, dimnames = NULL)

### EXERCISE 2
hist(Rays_SP$K9, probability = TRUE)
lines (density(Rays_SP$K9, na.rm = TRUE), col= "green")
shapiro.test(Rays_SP$K9)

### EXERCISE 3
data("iris")
hist(iris$Sepal.Length)
lines(density(iris$Sepal.Length, na.rm = TRUE), col= "green")
shapiro.test(iris$Sepal.Length)

hist(iris$Sepal.Width)
lines(density(iris$Sepal.Width, na.rm = TRUE), col= "red")
shapiro.test(iris$Sepal.Width)

hist(iris$Petal.Length)
lines(density(iris$Petal.Length, na.rm = TRUE), col= "yellow")
shapiro.test(iris$Petal.Length)

hist(iris$Petal.Width)
lines(density(iris$Petal.Width, na.rm = TRUE), col= "blue")
shapiro.test(iris$Petal.Width)