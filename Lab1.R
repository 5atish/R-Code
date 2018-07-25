
x <- matrix(c(1,2,3,4)) # Matrix with 1 column
x

x <- matrix(c(1,2,3,4),4,1) # Matrix with 4 row, 1 column
x

x <- matrix(c(1,2,3,4),1,4) # Matrix with 1 row, 4 column
x

x <- matrix(c(1,2,3,4),2,2) # Matrix with 2 row, 2 column
x

x <- matrix(c(1,2,3,4),2,2, byrow = TRUE) # Matrix with rowise filling the column
x

y <- matrix(c(0,3,2,1), 2, 2) 

x*y ### Matrix multification
x/y ### Matrix division
x+y ### Matrix addition
x-y ### Matrix subtraction

k <- rnorm(10, mean = .25, sd = 2) # rnorm() function generate vector of standard normal random variable with first argument n sample size with mean 0 and standard deviation 1.
k

j <- k + rnorm(10)
j

cor(k,j) # cor() to compute correlation between k and j

set.seed(41) ## set.seed() function reproduce exact same set of random                    numbers

p <- rnorm(40)
p
mean(p)
var(p)
sd(p)

k <- rnorm(40)
k

plot(p,k, xlab = "This is x-axis", ylab = "This is y-axis", main = "Plot p vs k", las = 3, pch = 9 , lwd = 2, col = "red")

x <- seq(-pi, pi, length = 20)
x

a <- matrix(1:16,4,4)
a

a[2:4,c(2,4)]
a[c(1,3),c(2,4)]

getwd()
setwd("D:/Royal Holloway University/Data Analysis/2017/Lab Work/Lab_practice")


Auto <- read.table("D:/Royal Holloway University/Data Analysis/2017/Lab Work/Lab_practice/Auto_data.txt", header = TRUE, na.strings = "?") ## an.string tells that R that any time it see the perticular character in double quote treat it as missing value.

View(Auto)
str(Auto)
dim(Auto)
Auto <- na.omit(Auto) ## na.omit function removes the rows with missing                           observation
names(Auto)
attach(Auto) ## attach function tells R that variable in dataframe                        available by name
plot(cylinders,mpg)
cylinders <- as.factor(cylinders) ## as.factor converts quantitative                                        variable into qualitative variable
plot(cylinders, mpg, col = "green", xlab = "Cylinders", ylab = "MPG")
hist(mpg, col = 2, breaks = 15)

pairs(Auto) ## pair function create scatterplot matrix
pairs(~mpg + cylinders + displacement + acceleration)

identify(horsepower,mpg,name) ## identify provide useful interactive method for identifying the value of a particular variable for point on a plot e.g. plot(x axis variable, y axis variable, variable whos value we would like to see printed for each point)

summary(Auto)  ## summary function produces a numerical summary of each                     variable in perticular dataset.


###1.Which of the variables are quantitative, and which are qualitative? (Answer this just looking at the data; there is no need to use any R commands.)

### origin, name are qualitative varables

# mpg, cylinders, displacement, horsepower, weight, acceleration, year are quantitative variables

###2.What is the range of each quantitative variable? You can answer this
##using the range() function.

range(mpg)
range(cylinders)
range(displacement)
range(horsepower)
range(weight)
range(acceleration)
range(year)

###3.What is the mean and standard deviation of each quantitative variable?

mean(mpg)
mean(cylinders)
mean(displacement)
mean(horsepower)
mean(weight)
mean(acceleration)
mean(year)

sd(mpg)
sd(cylinders)
sd(displacement)
sd(horsepower)
sd(weight)
sd(acceleration)
sd(year)

###4.Now remove all observations from the 10th to 85th. 

Auto_a <- Auto[c(-10,-85),]
str(Auto_a)

###What is the range,mean, and standard deviation of each quantitative variable in the subset of the data that remains?

sd(Auto_a$mpg)
sd(Auto_a$cylinders)
sd(Auto_a$displacement)
sd(Auto_a$horsepower)
sd(Auto_a$weight)
sd(Auto_a$acceleration)
sd(Auto_a$origin)

mean(Auto_a$mpg)
mean(Auto_a$cylinders)
mean(Auto_a$displacement)
mean(Auto_a$horsepower)
mean(Auto_a$weight)
mean(Auto_a$acceleration)
mean(Auto_a$origin)

range(Auto_a$mpg)
range(Auto_a$cylinders)
range(Auto_a$displacement)
range(Auto_a$horsepower)
range(Auto_a$weight)
range(Auto_a$acceleration)
range(Auto_a$origin)

###5.Using the full data set, investigate the quantitative variables graphically, using scatterplots or other tools of your choice. Create some plots highlighting the relationships among the variables. Comment on your findings.

pairs(Auto)

# There are very close positive relation among displacement, horsepower and weight

# Plot of Origin vs year is particularly uninformative

###6.Suppose that we wish to predict gas mileage ( mpg) on the basis of the other variables. Do your plots suggest that any of the other variables might be useful in predicting mpg? Justify your answer.

pairs(Auto)

## displacement, horsepower, weight are useful in predicting mpg