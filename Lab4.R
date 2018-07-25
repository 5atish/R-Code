Loadlibraries <- function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been print.")
}

Loadlibraries()

curve(x*sin(x^2),-3,3) ## to plot the function x sin(x^2) between x=-3 and x=3

x <- seq(-3,3,0.01)
y <- x*sin(x^2)
plot(x,y)


cube <- function(x){ return(x^4)}
cube(6)


normr <- function(x,y=0){
  square <- x^2+y^2 
  return(sqrt(square))
}

normr(2)




######################################################################
######################## 5 Normalization in R #########################
######################################################################

library(class)
dim(Caravan)
attach(Caravan)
summary(Caravan)

348/5822 #[1] 0.05977327 i.e. 0.6% people have purchase the Caravan Insurance


str(Caravan)
View(Caravan)
?Caravan


var(Caravan[,1]) ## varaince of Caravan column 1 is 165.0378
sd (Caravan[,1]) ## Standard deviation of Caravan column 1 is 12.84671
mean(Caravan[,1]) ## mean of Caravan column 1 is 24.25335
var(Caravan[,2]) ## varaince of Caravan column 2 is 0.1647078
sd (Caravan[,2]) ## Standard deviation of Caravan column 2 is 0.4058421
mean(Caravan[,2]) ## mean of Caravan column 1 is 1.110615

###########################################################################
#Standardization of data makes all varaible mean 0 and standard deviation1#
###########################################################################

standard.x <- scale(Caravan[,-86])
View(standard.x)
var(standard.x[,1]) ## varaince of standard.x of column 1 is 1
sd(standard.x[,1]) ## standard deviation of standard.x of column 1 is 1
mean(standard.x[,1])

var(standard.x[,2]) ## varaince of standard.x of column 2 is 1
sd(standard.x[,2]) ## standard deviation of standard.x of column 2 is 1
mean(standard.x[,2])

## Now split the data into first 1000 samples as test set and remaining as training set

test <- 1:1000
test.x <- standard.x[test,]
test.y <- Purchase[test]
train.x <- standard.x[-test,]
train.y <- Purchase[-test]

set.seed(2345)

knn.pred <- knn(train.x,test.x,train.y,k=1)
table(knn.pred,test.y)
mean(knn.pred!=test.y) ## 0.117
mean(test.y!="Yes") ## 0.941
mean(test.y!="No") ## 0.059 

knn.pred <- knn(train.x,test.x,train.y,k=3)
table(knn.pred,test.y)
5/24 ## 0.2083333
mean(knn.pred!=test.y) ## 0.073

knn.pred <- knn(train.x,test.x,train.y,k=5)
table(knn.pred,test.y)
4/15 #0.2666667
mean(knn.pred!=test.y) ## 0.066

knn.pred <- knn(train.x,test.x,train.y,k=10)
table(knn.pred,test.y)
mean(knn.pred!=test.y) ## 0.058

### As we increase the k success rate is increasing and it appears knn finds some problem finding real pattern in difficult data set 


### Let's impliment logistic regression model with cut of rate is 0.5

glm.fit <- glm(Purchase~., data = Caravan, family = binomial,subset =-test)
glm.prob <- predict(glm.fit,Caravan[test,], type = "response")
glm.pred <- rep("No", 1000)
glm.pred[glm.prob > .5] <- "Yes"
table(glm.pred, test.y)

## All we got 7 test observation are predicted to purchase insurance that though wrong observation i.e (Yes/No) and (Yes/Yes) is 0

## let's try with predicted probablity of purchase exceeds 0.25

glm.pred <- rep("No", 1000)
glm.pred[glm.prob > .25] <- "Yes"
table(glm.pred, test.y)

##         test.y
#glm.pred  No Yes
#     No  919  48
#     Yes  22  11

11/33 ## 0.3333333  i.e We predict 33% people will predicted to purchase insurance

######################################################################
######################## 6 Exercise ##################################
######################################################################

##1.Write a function, Power(), that prints out the result of raising 2 to the 3rd power. 

powerr <- function(x){return(x^3)}
powerr(2)


##2.Create a new function, Power2(), that allows you to pass any two numbers,x and a, and prints out the value of x^a. 

power2 <- function(x,a){return(x^a)}
power2(3,2)

##3.Using the Power2() function that you just wrote, compute 10^3,8^17,131^3

power2(10,3)
power2(8,17)
power2(131,3)

##4.Now create a new function, Power3(), that actually returns the result x^a as an R object, rather than simply printing it to the screen. 

power3 <- function(x,a){ result <- x^a
  return(result)}

power3(10,3)

#5.Now using the Power3() function, create a plot of f(x) = x^2. The x-axis should display a range of integers from 1 to 10, and the y-axis should display x^2. Label the axes appropriately, and use an appropriate title for the figure. Consider displaying either the x-axis, the y-axis, or both on the log-scale. You can do this by using log="x", log="y", or log="xy" as arguments to the plot() function.

power3 <- function(x){ 
              x <- 1:x
  result <- plot(x,x^2)
return(result)}

power3(5)


##6.Create a function, PlotPower(), that allows you to create a plot of x against x^a for a fixed a and for a range of values of x. For instance, then a plot should be created with an x-axis taking on values 1...10; and a y-axis taking on values 1^3,2^3,...10^3

PlotPower <- function(x){
  x <- 1:x
  y <- x^3
  return(plot(x,y, xlab = "x axis", ylab = "y axis"))
}

PlotPower(10)

# 7.Write your own function sum2() that given input n nds the sum 1^2+2^2+...+n^2

sum2 <- function(x){
  y = 0
  for (i in 1:x) {
    i=  i^2
    y = y + i
    i = i + 1
  }
  return(y)
}

sum2(5)


#8.Write a function sum3() that given input n finds the sum 1^3+2^3+    +n^3

sum3 <- function(x){
  y = 0
  for (i in 1:x) {
    i=  i^3
    y = y + i
    i = i + 1
  }
  return(y)
}

sum3(3)
