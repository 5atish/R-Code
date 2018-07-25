#####################################################################
######################## 1 Linear Regression#########################
#####################################################################

library(Mass)
library(ISLR)

data(Boston)
str(Boston)
fix(Boston)

set.seed(98998)

### Fit simple linear regression Using lm(y~x, data) functon with medv as label and lstat as attribute using boston data set

attach(Boston)

lm.boston <- lm(medv~lstat, data = Boston)

lm.boston

summary(lm.boston) ## detail Information of linear regression

names(lm.boston) ## this function shows what information stored in lm

lm.boston$coefficients
coef(lm.boston) ## 

View(Boston)

k <- (Boston$lstat = c(5,10,15))

predict(lm.boston, data.frame(lstat= c(5,10,15))) ## predict function used to produce prediction of medv for given value i.e.lstat

plot(lstat, medv)
abline(lm.boston)


### Fit multiple linear regression Using lm(y~x1+x2+x3, data) functon with medv as label and lstat, age as attribute using boston data set.

lm.boston <- lm(medv~lstat+age, data = Boston)

lm.boston

lm.boston <- lm(medv~., data = Boston)

######################Qualitative Variable###########################

####Goal###### To predict sales in 400 stores using number of attributes of Carset dataset

View(Carseats)

set.seed(9090)
 
attach(Carseats)
names(Carseats)

## R create dummy variable to fit multiple regression model using qualitative attributes

lm.car <- lm(Sales~., data = Carseats)
lm.car

contrasts(ShelveLoc) ## contrasts function shows dummy variables
contrasts(Urban)



######################## 2 Logistic Regression#########################
#####################################################################

View(Smarket)

set.seed(393)
attach(Smarket)

## Fit logistic regression using function glm(y~x1+x2.., data, family=binomial) (generalize linear model), direction as qualitative label and Lag1 to Lag5, voulume as attribute from Smarket dataset

glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family = binomial, data = Smarket)

glm.fit

?predict.glm

glm.prob <- predict(glm.fit, type ="response") ## type = "response" aregument is used to tell R to output probablities of form P(Y = 1|X)

contrasts(Direction)
glm.prob[1:10] ## first 10 probablities printed


## The following two commands create a vector of class predictions based on whether the predicted probability of a market increase is greater than or less than 0:5.

glm.pred <- rep("Down", 1250) ## this command create vector of 1250 elements

glm.pred[glm.prob > .5] <- "Up" ## transform to UP all the element with predicted probablity of market increase exceeds 0.5 

table(glm.pred, Direction)

mean(glm.pred==Direction) ## [1] 0.5216 i.e Logistic function correctly predicted moment of market


### now take train.smark as subset of smarket data 

train <- (Year<2005)
dim(train)
length(train)
smark.2005 <- Smarket[!train,]
dim(smark.2005)
length(smark.2005)
fix(smark.2005)
direction.2005 <- smark.2005[,"Direction"]
dim(direction.2005)
length(direction.2005)

glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family = binomial, data = Smarket, subset = train)


glm.prob <- predict(glm.fit,smark.2005, type ="response") 

glm.pred <- rep("Down",252)
glm.pred[glm.prob>.5] <- "Up"

table(glm.pred, direction.2005)

mean(glm.pred==direction.2005) ##  0.4801587

mean(glm.pred!=direction.2005) ##  0.5198413

## test error rate is 52% which is worse

##Let us now refit logistic regression using just Lag1 and Lag2


glm.fit <- glm(Direction~Lag1+Lag2, family = binomial, data = Smarket, subset = train)


glm.prob <- predict(glm.fit,smark.2005, type ="response") 

glm.pred <- rep("Down",252)
glm.pred[glm.prob>.5] <- "Up"

table(glm.pred, direction.2005)

##glm.pred Down  Up
###   Down   35  35
###   Up     76 106

mean(glm.pred==direction.2005)  ## [1] 0.5595238


######################## 3 K Nearest Neighbours ######################
#####################################################################

library(class)

train.x <- cbind(Lag1,Lag2)[train,] # matrix containing the attributes(Lag1, Lag2) associated with the training data, 
dim(train.x)
test.x <- cbind(Lag1,Lag2)[!train,] ## matrix containing the attributesdme(Lag1, Lag2) associated with the data for which to make prediction
dim(test.x)
train.direction <- Direction[train] ## Vector containing the class label for training observation 
dim(train.direction)
set.seed(755)

knn.pred <- knn(train.x, test.x, train.direction, K = 1)

table(knn.pred, direction.2005)

###knn.pred Down Up
###   Down   43 58
##    Up     68 83
(43+83)/252 # [1] 0.5

mean(knn.pred==direction.2005) ## [1] 0.5

### K=1 is not very good as only 50% observation is correctly predicted

knn.pred <- knn(train.x, test.x, train.direction,k=3)

table(knn.pred, direction.2005)

mean(knn.pred==direction.2005) ## [1] 0.531746

## The results have improved slightly. no denite conclusions are possible since the data set is so small.


######################## 4 Exercise #################################
#####################################################################

##1.This problem involves the Auto data set. Use the lm() function to perform a simple linear regression with mpg as the label and horsepower as the attribute.

View(Auto)
attach(Auto)

lm.auto <- lm(mpg~horsepower, data = Auto)

lm.auto
##(a)Is the relationship between the attribute and the label positive or negative for this data set?

#the relationship between the attribute and the label as slope is -0.1578.

##(b)What is the predicted mpg associated with a horsepower of 98?

predict(lm.auto, data.frame(horsepower= c(98))) #24.46708 


##(c)Plot the label and the attribute. Use the abline() function to dis- play the least squares regression line.

plot(horsepower,mpg)
abline(lm.auto)

##2.This problem involves the Boston data set. We will try to predict per capita crime rate using the other variables in this data set. In other words, per capita crime rate is the label, and the other variables are the attributes. (If this looks too tiresome, use a subset of two or three variables that look most promising to you as the attributes.)

View(Boston)

pairs(Boston)

set.seed(3378)

lm.boston <- lm(crim~tax+lstat+medv, data = Boston)
lm.boston

##(a)For each attribute, fit a simple linear regression model to predict the label.

lm.boston <- lm(crim~tax, data = Boston)
lm.boston # with slope 0.02974
plot(tax, crim)
abline(lm.boston)  ## there is strange standard value for tax around 670


lm.boston <- lm(crim~lstat, data = Boston)
lm.boston ## with slope 0.5488
plot(lstat, crim)
abline(lm.boston)

lm.boston <- lm(crim~medv, data = Boston)
lm.boston ## with slope -0.3632
plot(medv, crim)
abline(lm.boston) ## crim is gradually decreses from medv aronud 12 to 40, strangly at 50 it increse  

##(b)Fit a multiple regression model to predict the label using all of the attributes.

lm.boston <- lm(crim~tax+lstat+medv, data = Boston)
lm.boston

predict(lm.boston, data.frame(black+lstat+medv))

##(c)How do your results from item 2a compare to your results from item 2b? 

##As from Above results of 2a and 2b, we can see that intercept changes for both individual attributes and combine attribute. As combine the attribute coefficient decreases for each attribute.

## Create a plot displaying the univariate regression coeficients from item 2a on the x-axis, and the multiple regression coeficients from item 2b on the y-axis. That is, each attribute is displayed as a single point in the plot. Its coeficient in the simple linear regression model is shown on the x-axis, and its coeficient in the multiple linear regression model is shown on the y-axis.

x <- c(0.02974, 0.5488, -0.363)
y <- c(0.02406, 0.19996, -0.04208 )
plot(x,y)

##(d)Is there evidence of non-linear association between any of the attributes and the label? To answer this question, for each attribute X, fit the learning machine

attach(Boston)
pairs(Boston)
taxsquare <- tax^2
taxcube <- tax^3

lm.tax <- lm(crim~tax+taxsquare+taxcube)
lm.tax

curve(1.918e+01-1.533e-01*x+3.608e-04*x^2-2.204e-07*x^3,5,50)

## For Lstat

lstatsquare <- lstat^2
lstatcube <- lstat^3

lm.lstat <- lm(crim~lstat+lstatsquare+lstatcube)
lm.lstat

curve(1.2009656-0.4490656*x+0.0557794*x^2-0.0008574*x^3,1,10)

##for medv

medvsquare <- medv^2
medvcube <- medv^3

lm.medv <- lm(crim~medv+medvsquare+medvcube)
lm.medv

curve(53.16554-5.09483*x+0.15550*x^2-0.00149*x^3,5,100)

##3.In this problem, you will develop a model to predict whether a given car gets high or low petrol mileage based on the Auto data set.
View(Auto)
set.seed(77447)
attach(Auto)


#(a)Create a binary variable, mpg01, that contains a 1 if mpg contains a value above its median, and a 0 if mpg contains a value below its median. You can compute the median using the median() function.

mpg01 <- as.numeric(median(mpg)< mpg)
mpg01



Auto01 <- data.frame(Auto, mpg01)

View(Auto01)

#(b)Explore the data graphically in order to investigate the association between mpg01 and the other variables. Which of the other variables look most useful in predicting mpg01? Scatterplots and boxplots may be useful tools to answer this question.

pairs(Auto01)

## mpg,Cylinder horsepower,weight looks most useful in predicing mpg01

plot(mpg01,mpg, main = "Scatterplot")

boxplot(cylinders~mpg01)
boxplot(displacement~mpg01)
plot(mpg01,displacement)
boxplot(weight~mpg01)

## As per Attached image boxplot(displacement~mpg01),boxplot(cylinders~mpg01),boxplot(weight~mpg01) association of Displacement looks most suitable with mpg01 as discplacement is low mpg01 is high when discplacement increases mpg01 decreases.


#(c)Split the data into a training set and a test set.

##we plit the data into 60% training set and 40% test set

temp <- sample(2, nrow(Auto01), replace = TRUE, prob = c(0.6,0.4))
train.auto <- Auto01[temp==1,]
dim(train.auto)
test.auto <- Auto01[temp==2,]
dim(test.auto)
View(test.auto)


attach(Auto.t)
mpg01

#(d)Perform logistic regression on the training data in order to predict mpg01 using the variables that seemed most associated with mpg01 in item 3b. What is the test error of the model obtained?

View(train.auto)

glm.auto <- glm(mpg01~displacement+cylinders+weight, family = binomial, data = train.auto) ## train model on training data set

glm.prob <- predict(glm.auto, test.auto, type = "response") ## 

glm.pred <- rep(0,148)
glm.pred[glm.prob>.4] <- 1

table(glm.pred, test.auto$mpg01)

mean(glm.pred!=test.auto$mpg01) ## [1] 0.1013514

#### Test error rate is 10.13%  

##(e)Perform KNN on the training data, with several values of K, in order to predict mpg01. Use only the variables that seemed most associated with mpg01 in item 3b. What test errors do you obtain? Which value of K seems to perform the best on this data set?


library(class)

## for knn prediction following inputs required

train.x <- cbind(train.auto$displacement, train.auto$cylinders, train.auto$weight ) ## matrix containing the attributes associated with the                      training data

test.x <- cbind(test.auto$displacement, test.auto$cylinders, test.auto$weight) ## matrix containing the attributes associated with the test data for which to make prediction 

train.mpg01 <- train.auto$mpg01 ## vector containing the class labels for the training observations labelled

test.mpg01 <- test.auto$mpg01 ## vector containing the class labels for the test observations labelled

set.seed(3437)

knn.auto <- knn(train.x, test.x, train.mpg01, k=1)
table(knn.auto, test.mpg01)
mean(knn.auto!=test.mpg01) # [1] 0.1351351

knn.auto <- knn(train.x, test.x, train.mpg01, k=2)
table(knn.auto, test.mpg01)
mean(knn.auto!=test.mpg01) # [1] 0.1216216

knn.auto <- knn(train.x, test.x, train.mpg01, k=3)
table(knn.auto, test.mpg01)
mean(knn.auto!=test.mpg01) # [1] 0.1283784

knn.auto <- knn(train.x, test.x, train.mpg01, k=10)
table(knn.auto, test.mpg01)
mean(knn.auto!=test.mpg01) # [1] 0.1283784

knn.auto <- knn(train.x, test.x, train.mpg01, k=20)
table(knn.auto, test.mpg01)
mean(knn.auto!=test.mpg01) # [1] 0.1216216

knn.auto <- knn(train.x, test.x, train.mpg01, k=30)
table(knn.auto, test.mpg01)
mean(knn.auto!=test.mpg01) # [1]  0.1013514

knn.auto <- knn(train.x, test.x, train.mpg01, k=100)
table(knn.auto, test.mpg01)
mean(knn.auto!=test.mpg01) # [1] 0.108108

knn.auto <- knn(train.x, test.x, train.mpg01, k=200)
table(knn.auto, test.mpg01)
mean(knn.auto!=test.mpg01) # [1] 0.1621622

knn.auto <- knn(train.x, test.x, train.mpg01, k=220)
table(knn.auto, test.mpg01)
mean(knn.auto!=test.mpg01) # [1] 0.1891892


## Very good performance at K=30, 10.13% of error rate 


### 4.Using the Boston data set, fit classification models in order to predict whether a given suburb has a crime rate above or below the median.Explore logistic regression and KNN models using various subsets of the attributes.

View(Boston)
attach(Boston)
median(crim)
dim(Boston)
crim01 <- as.numeric(median(crim)>crim)
crim01

boston01 <- data.frame(Boston, crim01)

View(boston01)

set.seed(232)

##we plit the data into 60% training set and 40% test set
temp <- sample(2, nrow(boston01), replace = TRUE, prob = c(0.6, 0.4))
train.b <- boston01[temp==1,]
dim(train.b) #[1] 304  15
test.b <- boston01[temp==2,]
dim(test.b) # [1] 202  15

pairs(boston01)
plot(boston01$crim01, boston01$medv)
plot(boston01$crim01, boston01$lstat)
plot(boston01$crim01, boston01$rm)
plot(boston01$crim01, boston01$age)
plot(boston01$crim01, boston01$black)
plot(boston01$crim01, boston01$crim)
plot(boston01$crim01, boston01$zn)

## we choose attribute rm, age,zn black for label crim01

## fit logistic regression model on training set

glm.fit <- glm(crim01~rm+age+black+zn, data = train.b, family = binomial)
glm.fit

glm.prob <- predict(glm.fit,test.b, type = "response")

glm.prob[1:10]

glm.pred <- rep(0,202)
glm.pred[glm.prob>0.5] <- 1

table(glm.pred, test.b$crim01)
mean(glm.pred!=test.b$crim01) #[1] 0.1782178

## knn
library(class)
View(train.b)
train.x <- cbind(train.b$zn, train.b$rm, train.b$age, train.b$black)
test.x <- cbind(test.b$zn, test.b$rm, test.b$age, test.b$black)
train.crim01 <- train.b$crim01
teset.crim01 <- test.b$crim01

knn.pred <- knn(train.x, test.x, train.crim01, k=1)
table(knn.pred, teset.crim01)
mean(knn.pred!=teset.crim01) #[1] 0.2326733

knn.pred <- knn(train.x, test.x, train.crim01, k=2)
table(knn.pred, teset.crim01)
mean(knn.pred!=teset.crim01) #[1] 0.2673267

knn.pred <- knn(train.x, test.x, train.crim01, k=3)
table(knn.pred, teset.crim01)
mean(knn.pred!=teset.crim01) #[1] 0.2079208

knn.pred <- knn(train.x, test.x, train.crim01, k=5)
table(knn.pred, teset.crim01)
mean(knn.pred!=teset.crim01) #[1] 0.2178218

knn.pred <- knn(train.x, test.x, train.crim01, k=10)
table(knn.pred, teset.crim01)
mean(knn.pred!=teset.crim01) #[1] 0.1633663

knn.pred <- knn(train.x, test.x, train.crim01, k=20)
table(knn.pred, teset.crim01)
mean(knn.pred!=teset.crim01) #[1] 0.1732673

knn.pred <- knn(train.x, test.x, train.crim01, k=50)
table(knn.pred, teset.crim01)
mean(knn.pred!=teset.crim01) #[1] 0.1831683

knn.pred <- knn(train.x, test.x, train.crim01, k=100)
table(knn.pred, teset.crim01)
mean(knn.pred!=teset.crim01) #[1] 0.1782178

knn.pred <- knn(train.x, test.x, train.crim01, k=200)
table(knn.pred, teset.crim01)
mean(knn.pred!=teset.crim01) #[1] 0.1633663

## very good performance at k=10, error rate is 16.33%