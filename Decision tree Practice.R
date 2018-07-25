library(ISLR)
library(MASS)
library(tree)

require(datasets)
data()
names(data())

d <- data(package = "rpart.plot")
d$results[,"Item"]

?ptitanic
?Caravan
?College
?Khan
?Smarket
?OJ

View(Hitters)
##############################Regression tree on Hitters###################
set.seed(757)

attach(Hitters)
names(Hitters)
any(is.na(Hitters))
Hitter <- na.omit(Hitters)
rm(Hitter)
any(is.na(Hitter))

str(Hitters)


train.Hitter <- sample(1:nrow(Hitters), nrow(Hitters)/2)
length(train.Hitter)
test.Hitter <- Hitters[-train.Hitter, "Salary"]
length(test.Hitter)

tree.hitter <- tree(Salary ~., Hitters, subset = train.Hitter)
summary(tree.hitter)

plot(tree.hitter)
text(tree.hitter, pretty = 0)

##hit_yhat <- predict(tree.hitter, Hitters[-train.Hitter,])
##length(hit_yhat)
##length(test.Hitter)
##plot(hit_yhat, test.Hitter)
##abline(0,1)

cv.hitter <- cv.tree(tree.hitter)
cv.hitter

plot(cv.hitter$size, cv.hitter$dev, type = 'b')

prune.hitter <- prune.tree(tree.hitter, best = 2)
prune.hitter

plot(prune.hitter)
text(prune.hitter, pretty = 0)

### use unorune to make the prediction

yhat_hitter <- predict(prune.hitter, Hitter[-train.Hitter])
                       
length(yhat_hitter)
salary.hit <- Salary[-train.Hitter]
length(salary.hit)
(mean(yhat_hitter-salary.hit)^2)





#####################################################################

set.seed(889)
View(Hitter)
names(Hitter)
salary.hit <- Hitter[,19]
length(salary.hit)

tree.hitter <- tree(Salary~.,Hitter)
tree.hitter
plot(tree.hitter)
text(tree.hitter, pretty = 0)

summary(tree.hitter)

hit_yhat <- predict(tree.hitter, Hitter)
length(hit_yhat)

(mean(hit_yhat-salary.hit)^2)
sqrt((mean(hit_yhat-salary.hit)^2))

#########Regression tree on Hitters Dataset using rpart package######

library(rpart)
library(rpart.plot)
library(ggplot2)

View(Hitter)
attach(Hitter)
str(Hitter)
names(Hitter)
any(is.na(Hitter))

fit.hit <- rpart(Salary~., data = Hitter, method = "anova")
length(fit.hit)
?rpart()

rpart.plot(fit.hit, type = 1, extra = 1, digits = 3, fallen.leaves = TRUE)

pred.hit <- predict(fit.hit, Hitter)
pred.hit
length(pred.hit)

hit.mse <- function(actual, pred) {log10(mean(actual - pred))}
hit.mse(Salary, pred.hit) ## [1] 2.41978e-14

(100)
log10(100)



########################################################################
################Regression tree on Dataset Sleep#########
########################################################################

############################GOAL######################################

######## we want build regression tree that will predict total sleeping hour of mammals based on other variable ############################


library(rpart)
library(rpart.plot)
library(ggplot2)
data("msleep")
View(msleep)
?msleep
attach(msleep)
## we removed unwanted variable and create new data set

msl <- msleep[c(3,6,10,11)]
str(msl)

?rpart()

tree.msl <- rpart(sleep_total~ ., data = msl, method = "anova")
tree.msl

?rpart.plot

rpart.plot(tree.msl, type = 1, extra = 1, digits = 3, fallen.leaves = TRUE)

msl.pred <- predict(tree.msl, msl)
msl.pred

## To check acuraccy of algorithm we create function for both Mean absolute error and Mean square error

msl.mae <- function(actual, predicted) { mean(abs(actual - predicted))} # function for Mean absolute error
msl.mae(sleep_total, msl.pred) #[1] 2.452865
## mean absolute error is 2.45 between target value or response i.e. sleep_total and predicted value of the dataset.

msl.mse <- function(actaul, predicted){sqrt(mean((actaul - predicted)^2))}
## functoin for mean square root error
msl.mse(sleep_total,msl.pred) ## [1] 3.178207
## mean square error is 3.17 between target value or response i.e. sleep_total and predicted value of the dataset.

#####################################################################
###################classification tree using Titanic dataset########
####################################################################

#######Goal is to predict which poeple more like to survive after collision with Iceberg

getwd()
setwd("D:/Royal Holloway University/Data Analysis/2017/Lab Work/Lab_practice")

Titanic_d <- read.csv("D:/Royal Holloway University/Data Analysis/2017/Lab Work/Lab_practice/titanic_csv.csv")
View(Titanic_d)

## Remove unwanted variable, column from dataset
titc <- Titanic_d[-c(1,4,9,11,13)]
str(titc)
any(is.na(titc))
titc <- na.omit(titc)  ## Remove rows with 'NA'
any(is.na(titc))
## Create variable as if survived value 0 then No and 1 then Yes
survived <- ifelse(titc$survived == '0', "No","Yes")
titc <- titc[,-2] #Remove survived varaible from table
View(titc) 
titc <- data.frame(titc, survived) ## append newly created variable survived to data set

## create new varable pclass as if pclass value 1 then "Upper', 2 then 'Middle' and 3 then 'Lower'

pclass <- ifelse(titc$pclass == '1', "Upper",
                  ifelse( titc$pclass == '2', "Middle","Lower"))

titc <- titc[,-1] ## #Remove pclass varaible from table

titc <- data.frame(titc, pclass) ## append newly created variable pclass to data set

View(titc)
set.seed(5098)
View(titc)
attach(titc)

Suffle_titc <- sample(1:nrow(titc), nrow(titc))
View(Suffle_titc)
## Create training data set
train.titc <- sample(1:nrow(titc), (nrow(titc)/2))
length(train.titc)

## create testing data set
test.titc <- titc[-train.titc,"survived"] 
length(test.titc)

## this varaible is created from target variable from test data set in order to do comparison with prediction and actual variable 
test.survived <- survived[test.titc]
test.survived
length(test.survived)

## lets create tree/train the model using trainig data set
tree.titc <- tree(survived~., data = titc, subset = train.titc)
summary(tree.titc)

## Plot tree/ model
plot(tree.titc)
text(tree.titc, pretty = 0)

## now lets do the prediction using train model and test data
pred.titc <- predict(tree.titc, test.titc, type = "class")


pred.titc
length(pred.titc)
length(test.titc)

### create confusion matrix
table(pred.titc, test.titc)

#         test.titc
#pred.titc  No Yes
#      No  190 116
#      Yes 128  89
(190+89)/523 ##[1] 0.5334608 
### Correct precision leads to 53.34%

############Now lets prune the tree to check wether it's performance increase
set.seed(923) ## as cv.tree is random number generator,seed need to set again

## cv.tree function is used to do cross validation in order to check where to stop

cv.titc <- cv.tree(tree.titc, FUN = prune.misclass)
cv.titc

## plot the graph between x as different size of the tree and y as corresponding cros validation error of the tree
plot(cv.titc$size, cv.titc$dev, type = 'b')
 
## from the plot we can see that minimum error rate somewhere between size 5 or 6, we will prune the accordingly 

prune.titc <- prune.misclass(tree.titc, best = 6)

## now lets plot prune model

plot(prune.titc)
text(prune.titc, pretty = 0)

## Now lets do the prediction with prune model
length(test.titc)
prune_pred.titc <- predict(prune.titc, test.titc, type = "class")





########################################################################
################Regression tree on diamonds dataset#########
########################################################################

########################GOAL#########################################

### Goal is to predict price of diamond using 9 varable from dimonds dataset

data("iris")
View(iris)
?diamonds

data()

View(diamonds)
str(diamonds)

set.seed(474)
attach(diamonds)

any(is.na(diamonds))

train_dia <- sample(1:nrow(diamonds), nrow(diamonds)/2)
length(train_dia)
test_dia <- diamonds[-train_dia,]
length(test_dia)

tt.dia <- diamonds[-train.dia, "price"]
tt.dia
length(tt.dia)

tree.di <- tree(price ~ ., data = diamonds, subset = train_dia)

summary(tree.di)

plot(tree.di)
text(tree.di, pretty = 0)


set.seed(475)

cv_tree_dia <- cv.tree(tree.di)

plot(cv_tree_dia$size, cv_tree_dia$dev, type = 'b', xlab = "Tree siz" , ylab = "CV_MSE")


prune_tree_dia <- prune.tree(tree.di, best = 6)
summary(prune_tree_dia)

plot(prune_tree_dia)
text(prune_tree_dia, pretty = 0)
title("Pruned Regression Tree of Diamonds")


pred_prune_tree_dia = predict(prune_tree_dia, newdata = diamonds[train_dia,])

dia_rmse <- function(actaul, predicted){sqrt(mean((actaul - predicted)^2))}

dia_rmse(diamonds[train_dia,"price"], pred_prune_tree_dia)

pred_prune_tree_dia_tst <- predict(prune_tree_dia, newdata = diamonds[-train_dia,])

dia_rmse(diamonds[-train_dia, "price"], pred_prune_tree_dia_tst)

plot(pred_prune_tree_dia_tst, diamonds[-train_dia,]$price, xlab = "Predicted", ylab = "Actual")

abline(0,1)

##################Regression tree using package rpart####################

library(rpart)
library(rpart.plot)
library(ggplot2)

set.seed(6678)

attach(diamonds)
?rpart()

train_dia <- sample(1:nrow(diamonds), nrow(diamonds)/2)
length(train_dia)

model_tree_dia <- rpart(price~., data = diamonds,subset = train_dia, method = "anova")

rpart.plot(model_tree_dia, type = 1, extra = 1, digits = 3, fallen.leaves = TRUE)

pred_tree_dia <- predict(model_tree_dia, diamonds[train_dia,])

dia_rmse <- function(actaul, predicted){sqrt(mean((actaul - predicted)^2))}

dia_rmse(diamonds[train_dia, "price"], pred_tree_dia)

pred_tree_dia_tst <- predict(model_tree_dia, diamonds[-train_dia,])  

dia_rmse(diamonds[-train_dia, "price"], pred_tree_dia_tst)

