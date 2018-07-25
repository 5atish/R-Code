library(tree)
library(ISLR)


################################################################
################Fitting Classification Trees###################
################################################################

### We use classification trees to analyze the Carseats data set, which  contains information about sales of child car seats at 400 different stores 

####################GOAL##################################

##  Goal is to predict whether the sales at a store are high (defined as more than 8) or low (defined as 8 or less)

##########################################################

data("Carseats")
names(Carseats)
any(is.na(Carseats))  ## check whether there is any na value exist in     dataset

attach(Carseats)

High <- ifelse( Sales <= 8, "No", "Yes") ## created new variable High with condition as sales less than equal to 8 then No else Yes 

Carseats <- data.frame(Carseats, High) ## attach new variable to main Carseats dataset

View(Carseats)  

tree.carseats <- tree(High~.-Sales,Carseats) ## tree function is used to fit classification tree model in order to predict classification tree.
## for example, tree(y~x1+x2+x3,data) is used to fit a classification tree for predicting variable y 
## The . dot refers to the set of all variable in the dataset and we remove predictive variable from the dataset by syntax -Sales

length(tree.carseats)

summary(tree.carseats)  ## the training error rate is 9%.

plot(tree.carseats) ## Most attractive property of tree functoin is plot i.e. plot function to displayed tree structure 

text(tree.carseats, pretty = 0) ## text function to display node and argument pretty = 0 instruct R to include category name for qualitative predictors 

set.seed(332) ## seed function used to produce exact set random variable

Carseats.train <- sample(1:nrow(Carseats),200) ## sample function is used here to select random subset of 200 rows from 1 to last row of Carseat dataset i.e. 400 

Carseats.test <- Carseats[-Carseats.train,] ## test set is entire Carseats dataset minus Carseats.train dataset.

High.test <- High[-Carseats.train] ## variable High.test created with from minus train dataset i.e. it's from test dataset in order to camparison with prediction and target value from test dataset. 

?sample

## Build tree model using train dataset and tree function

tree.carseats <- tree(High~.-Sales, Carseats, subset = Carseats.train)
length(tree.carseats)

## Evaluate its performance on Test dataset and predict function can be used for this purpose

tree.predict <- predict(tree.carseats, Carseats.test, type = "class") ## argument type = "class" instruct R to return actual class prediction

## create confusion matrix 
table(tree.predict,High.test)

##              High.test
##   tree.predict No Yes
##            No  93  42
##            Yes 17  48

(93+48)/200  ## 0.705 i.e  This approach leads to correct prediction around 70.5% of the location in the test data set.

set.seed(3377)

## we consider whether pruning the tree might lead to improved results.
## The function cv.tree() performs cross-validation in order to determine the optimal level of tree complexity.

cv.Carseats <- cv.tree(tree.carseats, FUN = prune.misclass) ## We use the argument FUN=prune.misclass in order to indicate that the classification error rate to guide the cross validation and pruning process, rather than the default for the cv.tree() function.

names(cv.Carseats)

cv.Carseats

##  $size
##  [1] 18 11  9  5  4  2  1

##  $dev
##  [1] 48 48 48 64 55 55 77 ## Error rate

##  $k
##  [1]  -Inf  0.00  0.50  3.25  5.00  5.50 24.00  ## Terminal Node

##  $method
##  [1] "misclass"

##  attr(,"class")
##  [1] "prune"         "tree.sequence"

### There are 7 sizes of tree and lowest cross validation error rate is 48 for size 9

### Plot the error rate as function of both size and k,  

plot(cv.Carseats$size, cv.Carseats$dev, type = 'b')


### Prune the tree to obtain 7 node tree by applying prune.misclass () function.

### par function split the display screen into separate panel to view multiple plot simultenously

par(mfrow = c(1,2)) ## divide the plotting region in 1 into 2 grid of panels

prune.Carseats <- prune.misclass(tree.carseats, best = 7)
plot(prune.Carseats)
text(prune.Carseats, pretty = 0)

### Now check how well pruned tree perform on test data by applying predict() function.

tree.predict <- predict(prune.Carseats, Carseats.test, type = "class")
table(tree.predict, High.test)

##            High.test
##  tree.predict No Yes
##           No  92  38
##           Yes 18  52

(92+52)/200 ## 0.72 i.e. 72% of test observation are correctly classified, so pruning produce more interpretable tree and improve classification accuracy.

### if we increase value of best in prune function, we get a larger pruned tree with lower classification accuracy.

prune.Carseats <- prune.misclass(tree.carseats, best = 15)
tree.predict <- predict(prune.Carseats, Carseats.test, type = "class")
table(tree.predict,High.test)

##              High.test
##  tree.predict  No Yes
##           No  100  51
##           Yes  10  39

(100+39)/200 ## 0.695 i.e.69.5%  ## lower classification accuracy


##################################################################
##################Fitting Regression Trees#######################
##################################################################

## To fit a regression tree we use Boston data set which describe housing information of 506 neighbourhoods around boston suburb

############################Goal################################

## Goal is to predict medv(mediun house value) using 13 attributes such as rm, age, lstat etc.

library(MASS)
data(Boston)
?Boston
fix(Boston)
any(is.na(Boston))
attach(Boston)
names(Boston)

### First we create training and test data set and fit a tree on training dataset

set.seed(277)
train.boston <- sample(1:nrow(Boston),nrow(Boston)/2)
length(train.boston)
tree.boston <- tree(medv~.,Boston, subset = train.boston) ## for example, tree(y~x1+x2+x3,data)
summary(tree.boston) ## output of summary shows that only 5 variable has been used to construct the tree.

##Regression tree:
##  tree(formula = medv ~ ., data = Boston, subset = train.boston)
##Variables actually used in tree construction:
##  [1] "lstat" "rm"    "dis"   "indus" "nox"   "crim" 
##Number of terminal nodes:  10 
##Residual mean deviance:  12.24 = 2974 / 243 
##Distribution of residuals:
##  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
##-10.8000  -1.7020  -0.1022   0.0000   1.7820  20.6200 

plot(tree.boston)
text(tree.boston, pretty = 0)

### Tree indicate that lstate less than 5.95 (lstat < 5.95) and rm more than 7.37 (rm > 7.37) currespond to expensive houses.

### and lstat more than 15, crime more than 11.48 curresond to least expensive houses.

#### Now check Pruning the tree improves performance.

cv.boston <- cv.tree(tree.boston)
cv.boston

##$size
##[1] 10  8  7  6  5  4  3  2  1

##$dev
##[1]  6071.861  6076.855  6041.438  5879.649  5750.732  6172.479  8265.669
##[8] 11482.696 21345.680

##$k
##[1]       -Inf   231.6140   242.1136   251.5330   576.1806   741.6228
##[7]  1636.1411  3356.6156 10770.7995

##$method
##[1] "deviance"

##attr(,"class")
##[1] "prune"         "tree.sequence"

plot(cv.boston$size, cv.boston$dev, type = "b")

prune.boston <- prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)

### Lets use unpruned tree to make prediction on test data set
length(Boston[-train.boston,])
yhat <- predict(tree.boston, newdata = Boston[-train.boston,])
length(yhat)
test.boston <- Boston[-train.boston,"medv"]
length(test.boston)
plot(yhat, test.boston)
abline(0,1) ## abline function draw a line with intercept a and slope b
mean((yhat-test.boston)^2) 
## [1] 25.43626   ### test set MSE associate with regression tree is 25.436

?Boston

sqrt(mean((yhat-test.boston)^2))
##[1] 5.043437 ## which indicate that this model leads to test prediction that are within around $5,043.43 of the true median home value for the suburb. 

#############################################################
#####################Exercise#############################
#############################################################

##constructed a classification tree for the Carseats data set after converting Sales into a qualitative response variable. Now we will seek to predict Sales using regression trees and treating the response as a quantitative variable.


##1.Split the data set into a training set of size 200 and a test set (of the same size).

View(Carseats)

train.carseats <- sample(1:nrow(Carseats), nrow(Carseats)/2)
length(train.carseats)
test.carseats <- Carseats[(-train.carseats),"Sales"]
length(test.carseats)

##2.Fit a regression tree to the training set. Plot the tree, and interpret the results. What test MSE do you obtain?

regtree.carseats <- tree(Sales~.,Carseats, subset = train.carseats)
summary(regtree.carseats) ## output shows 6 variables has been used for tree formation

plot(regtree.carseats)
text(regtree.carseats, pretty = 0)


car_yhat <- predict(regtree.carseats, Carseats[-train.carseats,])
length(car_yhat)
length(test.carseats)
plot(car_yhat, test.carseats)
abline(0,1)
mean((car_yhat-test.carseats)^2) ## [1] 5.173834

sqrt(mean((car_yhat-test.carseats)^2)) ##[1] 2.274606

##3.Use cross-validation in order to determine the optimal level of tree complexity. Does pruning the tree improve the test MSE?
  

cross.carseats <- cv.tree(regtree.carseats)
cross.carseats
plot(cross.carseats$size, cross.carseats$dev, type = 'b')
prune.Carseats <- prune.tree(regtree.carseats, best = 7)
plot(prune.Carseats)
text(prune.Carseats, pretty = 0)
p_yhat <- predict(prune.Carseats,Carseats[-train.carseats,])
plot(prune.Carseats, test.carseats)
text(prune.Carseats, pretty = 0)
mean((p_yhat-test.carseats)^2) ## 4.462576
sqrt(mean((p_yhat-test.carseats)^2))  ##  2.112481


###9. This problem involves the OJ data set which is part of the ISLR package.

library(ISLR)
View(OJ)
attach(OJ)
names(OJ)

set.seed(1639)
###(a) Create a training set containing a random sample of 800 observations,and a test set containing the remaining observations.

train.oj <- sample(1:nrow(OJ), nrow(OJ)/2)
length(train.oj)
test.oj <- OJ[-train.oj,]
length(test.oj)
Purchase.test <- Purchase[-train.oj]
length(Purchase.test)

###(b) Fit a tree to the training data, with Purchase as the response and the other variables as predictors. 

tree.oj <- tree(Purchase~., OJ, subset = train.oj)

###Use the summary() function to produce summary statistics about the tree, and describe the results obtained. 

summary(tree.oj) ## 5 variable "LoyalCH","SalePriceMM","SpecialCH","PriceDiff","StoreID" used to construct the tree with 11 terminal node.

## mean deviance is 0.6664

#Classification tree:
#  tree(formula = Purchase ~ ., data = OJ, subset = train.oj)
#Variables actually used in tree construction:
#  [1] "LoyalCH"     "SalePriceMM" "SpecialCH"   "PriceDiff"   "StoreID"    
#Number of terminal nodes:  11 
#Residual mean deviance:  0.6664 = 349.2 / 524 
#Misclassification error rate: 0.1607 = 86 / 535

## 

###What is the training error rate? How many terminal nodes does the tree have?

# The training error rate is 16.07 and the tree has 11 terminal node


###(c) Type in the name of the tree object in order to get a detailed text output. Pick one of the terminal nodes, and interpret the information displayed.

names(tree.oj)
tree.oj
?OJ


#node), split, n, deviance, yval, (yprob)
#* denotes terminal node

#1) root 535 706.200 CH ( 0.62804 0.37196 )  
#2) LoyalCH < 0.482935 198 221.600 MM ( 0.24747 0.75253 )  
#4) LoyalCH < 0.0616725 45   9.591 MM ( 0.02222 0.97778 ) *
#  5) LoyalCH > 0.0616725 153 190.300 MM ( 0.31373 0.68627 )  
#10) SalePriceMM < 2.04 85  85.070 MM ( 0.20000 0.80000 )  
#20) SpecialCH < 0.5 71  57.720 MM ( 0.14085 0.85915 ) *
#  21) SpecialCH > 0.5 14  19.410 MM ( 0.50000 0.50000 ) *
#  11) SalePriceMM > 2.04 68  93.740 MM ( 0.45588 0.54412 ) *
#  3) LoyalCH > 0.482935 337 283.000 CH ( 0.85163 0.14837 )  
#6) PriceDiff < 0.265 175 199.500 CH ( 0.74286 0.25714 )  
#12) LoyalCH < 0.705699 74 102.400 CH ( 0.52703 0.47297 )  
#24) LoyalCH < 0.6864 69  94.480 CH ( 0.56522 0.43478 )  
#48) StoreID < 3.5 43  59.030 MM ( 0.44186 0.55814 ) *
#  49) StoreID > 3.5 26  28.090 CH ( 0.76923 0.23077 ) *
#  25) LoyalCH > 0.6864 5   0.000 MM ( 0.00000 1.00000 ) *
#  13) LoyalCH > 0.705699 101  65.230 CH ( 0.90099 0.09901 )  
#26) PriceDiff < -0.35 9  11.460 MM ( 0.33333 0.66667 ) *
#  27) PriceDiff > -0.35 92  32.910 CH ( 0.95652 0.04348 ) *
#  7) PriceDiff > 0.265 162  44.630 CH ( 0.96914 0.03086 )  
#14) LoyalCH < 0.835565 79  37.280 CH ( 0.93671 0.06329 ) *
#  15) LoyalCH > 0.835565 83   0.000 CH ( 1.00000 0.00000 ) *

## Tree chooses root node as customer brand loyalty for CH
## we can see that maximum obsefvation of purchase of Citrus hill   i.e. 92 with customer brand loyalty more than 0.70, pricediff is less than 0.265 and more than -0.35


###(d) Create a plot of the tree, and interpret the results.

plot(tree.oj)
text(tree.oj, pretty = 0)

## we can see that maximum obsefvation of purchase of Citrus hill   i.e. 92 with customer brand loyalty more than 0.70, pricediff is less than 0.265 and more than -0.35

###(e) Predict the response on the test data, and produce a confusion matrix comparing the test labels to the predicted test labels. What is the test error rate?

pred.oj <- predict(tree.oj, test.oj, type = "class")
length(pred.oj)
length(test.oj)
length(tree.oj)
table(pred.oj, Purchase.test)

##Purchase.test
##pred.oj  CH  MM
##CH 240  34
##MM  77 184

(240/184)/535 ## [1] 0.002438033
## Test error rate is 0.24%

###(f) Apply the cv.tree() function to the training set in order to determine the optimal tree size.

set.seed(7)
cv.oj <- cv.tree(tree.oj, FUN = prune.misclass)
cv.oj

#$size
#[1] 11  7  2  1

#$dev
#[1] 108 108 106 199

#$k
#[1]  -Inf   0.0   2.6 100.0

#$method
#[1] "misclass"

#attr(,"class")
#[1] "prune"         "tree.sequence"

###(g) Produce a plot with tree size on the x-axis and cross-validated classi???cation error rate on the y-axis. 

par(mfrow = c(1,2))
plot(cv.oj$size,cv.oj$dev , type = "b")


###(h) Which tree size corresponds to the lowest cross-validated classi???cationerror rate?

## tree size 2 has lowest CV classification error rate

###(i) Produce a pruned tree corresponding to the optimal tree size obtained using cross-validation. If cross-validation does not lead to selection of a pruned tree, then create a pruned tree with ???ve terminal nodes.

prune.OJ <- prune.misclass(tree.oj, best = 2)
plot(prune.OJ)
text(prune.OJ, pretty = 0)
prune.OJ
length(prune.OJ)

###(j) Compare the training error rates between the pruned and unpruned trees. Which is higher?

prune.pred <- predict(prune.OJ, test.oj, type = "class")
table(prune.pred, Purchase.test)

#           Purchase.test
#   prune.pred  CH  MM
#           CH 270  59
#           MM  51 155
(270/155)/535 #[1] 0.003255954

## training error rate is 0.32%

### Pruning has improve classification accuracy from 0.24% to 0.32%