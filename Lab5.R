#####################################################################
################## 1 Bagging and Random Forest ######################
#####################################################################

library(randomForest)
library(MASS)

set.seed(158)
attach(Boston)
## First apply bagging and random forest using randomForest package on Boston Data set to predict Medv.

train <- sample(1:nrow(Boston), nrow(Boston)/2)
bag.boston <- randomForest(medv~., data = Boston, subset = train, mtry = 13) ## mtry indicate all 13 attribute should be considered during the each tree split

bag.boston

## Let's check h ow bag model perform on test data set

yhat.bag <- predict(bag.boston,newdata = Boston[-train,])
length(yhat.bag)
boston.test <- Boston[-train, "medv"]
length(boston.test)
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag - boston.test)^2) ## [1] 11.71459, test set MSE = 11.71

### Now, let's restrict the number of tree grown by random forest to 25

bag.boston <- randomForest(medv~., data = Boston, subset = train, mtry = 13, ntree = 25)


yhat.bag <- predict(bag.boston, newdata = Boston[-train,])
mean((yhat.bag - boston.test)^2) ## [1] 11.57819, MSE is further improve


#####################################################################
######################## 2 Boosting #################################
#####################################################################

library(gbm)

# We use gbm package within it to use gbm() to fit boosted regression tree

set.seed(2314)
?gbm

boost.boston <- gbm(medv~., data = Boston[train,], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)

## we use argument distribution as "gaussion" as we are fitting boosted regression tree, if it would have been boosted classification tree then we would have used "bernoulli"
## The option interaction.depth limit the depth of each tree to 4.

yhat.boost <- predict(boost.boston, newdata = Boston[-train,], n.trees = 5000)

boston.test <- Boston[-train, "medv"]

mean((yhat.boost-boston.test)^2) # [1] 12.4099


## Let's perform boosting with different value learning rate / shrinkage parameter and check the test MSE

boost.boston <- gbm(medv~., data = Boston[train,], distribution = "gaussian", n.trees = 5000, interaction.depth = 4, shrinkage = 0.001)

yhat.boost <- predict(boost.boston, newdata = Boston[-train,], n.trees = 5000)

mean((yhat.boost-boston.test)^2) # 13.69078

## shrinkage = 0.2 Test MSE slightly increase 13.69078
## shrinkage = 0.001 Test MSE slightly decrese 12.39783


#####################################################################
######################## 3 Exercises ################################
#####################################################################


##1.What is the probability that the first bootstrap observation is not the jth observation from the original sample? Justify your answer (there is no need to write down your answer or justification).

## (1-(1/n))

##2.What is the probability that the second bootstrap observation is not the jth observation from the original sample? (There is no need to write down your answer.)

## (1-(1/n))

##3.Argue that the probability that the jth observation is not in the bootstrap sample is (1-1=n)^n. (There is no need to write down your argument.)

## as it is n independant events

##4.When n = 5, what is the probability that the jth observation is in the bootstrap sample? (There is no need to write down your answer.)

##1-(1/5)^5

##5.When n = 100, what is the probability that the jth observation is in the bootstrap sample? Write down this probability.

##1-(1/100)^100

##6.When n = 10000, what is the probability that the jth observation is in the bootstrap sample? (There is no need to write down your answer.)

## 1-(1/10000)^10000

##7.Create a plot that displays, for each integer value of n from 1 to 100000,the probability that the jth observation is in the bootstrap sample. Comment on what you observe. (There is no need to write down your comment.) Save your plot as a pdf file.

n <- 1:100000
prob <- 1-(1-1/n)^n
plot(n,prob)


##8.We will now investigate numerically the probability that a bootstrap sample of size n = 100 contains the jth observation. Here j = 4. We repeatedly create bootstrap samples, and each time we record whether or not the fourth observation is contained in the bootstrap sample.

store <- rep(NA,10000)
for (i in 1:10000) {
  store[i] <- sum(sample(1:100, rep=TRUE)==4)>0
}
mean(store)