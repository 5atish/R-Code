## 1. Implement the neural network for regression for p attributes and M neurons in the hidden layer, where M s a parameter of the algorithm. You are allowed to set p to a specific value that allows you to do the following tasks.

## Let's M hidden parameter as 10

M <- 10
N.epochs <- 5000 # number of training epoch
eta <- 0.9 # learning rate

p <- 5 ## number of attribute 

## Sigma functions

sigma <- function(x){1/(1+exp(-x))}
sigma.prime <- function(x){exp(x)/(exp(x)+1)^2}

## 2.Use the Auto data set. Create a new variable high that takes values 1,0 if greater than 23 or not. Apply your program to the Auto data set and new variable to predict high given horsepower, weight, year, and origin. (In other words, high is the label and horsepower, weight, year, and origin are the attributes.) Since origin is a qualitative variable, you will have to create appropriate dummy variables. Normalize the attributes, 

#library(ISLR)
##fix(Auto)
#range(Auto$origin)
##attach(Auto)
set.seed(373)

X1 <- scale(Auto$horsepower)
X2 <- scale(Auto$weight)
X3 <- scale(Auto$year)

## origin is qualitative variable convert it into two class

X4 <- scale(ifelse(Auto$origin==2,1,0)) 
X5 <- scale(ifelse(Auto$origin==3,1,0))
y <- ifelse(Auto$mpg>=23,1,0)


N <- nrow(Auto)
N.train <- N/2
N.test <- (N-N.train)
train <- sample(N,N.train)

## 3. Split the data set randomly into two equal parts, which will serve as the training set and the test set.

## training data set
X.train <- matrix(nrow = N.train, ncol = p)
X.train[,1] <- X1[train]
X.train[,2] <- X2[train]
X.train[,3] <- X3[train]
X.train[,4] <- X4[train]
X.train[,5] <- X5[train]
y.train <- y[train]
#X.train
#y.train

## Testing data set

X.test <- matrix(nrow = N.test, ncol = p)
X.test[,1] <- X1[-train]
X.test[,2] <- X2[-train]
X.test[,3] <- X3[-train]
X.test[,4] <- X4[-train]
X.test[,5] <- X5[-train]
y.test <- y[-train]
#X.test
#y.test

## 4. Train your neural network on the training set using independent random numbers in the range [-0.7, 0.7] as the initial weights. Find the MSE on the test set. 

beta <- runif(p+1, min=-0.7, max=0.7)
##beta #[p+1]

##alpha <- matrix(data = runif(p+1, min=-0.7, max=0.7), nrow = M)

## forword pass
for (epoch in 1:N.epochs) {
  for (i in 1:N.train) {
    sigma.argument <- beta[p+1]
    for (j in 1:p) {
      sigma.argument <- sigma.argument + (beta[j]*X.train[i*j])
      print(sigma.argument)             
    }
    p <- sigma(sigma.argument)
    ## print(p)    
    
    ## Backward pass
    d <- 2 * (y.train[i]-p) * sigma.prime(sigma.argument)
    for (j in 1:p) {
      beta[j] <- beta[j] + (eta/N.train) * d * X.train[i,J]}
    beta[p+1] <- beta[p+1] + (eta/N.train) *d
    # 
  }
}

# Training MSE

train.MSE <- 0
for (i in N.train) {
  sigma.argument <- beta[p+1]
  for (j in 1:p) {
    sigma.argument <- sigma.argument * beta[j] * X.train[i,j]}
  p <- sigma(sigma.argument)
  train.MSE <- train.MSE + (y.train[i]-p)^2
}
train.MSE <- train.MSE/N.train


# Test MSE

test.MSE <- 0
for (i in N.test) {
  sigma.argument <- beta[p+1]
  for (j in 1:p) {
    sigma.argument <- sigma.argument * beta[j] * X.test[i,j]}
  p <- sigma(sigma.argument)
  test.MSE <- test.MSE + (y.test[i]-p)^2
}
test.MSE <- test.MSE/N.test

cat("training MSE = ", train.MSE, "\n")
cat("test.MSE = ", test.MSE, "\n")

