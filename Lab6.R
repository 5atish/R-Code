#####################################################################
################## 1 Kernel density estimation ######################
#####################################################################

## We will see three kernel density estimation function such as rectangular, triangular, and guassian

rec <- function(x){(abs(x) < 1) * 0.5}
tri <- function(x){(abs(x) < 1) * (1 - abs(x))}
gauss <- function(x){(1/sqrt(2*pi))*(exp(-(x^2)/2))}
x <- seq(from = -3, to = 3, by = 0.001)
x
plot(x, rec(x), type = "l", ylim = c(0,1), lty = 1, ylab = expression(k(x)))
?plot

lines(x, tri(x), lty = 2)
lines(x, gauss(x), lty = 3)
legend(-3, 0.8, legend = c("Rectangular", "Triangular", "Gaussian"),
       lty = 1:3, title = "kernel functions", bty = "n")

### for data point x = (0, 1, 1.1, 1.5, 1.9, 2.8, 2.9, 3.5), let's compute the contribution of each measurement in x, with h=0.4 by the Guassian Kernel  
x <- c(0, 1, 1.1, 1.5, 1.9, 2.8, 2.9, 3.5)
n <- length(x)
xgrid <- seq(from = min(x)-1, to = max(x)+1, by = 0.01)
h <- 0.4
bumps <- sapply(x, function(a){gauss((xgrid-a)/h)/(n*h)}) ## sapply() applies a function to elements in a list and returns the results in a vector, matrix, or list.

plot(xgrid, rowSums(bumps), ylab=expression(hat(f)(x)),type = "l", xlab = "x", lwd = 2) ## rowSums() sums up the rows of a matrix 

rug(x, lwd = 2) ## rug() creates a set of tick marks along the base of a plot

out <- apply(bumps, 2, function(b) lines(xgrid, b))  ## apply() applies a function to rows or columns of a matrix (more generally,to dimensions of an array).

#####################################################################
################## 2 K means clustering #############################
#####################################################################

##The kmeans() function performs K-means clustering in R. We begin with a simple simulated example in which there truly are two clusters in the data: the first 25 observations have a mean shift relative to the next 25 observations achieved by adding some constant.

set.seed(22)
 
x <- matrix(rnorm(50*2), ncol = 2)
x
dim(x)
x[1:25,1] <- x[1:25,1]+3
x[1:25,2] <- x[1:25,2]-4
km.out <- kmeans(x, 2, nstart = 20) #The kmeans() function performs K-means clustering with k=2
## use the nstart argument, to run the kmeans() function with multiple initial cluster assignments
## we should always run K-means clustering with a large value of nstart, such as 20 or 50, otherwise an undesirable local optimum may be obtained.

km.out$cluster ## cluster assignments of the 50 observations are contained in km.out cluster
## The K-means clustering perfectly separated the observations into two clusters


km.out$centers ## K-means clustering computed two centroid for two colums
km.out$size
km.out$iter
km.out$tot.withinss #[1] 95.7047 ### The value of km.out$tot.withinss is the total within-cluster sum of squares

plot(x, col=(km.out$cluster+1), main = "K-means clustering result with k=2", xlab = "", ylab = "", pch = 2, cex = 2)

set.seed(4)

km.out <- kmeans(x, 3, nstart = 20) ##  K-means clustering with k=2
km.out$cluster

plot(x, col=(km.out$cluster+1), main = "K-means clustering result with k=3", xlab = "", ylab = "", pch = 2, cex = 2)

km.out$tot.withinss # [1] 73.59403

set.seed(5)

km.out <- kmeans(x, 3, nstart = 1) 
km.out$tot.withinss

km.out <- kmeans(x, 3, nstart = 20) 
km.out$tot.withinss

#####################################################################
################## 3 Hierarchical clustering ########################
#####################################################################

# The hclust() function implements hierarchical clustering 
# use Euclidean distance as the dissimilarity measure, which is achieved by using the dist() function.
x
hc.complete <- hclust(dist(x), method = "complete") # cluster observations using complete linkage
hc.single <- hclust(dist(x), method = "single") # cluster observations using single linkage
hc.average <- hclust(dist(x), method = "average") # cluster observations using average linkage

par(hfrow = c(1,3))

plot(hc.complete, main = "Complete linkage", xlab = "", sub = "", cex = 0.9)
plot(hc.average, main = "Complete average", xlab = "", sub = "", cex = 0.9)
plot(hc.single, main = "Complete single", xlab = "", sub = "", cex = 0.9)

#  the cutree() function use to determine the cluster labels for each observation associated with a given cut of the dendrogram 

cutree(hc.complete, 2)
cutree(hc.average,2)
cutree(hc.single, 34)

# The scale() function is use to scale the variables before performing hierarchical clustering of the observations

xsc <- scale(x, center = FALSE, scale = TRUE) # The default arguments for the scale() function are center=TRUE and scale=TRUE.

plot(hclust(dist(xsc), method = "complete"), main = " Heirarchical clustering with scaled observations")

#####################################################################
###################### 4 Exercises ##################################
#####################################################################

## 1.We used h = 0:4 in kernel density estimation in Section 1. Now try difierent values for h and describe your results.

x <- c(0, 1, 1.1, 1.5, 1.9, 2.8, 2.9, 3.5)
n <- length(x)
max(x)
xgrid <- seq(from = min(x)-1, to = max(x)+1, by = 0.01)
h <- 1
bumps <- sapply(x, function(a) gauss((xgrid-a)/h)/(n*h))

plot(xgrid, rowSums(bumps), type = "l", lwd = 2, xlab = "x", ylab = "expression(h(f)(x))")
rug(x, lwd = 2)
out <- apply(bumps, 2, function(b) lines(xgrid, b))


## 2.In this problem, you will generate simulated data, and then perform Kmeans clustering on the data.
## (a)Generate a simulated data set with 20 observations in each of three classes (i.e., 60 observations total), and 50 variables. Hint: There are a number of functions in R that you can use to generate data. One example is the rnorm() function; runif() is another option. Be sure to add a mean shift to the observations in each class so that there are three distinct classes.

set.seed(23)
?rnorm()
?runif()
rnorm(2*2)
runif(2)
x <- matrix(rnorm(20*3*50),ncol=50)
dim(x)
fix(x)
x[1:20,10] <- x[1:20,10]+9
x[21:40,20] <- x[21:40,20]+2
x[41:60,30] <- x[41:60,30]-6


##(b)Perform K-means clustering of the observations with K = 3. How well do the clusters that you obtained in K-means clustering compare with the true class labels?

kn.out <- kmeans(x,3, nstart = 20)
kn.out$cluster
plot(x, col=(kn.out$cluster+1), xlab = "", ylab = "", pch = 20, cex = 2)
table(kn.out$cluster, c(rep(1,20),rep(2,20),rep(3,20)))

#### clusters are form with 3 different classes

## (c)Perform K-means clustering with K = 2. Describe your results.

set.seed(22)
kn.out <- kmeans(x,2, nstart = 20)
kn.out$cluster
kn.out
plot(x, col=(kn.out$cluster+1), xlab = "", ylab = "", pch = 20, cex = 2)
table(kn.out$cluster, c(rep(1,20),rep(2,20),rep(3,20)))

###### we can see two classes ditributed randomly for the plot  

## (d)Now perform K-means clustering with K = 4, and describe your results.

set.seed(24)
kn.out <- kmeans(x,4, nstart = 20)
kn.out$cluster
plot(x, col=(kn.out$cluster+1), xlab = "", ylab = "", pch = 20, cex = 2)
table(kn.out$cluster, c(rep(1,20),rep(2,20),rep(3,20)))

## 3.Consider the USArrests data (part of the base R package). We will now perform hierarchical clustering on the states.

library(ISLR)
data("USArrests")
View(USArrests)

###(a)Using hierarchical clustering with complete linkage and Euclidean distance, cluster the states.

x <- USArrests

hc.complete <-hclust(dist(x), method = "complete") 
plot(hc.complete, xlab = "", ylab = "", cex = .9, main = "USArrest Complete Linkage") 

##(b)Cut the dendrogram at a height that results in three distinct cluster. Which states belong to which clusters?

cutree(hc.complete, 3)

###(c)Hierarchically cluster the states using complete linkage and Euclidean distance, after scaling the variables to have standard deviation one.

xsc <- scale(x)
hc.complete <-hclust(dist(xsc), method = "complete") 
plot(hc.complete, xlab = "", ylab = "", cex = .9, main = "USArrest Complete Linkage with Scaling") 

##(d)What effect does scaling the variables have on the hierarchical clustering obtained? In your opinion, should the variables be scaled before the inter-observation dissimilarities are computed? Provide a justification for your answer.

## Yes scaling is importan for analysis of statistic, for example there are few murders does not mean that the murder statistics is less important than other three attributes. 


##4.You are given a dataset
#X := {1; 1; 1; 2; 2; 2; 2; 3; 4; 4; 4; 5}
# Use Parzen windows to estimate the density p(x) at x = 2:5 and x = 6; set h = 3 (where h is the bandwidth parameter)

x <- c(1, 1, 1, 2, 2, 2, 2, 3, 4, 4, 4, 5)
n <- length(x)
xgrid <- seq(from = min(x)-1, to = max(x)+1, by = 0.01)
h <- 3
bumps <- sapply(x, function(a) gauss((xgrid-a)/h)/(n*h))

plot(xgrid, rowSums(bumps), type = "l", lwd = 2, xlab = "x", ylab = "expression (h(f)(x))")
rug(x, lwd = 2)
out <- apply(bumps, 2, function(b) lines(xgrid, b))


##5.Given the two-dimensional dataset
##X := f(1; 7); (4; 2); (3; 8:5); (6:6; 9); (1; 9:5); (4; 7)g ;
##apply the K-means clustering algorithm to obtain 3 clusters.

a <- c(1,4,3,6.6,1,4)
b <- c(7,2,8.5,9,9.5,7)
?matrix()
x <- matrix(c(a,b), ncol = 2)
x
dim(x)
set.seed(2341)
 
km.out <- kmeans(x,3, nstart = 10)
km.out
km.out$cluster
plot(x, col=(km.out$cluster+1), xlab = "", ylab = "", pch = 20, cex = 2) 
table(km.out$cluster, rep(1,6), rep(2,6))

## 6. apply hierarchical agglomerative clustering to the data set X in Question 14 to obtain 3 clusters first by using single linkage and then by using complete linkage

hc.single <- hclust(dist(x), method = "single")
plot(hc.single, xlab = "", ylab = "", cex = 0.9, main = "single linkage")

hc.complete <- hclust(dist(x), method = "complete")
plot(hc.complete, xlab = "", ylab = "", cex = 0.9, main = "single complete")


##7.Perform the agglomerative hierarchical clustering algorithm on the following 6 data points using single linkage and Euclidean distance measure:  (1; 1), (1:5; 1:5), (5; 5), (3; 4), (4; 4), and (3; 3:5).

a <- c(1,1.5,5,3,4,3)
b <- c(1,1.5,5,4,4,3.5)
x <- matrix(c(a,b), ncol = 2)

hc.single <- hclust(dist(x), method = "single")
plot(hc.single, xlab = "", ylab = "", cex = .9, main = "single linkage")


hc.complete <- hclust(dist(x), method = "complete")
plot(hc.complete, xlab = "", ylab = "", cex = .9, main = "single complete")
