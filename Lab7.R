#####################################################################
################## 1 Principal Component Analysis ###################
#####################################################################

states <- row.names(USArrests)
states

names(USArrests)

apply(USArrests,2,mean) ## apply() function allows us to apply a function(in this case, the mean() function) to each row or column of the data set.
##The second input here denotes whether we wish to compute the mean of the rows, 1, or the columns,2. 

apply(USArrests,1,mean) ## apply() function to calculate mean of each row of the dataset.

apply(USArrests,2,var)  ## apply() function to calculate variance of each column

pr.out <- prcomp(USArrests, scale= TRUE) ## prcomp() function is used to perform PCA which centres the variable to have mean zero. 
## by using scale=TRUE, we scale the variables to have standard deviation 1

names(pr.out)

pr.out$center ## components correspond to the means  of the variables that were used for scaling prior to implementing PCA

pr.out$scale  ## components correspond to the Standard Deviation of the variables that were used for scaling prior to implementing PCA

pr.out$rotation ## The rotation matrix provides the principal component loadings i.e. each column of pr.out$rotation contains the corresponding principal component loading vector.

pr.out$x

dim(pr.out$x)

biplot(pr.out, scale = 0) ## The scale=0 argument to biplot() ensures that the arrows are scaled 

pr.out$rotation <- -pr.out$rotation
pr.out$x <- -pr.out$x
biplot(pr.out, scale = 0)

pr.out$sdev  ##  standard deviation of each principal component

pr.var <- pr.out$sdev^2
pr.var     ##  variance of each principal componen

pve <- pr.var/sum(pr.var)
pve       ## To compute the proportion of variance explained by each principal component, we simply divide the variance explained by each principal component by the total variance explained by all four principal components

plot(pve, xlab = "Principal Component", ylab = "Propertion of Variance", ylim = c(0,1), type = "b")

plot(cumsum(pve), xlab = "Principal Component", ylab = "Propertion of Variance", ylim = c(0,1), type = "b")

a <- c(1,2,8,-3)
cumsum(a)


#####################################################################
########################## 2 Exercise ###############################
#####################################################################

## 1.Generate a simulated data set with 20 observations in each of three classes (i.e., 60 observations total), and 50 variables.
#Hint: There are a number of functions in R that you can use to generate data. One example is the rnorm() function; runif() is another option. Be sure to add a mean shift to the observations in each class so that there are three distinct classes.

set.seed(383)
x  <- matrix(rnorm(20*3*50),ncol = 50)
fix(x)
dim(x)
x[1:20,10] <- x[1:20,10]+100
x[21:40,20] <- x[21:40,20]+400
x[41:60,30] <- x[41:60,30]-200

## 2.Perform PCA on the 60 observations and plot the first two principal component score vectors. Use a different colour to indicate the observations in each of the three classes. If the three classes appear separated in this plot, then continue on to part 3. If not, then return to part 1 and modify the simulation so that there is greater separation between the three classes. Do not continue to part 3 until the three classes show at least some separation in the first two principal component score vectors.

pr.out <- prcomp(x, scale = TRUE)
pr.out

pr.out$center
pr.out$scale
pr.out$sdev
pr.out$rotation
pr.out$x

dim(pr.out$x)

plot(pr.out$x[,1], pr.out$x[,2], col = c(rep(1,20), rep(2,20), rep(3,20)), scale=0)

biplot(pr.out,scale = 0)

## 3.Perform K-means clustering of the observations with K = 3. How well do the clusters that you obtained in K-means clustering compare to the true class labels?

km.out <- kmeans(x,3, nstart = 20)

table(km.out$cluster,c(rep(1,20), rep(2,20), rep(3,20)))

# quality of cluster is good and we can see it coincide the classes

## 4.Now perform K-means clustering with K = 3 on the first two principal component score vectors, rather than on the raw data. That is, perform K-means clustering on the 60 x 2 matrix of which the first column is the first principal component score vector, and the second column is the second principal component score vector. Comment on your results.

km.out <- kmeans(pr.out$x[,1:2], 3, nstart = 20)

km.out$cluster

table(km.out$cluster,c(rep(1,20), rep(2,20), rep(3,20)))

## quality of cluster good but the perfect

#####################################################################
#####################################################################

x <- matrix(c(1,1,1,2,2,1,3,3,1), ncol = 3)

x <- matrix(c(2,2,0,2,2,0,0,0,0), ncol = 3)
eigen(x)
eigen(x)

pr.out <- prcomp(x)
pr.out$center
pr.out$scale
pr.out$x
``