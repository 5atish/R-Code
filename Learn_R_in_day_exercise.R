#### Exercise chapter 1

###The length of the River Thames is 346 km and the River Severn is 354 km 

###a) How much longer is the River Severn than the Thames?

Rtt <- 346 # length of the River Thames is 346 km
Rss <- 354 # length of the River Severn is 354 km

Rlong <- Rss - Rtt
Rlong  # 8 km longer is the River Severn than the Thames

### b) How many times longer is the River Severn than the Thames?

Rtl <- Rss/Rtt
Rtl   # 1.023 times longer is the River Severn than the Thames?

### 2. Write and test a function called 'multiplier' which multiples together the values 12.8, 19.2 and ??. N.B. ?? is stored internally by R to six decimal places and can be invoked by typing pi.


pi <- 3.141592
x <- 12.8
y <- 19.2
multiplier(x,y, pi)
{
  result <- x*y*pi
  print(result)
}


########### 2.  Sequences and Subscripting ################

f <- 2:12
#f
f[f>5]

ndf <- data.frame(X = 221:230, Y = round(seq(from = 150, to = 200, length = 10)))

ndf
ndf[4:8,]
ndf$Y[c(3,8)]
2*4:7


seq(from = 2, to = 12, length = 25)

seq(from = 10, length = 50, by = 20)

###1. Define an object called  a with values 60 to 85 inclusive. 

a <- 60:85
a
###2. Determine element 12 of  a. 

a[12]

#3. Determine element 20 of  a. 

a[20]

###4. Display the 5th and 25th elements of  a. 

a[c(5,25)]

###5. Display the 4th, 8th, 12th, 16th, 20th and 24th elements of  a

a[c(4,8,12,16,20,24)]

#6. Display all but the 18th element of  a.

a[-18]

###7. Display all but the 2nd, 15th and 17th elements of  a. 

a[c(-2,-15,-17)]

###8. Explain the difference between  1:n-1 and  1:(n-1). 

###9. Generate a data frame called  new_frame with 3 columns: the first with values 33 to 47; the second starting at 115 and ending at 157, with increments of 3; and the third starting at 10, with 15 elements, and each element decreasing by 8.5. Each column should be called  X,  Y and  Z respectively. 

new_frame <- data.frame(X = 33:47, Y = seq( from = 115, to = 157, by = 3), Z = seq( from = 10, length = 15, by = -8.5))

new_frame

###10. Extract all values from column 1. 

new_frame[,1]

###11. Extract rows 6 to 10 from column 2.

new_frame[6:10,2] 

###12. Extract the values in row 11 for all columns. 

new_frame[11,]

###13. Extract the values for rows 4 to 8 from columns 1 and 3 in data frame format. 

new_frame[4:8,c(1,3)]

###14. Demonstrate how to show the number of rows in  new_frame. 

nrow(new_frame) ## to show the number of rows
ncol(new_frame) ## to show the number of colums
dim(new_frame)  ## to show dimension

###15. Demonstrate four different methods of extracting all row values from column 1 of  new_frame.

new_frame[1,]
new_frame[1,c(1,2,3)]
new_frame[1:1,]
new_frame[1:1,c(1,2,3)]

new_frame[,1]
new_frame$X
new_frame[1:15,1]
new_frame[seq(from = 1, to = 15), 1]

###################################################################
#########3.  Data Querying and Simple Statistics #################
###################################################################

#To convert this dataset into tabular format, the  data.frame function can be used

require(datasets)
sunspot
any(sunspot[,2]) ## to check if there is any negative value in column 2 of the datset

which((sunspot[,2])==0) ## to show which row has value 0
which(sunspot[,2] <= 5) ## to show row numbers with value less than or equal to 5
sunspot[which(sunspot[,2] <= 5),] ## to show rows with value less than or equal to 5
length(which(sunspot[,2] <= 5)) ## to show no of rows with value less than or equal 5

max(sunspot[,2]) # to show maximum value in column 2 of the dataset

sunspot[1,]

summary(sunspot[,2]) #To gain rapid statistical information regarding an object, or a component of an object, the summary() function can be used.

colSums(sunspot[2]) # to show total of column 2 

airquality
str(airquality) ## to show structure of dataset airquality

head(airquality) ## to view first six rows of dataset airquality
tail(airquality) ## to view last six rows of dataset airquality

aircomplete <- na.omit(airquality) # to remove rows with value 'NA' and assign it new object aircomplete

str(aircomplete)
any(is.na(aircomplete)) ## To test if a data frame contains 'NA' values, any() can be used in tandem with the logical test is.na().

View(aircomplete) # to view object aircomplete
View(airquality)
na.fail(aircomplete)
str(air_comp)

t.test(airquality[1:31,1],airquality[124:153,1])

cor.test(ToothGrowth[,1],ToothGrowth[,3]) ##To calculate a simple Pearson's correlation coefficient between column 1 and 3.

cor.test(ToothGrowth[,1],ToothGrowth[,3], method = 'k') ## The arguments  method="k" and  method="s" can be used to run Kendall's and Spearman's tests.
 
summary(airquality$Solar.R) ## summary() function is a useful means of generating a synopsis of basic statistics regarding an object, or its sub-component(s).

any(airquality$Solar.R)
is.na(airquality$Solar.R)
airquality$Solar.R
##airquality$Solar.R=="NA")


which(airquality[,2]=="NA")

mean(airquality$Solar.R, na.rm = TRUE) #To show mean of the column Solar radiation
median(airquality$Solar.R, na.rm = TRUE) #To show median of the column Solar radiation
range(airquality$Solar.R, na.rm = TRUE) #To show range (maximum and minimum) of the column Solar radiation
var(airquality$Solar.R, na.rm = TRUE) #To show sample variance of the column Solar radiation
quantile(airquality$Solar.R, na.rm = TRUE) #To show vector containing the minimum, median, upper quantile and maximum of column Solar radiation

View(ToothGrowth)

?prop.test


############Exercise#####################

###1. Using the in-built  airquality dataset, find the mean ozone value (in ppb) measured during the study. 

mean(airquality$Ozone, na.rm = TRUE)

###2. Perform the same calculation as above, but round the answer to two decimal places. 

round(mean(airquality$Ozone, na.rm = TRUE), digits = 2)

###3. Perform a test to see if there are  NA values present in the 
##'Temp'column. 

any(is.na(airquality$Temp))

###4. Write and test a function to convert the temperature observations (column four) from °F to °C, rounded to one decimal place, using the formula:
###where TC = temperature in degrees Celsius and TF = temperature in degrees Fahrenheit. 

View(airquality)


fahtodeg <- function(x){
  result <- round((x-32)*(5/9), digits = 1)
  print(result)
}
at <- fahtodeg(airquality$Temp)

### 5.What was a) the maximum and b) the minimum temperature (in °C) measured during the study, and on which days did these occur? 

range(at)  ### min = 13.3 and max = 36.1 

which.max(at) ### row no 120
airquality[120,] ### max == 36.1 on day 28

airquality[which.min(at),] ### min == 13.3 at day 5

###6. How many days had wind speeds equal to or less than 4 mph? 


length(which(airquality[,3]<=4)) ## 5  days had wind speeds equal to or less than 4 mph
airquality[which(airquality[,3]<=4),]


###7. Display the rows (and all columns) where solar radiation is greater than 300 Langleys.


airquality[which(airquality[,2]>300),]

###8. Demonstrate two different ways of establishing the median ozone concentration. 

median(airquality$Ozone, na.rm = TRUE)
median(airquality[,1], na.rm = TRUE)

###9. Conduct Pearson's product-moment correlation tests to determine which meteorological variable was most highly correlated with the presence of ozone. 

cor.test(airquality[,1], airquality[,2]) ## 0.3483417
cor.test(airquality[,1], airquality[,3]) ## -0.6015465 
cor.test(airquality[,1], airquality[,4]) ## 0.6983603 
cor.test(airquality[,1], airquality[,5]) ## 0.1645193
cor.test(airquality[,1], airquality[,6]) ## -0.01322565

## Temparature was most highly correlated with the presence of ozone. 

###10. Which method of vitamin C ingestion at 2.0 mg dosage was associated with the greatest mean tooth length in the sample of guinea pigs?
?ToothGrowth
ToothGrowth[which(ToothGrowth[,2] =='OJ'),]
ToothGrowth[(which(ToothGrowth[,3]==2.0)),]
mean(ToothGrowth[which(ToothGrowth[,3]==2.0 & ToothGrowth[,2] =='OJ'),1])
mean(ToothGrowth[which(ToothGrowth[,3]==2.0 & ToothGrowth[,2] =='VC'),1])

## method of vitamin C with ascorbic acid (VC) ingestion at 2.0 mg dosage was associated with the greatest mean tooth length in the sample of guinea pigs?

##############################################################
##########4.  Conditional and Iterative Execution##############
#############################################################

sum(1:3)
sqrt(16)

## the sum of a number sequence from 1 to 3 is tested for being greater than or equal to the square-root of twenty.
## else must be written on the same line as the concluding brace for  if(), in order to inform R that the conditional test is intended to be continued.

if(sum(1:3)>sqrt(20)){print("Statement is true")
  }else{"statement is false"}
 

##ifelse(conditional test, result if TRUE, result if FALSE)

ifelse(sum(1:3) >= sqrt(20), "statement is true", "statement + is false")


ifelse(sum(1:3) >= sqrt(100), "statement is true", 
       ifelse(sum(1:3) >= sqrt(120), "statement is now true", 
              "statement is still false"))

################4.2. Restricted Iteration#########################
 
for (i in 1:5) {
  print(i*3)
}

j <- 0
k <- 0
for (i in 1:5) {
  j <- j + 1
  k <- ((k+i)*j)
  print(i+k+j)
}

a <- c("apple", "Orange", "banana", "grape")
for (i in a) {
  print(i)
}

##################4.3. Unrestricted Iteration#########################

x <- 0
while (x <10) {
  print(x)
  x <- x +1
}

##########################Exercise ###################################

###1. Assign  x = 10 and  y = 25. 

x <- 10
y <- 25

###2. Print to the screen "statement is correct" under the condition that  x is less than  y. 

if(x<y){
  print("statement is correct")
}

###3. Show two ways of conditionally executing a statement which would print "statement is correct" if  x is less than or equal to  y, and otherwise print "statement is incorrect" if this is not the case. 

if(x>y){
  print("statement is correct")
}else{
  ("statement is incorrect")
}

###4. Assign  z = 50 and construct an  ifelse() function which tests if  x is greater than  y, printing "true" if the condition is met. Nest within this a second conditional test which prints "one true, one false" or "both false" if  x is greater than  z. 

z <- 50
ifelse(x>y, print("true"), 
ifelse(x>z, print("one true, one false"), "both false"))

###5. Use restricted iteration to generate a sequence of numbers from one to twenty. 

for (i in 1:20) {
  print(i)
}

###6. Use restricted iteration to conduct the calculation  (i^2)-i, where  i ranges between five and ten. 

for (i in 5:10) {
  print((i^2)-i)
}

###7. Use restricted iteration to generate a sequence of letters spelling "PIRATE". 

p <- c("P","I","R","A","T","E")
for (i in p) {
  print(i)
}

###8. Use unrestricted iteration to generate a sequence of integers from minus ten to minus twenty.

  t <- -10
  while(t>(-21)){
    print(t)
    t <- t-1
  }


###9. Use conditional execution to test if  h (= 10) is less than  j (= -5). If true, print "statement is true", and if false, use unrestricted iteration to generate a sequence of numbers between  h and  j. 

h <- 10
j <- -5
if(h<j){
  print("statement is true")
}else{
  while (h>j) {
    print(h)
    h <- h-1
  }
}

###10. Conduct question nine using restricted rather than unrestricted iteration.

h <- 10
j <- -5
if(h<j){
  print("statement is true")
}else{
if(h>j){
i <- 10  
  for (i in h:j) {
    print(i)
    i <- i-1
  }
}
  }


#############################################################
##############5.Reading In and Writing Out Data############
#############################################################


getwd() # to show current working directory
setwd("D:/Programming/Learn R in a day") ## to set current working directory

AirPassengers
View(AirPassengers)

AP <- read.table("Airpassengers.txt", header = TRUE, sep = "") # to read the data from dataset Airpassengers.txt file located at current working directory read.table function is used.
## header=TRUE indicates that the first line of the file contains the names of the variables.
##sep="" indicates the field separator i.e. what is separating values in the field

##sep=" " indicates space-delimited data 
##sep="\t" indicates tab-delimited data 
##sep="\n" indicates 'new-line' delimited data


AP$Annaul_sum <- cbind(rowSums(AP))  ## functions cbind() (to add a new column) and rowSums() are used in tandem with $ (to create a new column called 'Ann_Sum')

AP
AP <- rbind(colSums(AP))

## To save an object created in R to disk, the  write.table() function is used in

write.table(AP,"AP_annaul.txt",col.names = NA,row.names = TRUE, quote = FALSE, sep = "\t")

## In above example rowname are required as it repesent associated years and column name represent hold month variable 
## quote = FALSE represent quotation mark ("") being displayed around every value within the object which is not required and removed.

write.table(AP,"AP_annaul.txt",col.names = NA,row.names = TRUE, quote = FALSE, sep = "\t")

?cbind()
?rbind

####1. The in-built  nottem dataset contains mean temperature observations (in degrees Fahrenheit) for 1920 to 1939 at Nottingham Castle. Inspect the data by loading it into R and then copy and paste the data into a text editor (e.g. Notepad or Word) file. Add a comment at the top of the file to provide brief details about the dataset. Save this file onto the desktop as "nott_castle.txt".

nottem


###2. Set the working directory and check it has been set correctly

setwd("D:/Programming/Learn R in a day")
getwd()

###3. Read the external file into R, using the arguments to  read.table() as required and create a new object called  nott_temp. 

nott_temp <- read.table("nott_castle.txt", header = TRUE, sep = "")
nott_temp

###4. Inspect  nott_temp to ensure it has been read in successfully and formatted sensibly.

nott_temp

###5. Use the equation below to convert the values in  nott_temp from °F to °C, with rounding to one decimal place. Call the new object  nott_celsius.
###TC = temperature in degrees Celsius and TF = temperature in degrees Fahrenheit


nott_celsius <- function(x){
  result <- round((5/9)*(x-32), digits = 1)
  print(result)
}
nott_celsius(nott_temp)


###6. Add an additional column to  nott_celsius called 'Annual_Mean' which contains the mean annual temperature for each year, rounded to one decimal place. 

nott_temp$Annual_mean <- cbind(rowSums(nott_temp))
nott_temp

###7. Export  nott_celsius as a new tab-delimited text file on the desktop called "nott_castle_celsius.txt".  Open the file to ensure that

write.table(nott_temp,"nott_castle_celsius.txt", col.names = NA, row.names = TRUE, quote = FALSE, sep = "\t")

###################################################################
#######################6.Graphics###############################
##################################################################

################6.1. Standard Graph Types#####################

VADebahs
barplot(VADeaths) # Death rates by age group (particulars  + not shown) per 1000 in Virginia in 1940

boxplot(len ~ dose, ToothGrowth, subset = supp=="VC") # Tooth growth in guinea pigs (y-axis, mm) in response to three dose levels of vitamin C ingestion by orange juice (x-axis, mg)

hist(islands) 

pie(1:15, col = rainbow(15)) # rainbow palette in fifteen segments

plot(cars) # stopping distance of cars traveling at various speed

trees

##################6.2. Scatter and Line Graphs#################

plot(trees$Girth,trees$Height, type = "p") ## plot with type points
plot(trees$Girth,trees$Height, type = "l") ## plot with type lines
plot(trees$Girth,trees$Height, type = "b") ## plot with type points and lines  

plot(trees$Girth,trees$Height, type = "l", lty = 4) ## plotting line can be changed from solid to dotted or dash using 'lty' argument in plot function with value ranging from 1 to 6 

plot(trees$Girth,trees$Height, type = "l", lwd = 2) ## plotting line width can be changed 

####################6.3. Formatting Axes##########################

plot(trees$Girth,trees$Height, xlab = "Tree Girth (Inches)", ylab = "Tree height (feet)") ## x and y axis titles

plot(trees$Girth,trees$Height, xlim = c(8,21), ylim = c(60,90)) ## x and y axis minimum and maximum values

plot(trees$Girth,trees$Height, las = 0) ## axis label values parallel to axis

plot(trees$Girth,trees$Height, las = 1) ## axis label values always horizontal

plot(trees$Girth,trees$Height, las = 2) ## axis label values perpendicular to the axis

plot(trees$Girth,trees$Height, las = 3) ## axis label values always vertical     

plot(trees$Girth,trees$Height, las = 0) ## axis label values parallel to axis     

#################6.4. Changing the Plotting Characters##############

plot(trees$Girth,trees$Height, pch = 11) ## plotting character can be change by 'pch' argument in plot function ranging vales from 1 to 25

plot(trees$Girth,trees$Height, pch = 16, cex = 2) ## plotting size can be  change by using 'cex' argument in plot function

plot(trees$Girth,trees$Height, pch = 16, col = "springgreen4") ## plotting colour can be change using col argument

plot(trees$Girth,trees$Height, type = "l", lty = 2)

#################6.8. Adding a Line of Best Fit#####################

abline(lm(trees$Girth~trees$Height)) ## to plot best fit regression line abline () function is used nested with lm() ('linear model')function

par(mar=c(5,4,4,4)) ### margin of plotting region can be adjusted using par function 

plot(trees$Girth, trees$Height, xlab = "Trees girth (Inches)", ylab = "Trees height (feet)", las = 3, pch = 16, col="springgreen")

par(new=TRUE)

plot(trees$Girth, trees$Volume, axes = FALSE, xlab = "", ylab = "", pch = 4, cex = 2, col = "blue" )

axis(side = 2, las = 1) # axes can be added using side argument( 1= bottom, 2= left, 3= top, 4= right )

mtext(side = 4, line = 2.5, "Timber volume (cubic feet)") # axis label can be added using mtext() function

###############6.10. Adding a Legend#############################

## the function legend() is another plotting command for implementing after displaying the graph.

legend("topleft", c("Tree Height", "Tree Volume"), pch = c(16,4), col=c("springgreen", "blue"), bty="n") ## bty = "n" suppress the drawing of box around the legend graph.

###############6.11. Adding a Title#############################

## Title can be added using function title()

title("Heigth, Girth, Volume Measurements for 31 trees")


################Exercise#######################################


plot(sunspot$Year, sunspot$Count, xlab = "Year", ylab = "Sunspot Count", xlim = c(1700,2000), ylim = c(0,200), las = 1, type = "l", lty = 1, col = "darkorange", lwd = 2)

title("Sunspot Count Observation between 1700 and 1988")

getwd()


savePlot("Sunspot_Count.jpg", type = "jpg")

plot(airquality$Ozone, airquality$Temp, xlab = "Ozone Concentration (ppb)", ylab = "Temperature (degrees Fahrenheit)", xlim = c(0,180), ylim = c(50,100), las = 1, pch = 18, col = "red",cex = 1.2) 

abline(lm(airquality$Temp ~ airquality$Ozone), col = "red", lty = 2, lwd = 1.5)

