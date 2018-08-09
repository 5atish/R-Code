###################### Task 2 #########################################

## Load data dataset "searches.json" contains data from one such A/B testing experiment. Each line in this file represents data of one user's interactions. 

library("jsonlite")
library("rjson")
library("plyr")
library("tidyr")

setwd("D:\Data Analysis")

## JSON file is actually 'NDJSON',so use of stream_in function from 'jsonlite' package 

searches <- stream_in(file("searches.json"))

## file() function to create a connection to accessing file from disk

str(searches)
fix(searches)
attach(searches)


# Question 1 # Did more users use the search feature in the new design (B)?

### Logic #### we count the login_count of users with odd uid as shown               #### interface B (new design) and even uid as shown interface A                 (existing design) and campare the result.
          #### Assuming each user searches with shown design

### Query #### select count (login_count) 
          #### from Searches  
          #### where uid = (uid/2)%%1 for odd,  (uid/2)%%0 for Even

count(searches$uid%%2==0) ## Count the odd and even uid for users

subset(searches, (searches$uid%%2==0)==TRUE) ## Dataset with Even uid
subset(searches, (searches$uid%%2==0)== TRUE, login_count) # freq for even uid
count(subset(searches, (searches$uid%%2==0)== TRUE, login_count)) ## count for even uid is 333

subset(searches, (searches$uid%%2==1)==TRUE) ## Dataset with odd uid
subset(searches, (searches$uid%%2==1)== TRUE, login_count) # freq for odd uid
count(subset(searches, (searches$uid%%2==1)== TRUE, login_count)) ## count for odd uid is 348

### Answer ## Yes, from above result we can say that more users use the search feature in the new design (B)




### Question 2 ### Did users search more often in the new design (B)?

### Logic #### we count the search_count of users with odd and even uid              #### and campare the result along with ignoring search_count '0'           #### which has no significant importance in this situation.
          #### And assuming each user searches with shown design

### Query #### select count (search_count) 
          #### from Searches  
          #### where uid = (uid/2)%%1 for odd,  (uid/2)%%0 for Even

count(subset(searches, (searches$uid%%2==0)== TRUE, search_count)) ## Count for even uid is 111

count(subset(searches, (searches$uid%%2==1)== TRUE, search_count)) ## Count for odd uid is 98

### Answer ## No, from above result we can say that user search more often int the existing design.
