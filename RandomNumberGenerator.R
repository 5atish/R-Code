######## Random Number generator Program #############
### Author: Mr. Satish Waghmare
### Version: 1
### Formula: (seed*a+b)mod(m) where a,b,m constant

  start <- Sys.time() # start time
  seed <- strtoi(substr(Sys.time(), 19, 20)) ##  to intiate random seed
  arr <- array(1:10000000) ## array to store random numbers
  n <- length(arr)
  i <- 1
  j <- i+1
  
  # Random number generator formula
  for (i in 1:n) {
    arr[i] <- (seed*12397463766 + 6004567989)%%59
    
    ## condition to provide random seed for next interation
    ifelse(arr[i]<50 || arr[i]==0, seed <- arr[i]*5+477, seed <- arr[i]*29+39) 
    i <- i+1
  }
  
  #Insertion sorting algorithm
  for (j in 2:n) {
    temp <- arr[j]
    i <- j-1
    while (i > 0 && arr[i] > temp ) {
      arr[(i+1)] = arr[i]
      i <- i-1
    }
    arr[(i+1)] = temp
  }
  
  end <- Sys.time() ## End time 
  cat("Time elapsed (in mins):", difftime(end,start,units="mins"), "\n")
  cat("Random Numbers:",arr, sep = "\n")
  
  
 
  

