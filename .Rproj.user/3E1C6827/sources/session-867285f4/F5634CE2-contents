#read in data 
reports <- readLines("day_2/input.txt")
reports <- strsplit(reports, " ")
reports <- lapply(reports, function(x) as.numeric(x))

##Question 1 -----------------------------------------------------------------------
check_safe <- function(vect) {
  safe <- FALSE
  #the differences between reports
  d <- diff(vect)
  #the safe differences
  d_safe <- which(abs(d) > 0 & abs(d) <= 3)
  
  #if the increments are correct, check signs
  if(length(d) == length(d_safe)) {
    #count the signs
    plus <- length(which(d > 0))
    minus <- length(which(d < 0))
    
    #compare to length
    if(length(d) == plus | length(d) == minus) {
      safe <- TRUE
    }
  }
  
  return(safe)
}


counter <- 0
for(r in reports) {
  safe <- check_safe(r)
  
  if(safe) counter <- counter + 1
}

##Answer 1 -------------------------------------------------------------------------
counter

##Question 2 -----------------------------------------------------------------------
counter <- 0
for(r in reports) {
  #check if currently safe
  safe <- check_safe(r)
  
  #if not currently safe, remove one by one an element and check 
  if(! safe) {
    for(i in 1:length(r)) {
      #declare new vect to check
      new_r <- r
      n = new_r[i]
      new_r[i] <- -1
      new_r <- new_r[which(new_r != -1)]
      
      #check safe
      new_safe <- check_safe(new_r)
      if(new_safe) {
        safe <- new_safe
        #print(n)
        break
      }
    }
  }
  
  if(safe)
    counter <- counter + 1
}

##Answer 2 -------------------------------------------------------------------------
counter
