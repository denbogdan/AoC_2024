input <- readLines("day_5/input.txt")

#sorting rules
rules <- grep("|", input, value = TRUE)
rules <- strsplit(rules, split = "|", fixed = TRUE)
rules <- lapply(rules, as.numeric)
rules <- do.call(rbind.data.frame, rules)
colnames(rules) <- c("before", "after")

#manuals to sort
manuals <- grep(",", input, value = TRUE)
manuals <- strsplit(manuals, split = ",", fixed = TRUE)
manuals <- lapply(manuals, as.numeric)


##Question 1 -----------------------------------------------------------------------
sum <- 0
for(x in manuals) {
  #check for correctness
  correct <- TRUE
  i <- 1
  while(correct & i < length(x)) {
    #grab all elements after
    rest <- x[(which(x == x[i])+1):length(x)]
    
    #grab all elements permitted after
    permitted <- rules$after[which(rules$before == x[i])]
    
    #check all there is in the rest is permitted
    correct <- (sum(rest %in% permitted) == length(rest))
    
    i <- i+1
  }

  #add to sum
  if(correct) {
    #select the middle element and add to sum 
    sum <- sum + x[ceiling(length(x)/2)]
  }
}

##Answer 1 -------------------------------------------------------------------------
sum


##Question 2 -----------------------------------------------------------------------
sum <- 0

for(x in manuals) {
  #check for correctness
  correct <- TRUE
  i <- 1
  while(correct & i < length(x)) {
    #grab all elements after
    rest <- x[(which(x == x[i])+1):length(x)]
    
    #grab all elements permitted after
    permitted <- rules$after[which(rules$before == x[i])]
    
    #check all there is in the rest is permitted
    correct <- (sum(rest %in% permitted) == length(rest))
    
    i <- i+1
  }
  
  #sort if incorrect
  if(!correct) {
    #bubble sort
    swap_performed <- TRUE
    
    #loop through until they are all in order
    while (swap_performed) {
      swap_performed <- FALSE
      
      for (i in 1:(length(x) - 1)) {
        #find the permitted neighbours
        permitted <- rules$after[which(rules$before == x[i])]
        
        #check if current neighbour is permitted
        if (!(x[i+1] %in% permitted)) {
          #if not, swap
          tmp <- x[i]
          x[i] <- x[i + 1]
          x[i + 1] <- tmp
          
          #continue looping until there are no swaps left to perform
          swap_performed <- TRUE
        }
      }
    }
    
    #select the middle element and add to sum 
    sum <- sum + x[ceiling(length(x)/2)]
  }
}

##Answer 2 -------------------------------------------------------------------------
sum

