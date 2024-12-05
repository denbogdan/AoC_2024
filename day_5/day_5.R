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
    n <- x[i]
    
    #grab all elements after
    rest <- x[(which(x == n)+1):length(x)]
    
    #grab all elements permitted after
    permitted <- rules$after[which(rules$before == n)]
    
    #check all there is in the rest is permitted
    correct <- (sum(rest %in% permitted) == length(rest))
    
    i <- i+1
  }

  #add to sum
  if(correct) {
    #select the middle element and add to tally 
    sum <- sum + x[ceiling(length(x)/2)]
  }
}

##Answer 1 -------------------------------------------------------------------------
sum


##Question 2 -----------------------------------------------------------------------
