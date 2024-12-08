data <- readLines("day_7/input.txt")
data <- strsplit(data, split = ":")

#read in results 
result <- lapply(data, function(x) as.numeric(x[1])) |> unlist()

factors <- lapply(data, function(x) strsplit(x[2], split = " ") |> unlist() |> as.numeric()) 
factors <- lapply(factors, function(x) x[!is.na(x)])


##Question 1 -----------------------------------------------------------------------

#operations
ops <- c("*", "+")

#initialise sum
sum_1 <- 0
#keep a copy of the indeces that did work out
working <- c()

for(k in 1:length(result)) {
  r = result[k]
  f = factors[[k]]
  
  #get all combinations of these for the needed number of spaces
  variants <- expand.grid(rep(list(ops), length(f)-1))
  
  #for each combination, compute the result
  for(i in 1:nrow(variants)) {
    #create the vector of operators
    v <- variants[i,] 
    
    #go along each set and calculate
    calc <- f[1]
    for(j in 1:length(v)) {
      #perform operation
      if(v[j] == "+") {
        calc <- calc + f[j+1]
      } else {
        calc <- calc * f[j+1]
      }
    }
    if(calc == r) {
      sum_1 <- sum_1 + r
      working <- c(working, k)
      break
    }
  }
}

##Answer 1 -------------------------------------------------------------------------
sum_1


##Question 2 -----------------------------------------------------------------------

not_working <- setdiff(seq(1:length(result)), working)

#operations
ops <- c("*", "+", "|")

#maximum number of operators needed in one expression
max <- lapply(factors, length) |> unlist() |> max() - 1

#make list of variants 
variants_list <- list()
for(i in 1:max) {
  variants_list[[i]] <- expand.grid(rep(list(ops), i))
}

a <- Sys.time()

sum_2 <- 0
for(k in not_working) {
  r = result[k]
  f = factors[[k]]
  
  #get all combinations of these for the needed number of spaces
  variants <- variants_list[[length(f)-1]]
  
  #for each combination, compute the result
  for(i in 1:nrow(variants)) {
    #create the vector of operators
    v <- variants[i,] 
    
    #only bother if there is a | operator
    if(length(which(v == "|")) > 0) {
      #concatenate the two numbers on either side
      calc <- f[1]
      for(j in 1:length(v)) {
        #perform operation
        if(v[j] == "+") {
          calc <- calc + f[j+1]
        } else {
          if(v[j] == "*") {
            calc <- calc * f[j+1]
          } else {
          calc <- paste0(calc, f[j+1]) |> as.numeric()
        }
        }
        if(calc > r) break
      }
    if(calc == r) {
        sum_2 <- sum_2 + r
        break
    }
  }
}
}

sum_2

##Answer 2 -------------------------------------------------------------------------
sum <- sum_1 + sum_2

sprintf("%.5f", sum)

b <- Sys.time()
a-b