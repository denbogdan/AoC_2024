#read in data
l <- read.delim("day_1/input.txt", header = FALSE, sep = " ")

##Answer 1 -------------------------------------------------------------------------
sum(abs(sort(l$V1) - sort(l$V4)))


##Question 2 -----------------------------------------------------------------------
sum <- 0
for(x in l$V1) {
  #find multiplication factor
  f <- (table(x == l$V4)["TRUE"]) |> as.numeric()
  #if there is an occurrence in the other list, add
  if(!is.na(f))
    sum <- sum + (x*f)
}

##Answer 2 -------------------------------------------------------------------------
sum
