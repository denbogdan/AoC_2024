library(stringr)

#read in data
s <- readLines("day_3/input.txt")

##Question 1 -----------------------------------------------------------------------
find_pairs <- function(string) {
  #extract tuples
  tuples <- str_extract_all(string, "mul[\\(][0-9]+,[0-9]+[\\)]") |> unlist()
  #parse the numbers
  n <- lapply(tuples, function(x) gsub("mul\\(", "", x))
  n <- lapply(n, function(x) gsub("\\)", "", x))
  
  sum <- 0
  for(pair in n) {
    n1 <- sub(".*,", "", pair) |> as.numeric()
    n2 <- sub(",.*", "", pair) |> as.numeric()
    
    if(n1 < 1000 & n2 < 1000)
    sum <- sum + (n1*n2)
  }
  
  return(sum)
}

sum <- 0
for(line in s) 
  sum <- sum + find_pairs(line)

##Answer 1 -------------------------------------------------------------------------
sum


##Question 2 -----------------------------------------------------------------------
line <- paste(s, collapse = "\n")

#split by DON'T
x = str_split(line, "don't\\(\\)") |> unlist()
x_plus <- c()
for(i in 1:length(x))
  x_plus <- c(x_plus, x[i], "don't")
x_plus <- x_plus[1:(length(x_plus)-1)]

#further split by DO
y <- strsplit(x_plus, "do\\(\\)")
y_plus <- lapply(y, function(x) {
  if(length(x) >= 2) {
    x_plus <- c()
    for(i in 1:length(x))
      x_plus <- c(x_plus, x[i], "do")
    return(x_plus[1:(length(x_plus)-1)])
  } else return(x)
})

y_plus <- c("do", y_plus |> unlist())

#only search and add the pairs after a DO
sum <- 0
for(i in 1:(length(y_plus)-1)) {
  if(y_plus[i] == "do") {
    to_calculate <- y_plus[i+1]
    sum <- sum + find_pairs(to_calculate)
  } 
}

##Answer 2 -------------------------------------------------------------------------
sum

