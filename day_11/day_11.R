#stones <- c(125, 17)
stones <- c(572556, 22, 0, 528, 4679021, 1, 10725, 2790)

##Question 1 & 2 -------------------------------------------------------------------
stones <- table(stones)
stones <- lapply(stones, function(x) x)

#keep track of all the numbers that are appearing
blink <- function(stones) {
  stones_copy <- stones
  for(i in 1:length(stones_copy)) {
      #get stone and count
      st <- names(stones_copy)[i]
      count <- stones_copy[[st]] |> as.numeric()
      
      #if nothing to add move on
      if(count == 0) next
      
      #decide what happens to it
      if(st == "0") {
        #the number of 0s becomes 1s for the next iteration
        if("1" %in% names(stones)) {
          stones[["1"]] <- stones[["1"]] + count
        } else {
          stones[["1"]] <- count
        }
        #remove 0s
        stones[["0"]] <- stones[["0"]] - count
      } else {
        if(nchar(st) %% 2 == 0) {
          #split into two numbers and add them as slots to the dictionary
          stone_1 <- substr(st, 1, nchar(st)/2) |> as.numeric() |> as.character()
          stone_2 <- substr(st, nchar(st)/2+1, nchar(st)) |> as.numeric() |> as.character()
          
          #add stone 1
          if(stone_1 %in% names(stones)) {
            stones[[stone_1]] <- stones[[stone_1]] + count
          } else {
            stones[[stone_1]] <- count
          }
          
          #add stone 2
          if(stone_2 %in% names(stones)) {
            stones[[stone_2]] <- stones[[stone_2]] + count
          } else {
            stones[[stone_2]] <- count
          }
          
          #eliminate original stone by the number of counts
          stones[[st]] <- stones[[st]] - count
          
        } else {
          #add counter to number multiplied by 2024
          st_multiplied <- as.numeric(st) * 2024
          if(as.character(st_multiplied) %in% names(stones)) {
            stones[[as.character(st_multiplied)]] <- stones[[as.character(st_multiplied)]] + count
          } else {
            stones[[as.character(st_multiplied)]] <- count
          }
          
          #eliminate original stone by the number of counts
          stones[[st]] <- stones[[st]] - count
        }
      }
  }
  return(stones)
}


##Answer 1 -------------------------------------------------------------------------
for(k in 1:25) {
  new_stones <- blink(stones)
  stones <- new_stones
}

sum <- sum(unlist(stones))
sprintf("%.2f", sum)


##Answer 2 -------------------------------------------------------------------------
for(k in 1:75) {
  new_stones <- blink(stones)
  stones <- new_stones
}

sum <- sum(unlist(stones))
sprintf("%.2f", sum)





# Not-so-honourable mention: brute force solution for part 1

blink <- function(stones) {
  new_line <- c()
  for(n in stones) {
    if(n == "0") {
      new_line <- c(new_line, 1)
    } else {
      if(nchar(n) %% 2 == 0) {
        #split in the middle
        new_line <- c(new_line, substr(n, 1, nchar(n)/2) |> as.numeric(),
                      substr(n, nchar(n)/2+1, nchar(n)) |> as.numeric())
      } else {
        new_line <- c(new_line, as.numeric(n)*2024)
      }
    }
  }
  return(new_line)
}

for(i in 1:25) {
  new_line <- blink(stones)
  stones <- new_line
}

length(stones)