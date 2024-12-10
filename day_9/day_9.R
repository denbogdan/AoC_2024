map <- readLines("day_9/input.txt")
#map <- "12345"
#split into factor
map <- strsplit(map, split = "") |> unlist() |> as.numeric()


##Question 1 -----------------------------------------------------------------------

#generate the long map
long_map <- c()
id <- 0
for(i in 1:length(map)) {
  if(i %% 2 == 1) {
    long_map <- c(long_map, rep(id, map[i]))
    id <- id + 1 
  } else {
    long_map <- c(long_map, rep(".", map[i]))
  }
}

#fill the gaps
#find the number of gaps 
n_gaps <- length(which(long_map == "."))

#get all the values to plug in order 
val_to_plug <- long_map[which(long_map != ".")] 
val_to_plug <- rev(val_to_plug[(length(val_to_plug)-n_gaps+1):length(val_to_plug)])

#reorder
long_map_reordered <- long_map
long_map_reordered[which(long_map == ".")] <- val_to_plug 
long_map_reordered <- as.numeric(long_map_reordered)
long_map_reordered <- long_map_reordered[1:(length(long_map_reordered)-n_gaps)]

#calculate checksum
sum <- 0
for(i in 1:length(long_map_reordered)) {
  sum <- sum + (long_map_reordered[i]*(i-1))
  #print(long_map_reordered[i]*(i-1))
}


##Answer 1 -------------------------------------------------------------------------
sum

##Question 2 -----------------------------------------------------------------------

#generate the long map
long_map <- list()
id <- 0
for(i in 1:length(map)) {
  if(i %% 2 == 1) {
    long_map[[i]] <- rep(id, map[i])
    id <- id + 1 
  } else {
    long_map[[i]] <- rep(".", map[i])
  }
}

n <- length(long_map)

for(i in seq(n, 1, -2)) {
  group <- long_map[[i]]
  
  #find all spaces available
  loc <- lapply(long_map, function(x) {"." %in% x}) |> unlist()
  free_spaces <- long_map[loc]
  free_space_ids <- which(loc == TRUE)
  
  #find first adequate space - check each
  found <- FALSE
  j <- 1
  #while(!found & j <= length(free_spaces)) {
  while(!found & free_space_ids[j] <= i) {
    this_space <- free_spaces[[j]]
    #check if adequate
    if(table(free_spaces[[j]])["."] >= length(group)) {
      found <- TRUE
      #find those empty cells from left to right and populate
      k <- 1
      t <- 1
      while(k <= length(group) & t <= length(this_space)) {
        if(this_space[t] == ".") {
          #overwrite
          long_map[[free_space_ids[j]]][t] <- group[k]
          k <- k + 1
          #empty the moved group
          long_map[[i]] <- rep(".", length(group))
        }
        t <- t + 1
      }
    }
    j <- j + 1
  }
}

long_map <- long_map |> unlist() |> paste0()

#calculate checksum
sum <- 0
for(i in 1:length(long_map)) {
  if(long_map[i] != ".") {
    sum <- sum + (as.numeric(long_map[i])*(i-1))
  }
}


##Answer 2 -------------------------------------------------------------------------
sprintf("%.2f", sum)
