map <- readLines("day_6/input.txt")
map <- sapply(map, function(x) strsplit(x, split = ""))

#parse the data
m_mat <- map[[1]]
for(i in 2:length(map)) {
  m_mat <- rbind(m_mat, map[[i]])
}

#pad the matrix
m_mat <- rbind( rep("O", ncol(m_mat)), m_mat, rep("O", ncol(m_mat)))
m_mat <- cbind( rep("O", nrow(m_mat)), m_mat, rep("O", nrow(m_mat)))

#intact copy for part 2
m_mat_pt2 <- m_mat

##Question 1 -----------------------------------------------------------------------
#find the start and mark
start <- which(m_mat == "^", arr.ind = TRUE)
m_mat[start] <- "X"

#initialise vars for looping
current_pos <- start
current_direction <- c(-1, 0)
i = current_pos[1]
j = current_pos[2]
#counter needed for updating list of visited positions 
counter <- 2

#declare positions visited
positions_visited <- list(start)

#crawl matrix
while(m_mat[i+ current_direction[1],j+ current_direction[2]] != "O") {
  
  #if unoccupied continue in same direction
  if(m_mat[i + current_direction[1], j + current_direction[2]] == "." | m_mat[i + current_direction[1], j + current_direction[2]] == "X") {
    current_pos <- c(i + current_direction[1], j + current_direction[2])
  } else {
    #if otherwise, change direction - turn right 
    #if facing north
    if(sum(current_direction == c(-1,0)) == 2) {
      current_direction <- c(0,1)
    } else {
      #east
      if(sum(current_direction == c(0,1)) == 2) {
        current_direction <- c(1,0)
      } else {
        #south
        if(sum(current_direction == c(1,0)) == 2) {
          current_direction <- c(0,-1)
        } else {
          #west
          if(sum(current_direction == c(0,-1)) == 2) current_direction <- c(-1,0)
        }
      }
    }
    
    #update position
    current_pos <- c(i + current_direction[1], j + current_direction[2])
  }
  
  #move on to next cell and mark visited
  i = current_pos[1]
  j = current_pos[2]
  m_mat[i,j] <- "X"
  
  
  positions_visited[[counter]] <- current_pos
  counter <- counter + 1
}


##Answer 1 -------------------------------------------------------------------------
table(m_mat)["X"]


##Question 2 -----------------------------------------------------------------------
p <- which(m_mat == "X", arr.ind = TRUE) %>% as.data.frame() %>% distinct()

#restore intact copy
m_mat <- m_mat_pt2

#find the start and mark
start <- which(m_mat == "^", arr.ind = TRUE)
m_mat[start] <- "X"

#loop through visited positions and make each an obstacle 
found <- 0

#for(pair in unique(positions_visited_part1)[2:length(unique(positions_visited_part1))]) {
for(k in 1:nrow(p)) {
  m = p[k,1]
  n = p[k,2]
  
  #make map copy 
  m_mat_copy <- m_mat
  
  #put an obstacle
  m_mat[m,n] <- "#"
  
  #on this newly created map - try to figure out if it's a loop
  counter <- 0
  
  #initialise vars for looping
  current_pos <- start
  current_direction <- c(-1, 0)
  i = current_pos[1]
  j = current_pos[2]
  
  #keep going if no proof of looping 
  CARRYON <- TRUE
  obst_visited <- 0
  
  #crawl matrix
  while(m_mat[i+ current_direction[1],j+ current_direction[2]] != "O" & CARRYON) {
    #check if the position ahead is the new obstacle
    #if so, increment how many times it has been encountered
    if((i+current_direction[1] == m) & (j+current_direction[2] == n)) 
      obst_visited <- obst_visited + 1
    
    if(obst_visited > 2) CARRYON <- FALSE
    if(counter > 100000) CARRYON <- FALSE
    
    #if not looped
    #if unoccupied - continue in same direction
    if(m_mat[i + current_direction[1], j + current_direction[2]] == "." | m_mat[i + current_direction[1], j + current_direction[2]] == "X") {
      current_pos <- c(i + current_direction[1], j + current_direction[2])
    } else {
      #if otherwise - change direction - turn right 
      #if facing north
      if(sum(current_direction == c(-1,0)) == 2) {
        current_direction <- c(0,1)
      } else {
        #east
        if(sum(current_direction == c(0,1)) == 2) {
          current_direction <- c(1,0)
        } else {
          #south
          if(sum(current_direction == c(1,0)) == 2) {
            current_direction <- c(0,-1)
          } else {
            #west
            if(sum(current_direction == c(0,-1)) == 2) current_direction <- c(-1,0)
          }
        }
      }
      
      #corner case - only update position if the next symbol is not an obstacle
      if(m_mat[i + current_direction[1], j + current_direction[2]] != "#") {
        #update position
        current_pos <- c(i + current_direction[1], j + current_direction[2])
      }
    }
    
    #move on to next cell and mark visited
    i = current_pos[1]
    j = current_pos[2]
    m_mat[i,j] <- "X"
    
    counter <- counter + 1
  }
  
  #add loops to tally
  if(!CARRYON) found <- found + 1
  
  #restore original map 
  m_mat <- m_mat_copy
}

##Answer 2 -------------------------------------------------------------------------
found
