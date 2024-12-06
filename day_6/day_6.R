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


##Question 1 -----------------------------------------------------------------------

start <- which(m_mat == "^", arr.ind = TRUE)
m_mat[start] <- "X"
current_pos <- start
current_direction <- c(-1, 0)
i = current_pos[1]
j = current_pos[2]
counter <- 2

positions_visited <- list(start)

while(m_mat[i+ current_direction[1],j+ current_direction[2]] != "O") {
  
  if(m_mat[i + current_direction[1], j + current_direction[2]] == "." | m_mat[i + current_direction[1], j + current_direction[2]] == "X") {
    current_pos <- c(i + current_direction[1], j + current_direction[2])
  } else {
    #turn right 
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
  
  i = current_pos[1]
  j = current_pos[2]
  
  m_mat[i,j] <- "X"
  
  if(m_mat[i + current_direction[1], j + current_direction[2]] != "O") {
    positions_visited[[counter]] <- current_pos
    counter <- counter + 1
  }
}

##Answer 1 -------------------------------------------------------------------------
length(unique(positions_visited)) + 1
