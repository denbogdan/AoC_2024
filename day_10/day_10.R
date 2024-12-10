map <- readLines("day_10/input.txt")
map <- sapply(map, function(x) strsplit(x, split = ""))

#parse the data
m_mat <- map[[1]]
for(i in 2:length(map)) {
  m_mat <- rbind(m_mat, map[[i]])
}

#pad the matrix
m_mat <- rbind( rep(".", ncol(m_mat)), m_mat, rep(".", ncol(m_mat)))
m_mat <- cbind( rep(".", nrow(m_mat)), m_mat, rep(".", nrow(m_mat)))

##Question 1 -----------------------------------------------------------------------

#encode the matrix as named list of neighbours
m_hash <- list()
for(i in 2:(nrow(m_mat)-1)) {
  for(j in 2:(ncol(m_mat)-1)) {
    m_hash[[paste(m_mat[i,j], i, j)]] <- c(paste(m_mat[i-1,j], i-1, j), #up
                                           paste(m_mat[i+1,j], i+1, j), #down
                                           paste(m_mat[i,j-1], i, j-1), #left
                                           paste(m_mat[i,j+1], i, j+1)) #right
  }
}

#dfs function
dfs <- function(value, i, j) {
  #collate node name
  node <- paste(c(value, i, j), collapse = " ")
  
  #check each neighbour and progress if it satisfies condition
  if(value == 9) {
    counter <<- counter + 1
    ends_visited <<- c(ends_visited, node)
  } else {
    for(n in m_hash[[node]]) {
      value_n <- strsplit(n, " ")[[1]][1] 
      #parse node information
      if(value_n != ".") {
        value_n <- value_n |> as.numeric()
        i_n <- strsplit(n, " ")[[1]][2] |> as.numeric()
        j_n <- strsplit(n, " ")[[1]][3] |> as.numeric()
        
        #if one step increase, keep going
        if(value_n == value + 1) {
          dfs(value_n, i_n, j_n)
        }
      }
    }
  }
}

#find all the start positions
start_positions <- names(m_hash)[grep("^0", names(m_hash))]

sum <- 0
n_tails <- 0
for(node in start_positions) {
  #initialise counter for the paths
  counter <- 0
  ends_visited <- c()
  
  #parse start information
  value <- strsplit(node, " ")[[1]][1] |> as.numeric()
  i <- strsplit(node, " ")[[1]][2] |> as.numeric()
  j <- strsplit(node, " ")[[1]][3] |> as.numeric()
  
  #recurse 
  dfs(value, i, j)
  
  #add to scores
  sum <- sum + length(unique(ends_visited))
  
  #add to tails
  n_tails <- n_tails + counter
}


##Answer 1 -------------------------------------------------------------------------
sum


##Answer 2 -------------------------------------------------------------------------
n_tails

