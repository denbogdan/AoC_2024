map <- readLines("day_4/input.txt")
map <- sapply(map, function(x) strsplit(x, split = ""))

#parse the data
m_mat <- map[[1]]
for(i in 2:length(map)) {
  m_mat <- rbind(m_mat, map[[i]])
}

#pad the matrix
m_mat <- rbind( rep("O", ncol(m_mat)),  rep("O", ncol(m_mat)),  rep("O", ncol(m_mat)),  rep("O", ncol(m_mat)), m_mat, 
                rep("O", ncol(m_mat)),  rep("O", ncol(m_mat)),  rep("O", ncol(m_mat)),  rep("O", ncol(m_mat)))

m_mat <- cbind( rep("O", nrow(m_mat)),  rep("O", nrow(m_mat)),  rep("O", nrow(m_mat)),  rep("O", nrow(m_mat)), m_mat, 
                rep("O", nrow(m_mat)),  rep("O", nrow(m_mat)),  rep("O", nrow(m_mat)),  rep("O", nrow(m_mat)))


##Question 1 -----------------------------------------------------------------------
counter <- 0
for(i in 5:(nrow(m_mat)-4)) {
  for(j in 5:(ncol(m_mat)-4)) {
    if(m_mat[i,j] == "X") {
      #gather the four words from each of the 8 directions
      up <- paste(m_mat[c(i,i-1,i-2,i-3),j], collapse = "")
      down <- paste(m_mat[c(i,i+1,i+2,i+3),j], collapse = "")
      left <- paste(m_mat[i,c(j, j-1, j-2, j-3)], collapse = "")
      right <- paste(m_mat[i,c(j, j+1, j+2, j+3)], collapse = "")
      
      upl <- paste(c(m_mat[i,j], m_mat[i-1,j-1], m_mat[i-2,j-2], m_mat[i-3,j-3]), collapse = "")
      upr <- paste(c(m_mat[i,j], m_mat[i-1,j+1], m_mat[i-2,j+2], m_mat[i-3,j+3]), collapse = "")
      downr <- paste(c(m_mat[i,j], m_mat[i+1,j+1], m_mat[i+2,j+2], m_mat[i+3,j+3]), collapse = "")
      downl <- paste(c(m_mat[i,j], m_mat[i+1,j-1], m_mat[i+2,j-2], m_mat[i+3,j-3]), collapse = "")
      
      #look for XMAS and count
      counter <- counter + sum(c(up,down,left,right,upl,upr,downl,downr) %in% "XMAS")
    }
  }
}

##Answer 1 -------------------------------------------------------------------------
counter


##Question 2 -----------------------------------------------------------------------
counter <- 0
for(i in 5:(nrow(m_mat)-4)) {
  for(j in 5:(ncol(m_mat)-4)) {
    if(m_mat[i,j] == "A") {
      nw <- paste(c(m_mat[i-1,j-1], m_mat[i,j], m_mat[i+1,j+1]), collapse = "")
      ne <- paste(c(m_mat[i-1,j+1], m_mat[i,j], m_mat[i+1,j-1]), collapse = "")
      if(nw %in% c("MAS", "SAM") & ne %in% c("MAS", "SAM")) 
        counter <- counter + 1
    }
  }
}

##Answer 2 -------------------------------------------------------------------------
counter




#Darling I couldn't kill - DSF finding all XMAs words, even the non-permitted ones

#encode the matrix as named list of neighbours
m_hash <- list()
for(i in 5:(nrow(m_mat)-4)) {
  for(j in 5:(ncol(m_mat)-4)) {
    m_hash[[paste(m_mat[i,j], i, j)]] <- c(paste(m_mat[i-1,j], i-1, j), #up
                                           paste(m_mat[i+1,j], i+1, j), #down
                                           paste(m_mat[i,j-1], i, j-1), #left
                                           paste(m_mat[i,j+1], i, j+1), #right
                                           paste(m_mat[i-1,j-1], i-1, j-1), #up-left
                                           paste(m_mat[i-1,j+1], i-1, j+1), #up-right
                                           paste(m_mat[i+1,j+1], i+1, j+1), #down-right
                                           paste(m_mat[i+1,j-1], i+1, j-1)) #down-left
  }
}

#dft function
dfs <- function(node, string, depth) {
  if(depth < 4) {
    #update depth
    depth <- depth + 1
    
    #parse node information
    letter <- strsplit(node, " ")[[1]][1]
    
    #update string
    string <- paste0(string, letter)
    print(string)
    
    if(string == "XMAS") {
      counter <<- counter + 1
    } else {
      for(n in m_hash[[node]]) {
        if(n %in% names(m_hash))
          dfs(n, string, depth)
      }
    }
  }
}

counter <- 0
for(node in names(m_hash)) {
  dfs(node, "", 0)
}

counter
