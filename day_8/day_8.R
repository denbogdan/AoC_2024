map <- readLines("day_8/dummy.txt")
map <- sapply(map, function(x) strsplit(x, split = ""))

#parse the data
m_mat <- map[[1]]
for(i in 2:length(map)) {
  m_mat <- rbind(m_mat, map[[i]])
}

##Question 1 -----------------------------------------------------------------------

#find the locations of each symbol
symbols <- setdiff(names(table(m_mat)), ".")


