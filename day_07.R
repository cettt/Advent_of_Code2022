data07 <- readLines("Input/day07.txt")

size <- 0L
idx <- 1L

for (k in data07[-1]) {
  
  if (grepl("\\d", k)) size[idx] <- size[idx] + as.integer(gsub("\\D", "", k))
  else if (grepl("\\.\\.", k)) idx <- idx[-1]
  else if (grepl("cd", k)) {
    size <- c(size, 0L)
    idx <- c(length(size), idx)
  } 
}

#part1---------
sum(size[size <= 100000L])

#part2-----
min(size[size >= -40000000L + size[1]])
