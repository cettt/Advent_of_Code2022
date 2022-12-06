data04 <- as.integer(unlist(strsplit(readLines("Input/day04.txt"), ",|\\-")))
m <- matrix(data04, ncol = 4, byrow = TRUE)

#part1----
sum(((m[,1] <= m[,3]) & m[,2] >= m[,4]) | ((m[,3] <= m[,1]) & m[,4] >= m[,2]))

#part2-------
sum((m[,3] <= m[,2] & m[,1] <= m[,4]))
