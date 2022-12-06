data06 <- strsplit(readLines("Input/day06.txt"), "")[[1]]

find_dupl <- \(n) sapply(n:length(data06), \(k) anyDuplicated(data06[1:n + k - n]) == 0)  

#part 1 and 2--------
sapply(c(4L, 14L), \(n) which(find_dupl(n))[1] + n - 1L)
