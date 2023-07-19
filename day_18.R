data18 <- unname(t(as.matrix(read.table("Input/day18.txt", sep = ","))))

#part1-----
sum(apply(data18, 2, \(x) 6L - sum(colSums(abs(data18 - x)) == 1L)))

#part2------
data18_int <- colSums(data18 * 10L^(2:0 * 2L))

hlp <- \(x) seq.int(x[1] - 1L, x[2] + 1L)
cube <- expand.grid(hlp(range(data18[1, ])), hlp(range(data18[2, ])), hlp(range(data18[3, ])))  

outside <- cube[cube[,1] %in% range(cube[,1]) | cube[,2] %in% range(cube[,2]) |
                  cube[,3] %in% range(cube[,3]), ]

cube_int <- unname(colSums(t(cube) * 10L^(2:0 * 2L)))

outside <- unname(colSums(t(outside) * 10L^(2:0 * 2L)))
j <- 1L

while (j <= length(outside)) {
  new_edge <- outside[j] + c(1L, -1L, 100L, -100L, 1e4L, -1e4L)
  new_edge <- new_edge[match(new_edge, outside, 0L) == 0L]
  new_edge <- new_edge[new_edge %in% cube_int & !new_edge %in% data18_int]
  outside <- c(outside, new_edge)
  j <- j + 1L
}

sum(sapply(data18_int, \(x) sum(abs(outside - x) %in% 10L^(2:0 * 2L))))
