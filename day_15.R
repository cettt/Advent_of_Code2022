data15 <- sapply(strsplit(gsub("\\D+", " ", readLines("Input/day15.txt")), " "), as.integer)
dist <- abs(data15[2, ] - data15[4, ]) + abs(data15[3, ] - data15[5, ])

#part1------
d0 <- dist - abs(data15[3,] - 2e6)
x_tar <- lapply(seq_along(d0), \(k) c(-d0[k], d0[k]) + data15[2,k])
x_tar <- x_tar[d0 >= 0]

diff(range(unlist(x_tar))) + 1L - length(unique(data15[4, data15[5,] == 2e6]))
# works as intervals in x_tar are disjoint


#part2--------
check_point <- \(xy) all(colSums(abs(data15[2:3, ] - xy)) > dist)


d1 <- data15[3, ] + dist + 1 + data15[2, ]
d2 <- data15[3, ] + dist + 1 - data15[2, ]

for (k in seq_along(d1)) {
  for (l in seq_along(d1)[-k]) {
    xy <- (d1[k] + c(-d2[l], d2[l])) / 2
    if (all(xy >= 0) & all(xy <= 4e6)) if (check_point(xy)) res <- sum(xy * c(4e6, 1))
  }
}

sprintf("%.f", res)


#Explanation for part 2:
# we are looking for a point that is not contained within any of the balls 
#   with center sen and radius dist.
#  This point is unique within the square [0, 4e6]^2
# It follows that this ball has to have a distance of (dist + 1) to 
#    at least two of the balls (meaning this point is just outside of these balls)
# The boundaries of the balls can be described by straight lines with slopes +1 and -1
# In particular for each ball with center x0, y0 and radius d the  point y0 + d +1
#   is just outside of the ball
#    therefore we get the lines
#    t1: y = -x + d1 and t2: y= x + d2
# Easy algebra shows that d1 = y0 + d + 1 + x0 and d2 = y0 + d + 1 - x0
#   We intersect these lines and check whether the intersection is seen by any sensors 