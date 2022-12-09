data09 <- read.table("Input/day09.txt", sep = " ")

dir <- c("U" = 1i, "D" = -1i, "L" = -1, "R" = 1)
hp <- unname(c(0i, cumsum(unlist(apply(data09, 1, \(x) rep(dir[x[1]], x[2]))))))

move_knot <- function(x) {
  for (k in seq_along(x)[-1]) {
    d <-  x[k] - x[k - 1]
    x[k] <- x[k - 1] + if (abs(d) >= 2) sign(Re(d)) + sign(Im(d))*1i else 0
  }
  return(x)
}

#part1----
tp <- move_knot(hp)
length(unique(tp))

#part2----
length(unique(Reduce(\(x,y) move_knot(x), 3:10, init = tp)))