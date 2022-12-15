f <- \(x) sapply(strsplit(x, ","), \(x) sum(as.integer(x) * c(1, 1i)))
data14 <- lapply(strsplit(readLines("Input/day14.txt"), " -> "), f)

c_seq <- \(x, y) Re(x):Re(y) + Im(x):Im(y)*1i


rock <- complex()
for (k in seq_along(data14)) {
  for (j in seq_along(data14[[k]])[-1]) {
    rock <- unique(c(rock, c_seq(data14[[k]][j - 1], data14[[k]][j])))
  }
}

drop_sand <- function(rock) {
  
  sand_vec <- 500 + 0i
  
  for (k in 0:100000) {
    sand <- sand_vec[1]
    
    while (TRUE) {
      idx <- Re(rock) == Re(sand) & Im(rock) > Im(sand)
      if (!any(idx)) return(k)
      bot <- Re(sand) + min(Im(rock[idx]))*1i
      sand_vec <- c(Re(sand) + (Im(bot) - 1):Im(sand)*1i, sand_vec)
      if (!(bot - 1) %in% rock) sand <- bot - 1
      else if (!(bot + 1) %in% rock) sand <- bot + 1
      else {
        sand_vec <- unique(sand_vec)[-1]
        rock <- c(rock, bot - 1i)
        break
      }
    }
  }
}

drop_sand(rock)

#part2-----------
sand <- 500 + 0i
for (im in 0:max(Im(rock)) + 1) {
  z <- setdiff((-im):im + 500 + im*1i, rock)
  idx <- apply(outer(z, sand[Im(sand) == im - 1], \(x, y) abs(x - y)), 1, \(x) min(x) < 2) 
  sand <- c(sand, z[idx])
}

length(sand)


