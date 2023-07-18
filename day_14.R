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

#part2----------
n_sand <- 1L
snd <- 500L
for (im in 0:max(Im(rock)) + 1L) {
  hlp <- unique(c(snd, snd + 1, snd - 1L))
  snd <- (-im):im + 500L
  snd <- snd[match(snd, Re(rock[Im(rock) == im]), 0L) == 0L]
  snd <- snd[snd %in% hlp]
  n_sand <- n_sand + length(snd)
}

n_sand



#explanation part2:
## after all units of sand have fallen, the sand forms a symmetric triangle.
### the triangles top is at x = 500, y = 0 and the bottom is at max(y) + 1.
### Every coordinate inside this triangle will be filled with sand unless is below 
###    a long rock formation.
### We fill the rock up starting from the top,
###   the first layer (y = 0) has 1 unit of sand, the second layer (y = 1) has 2 units, etc
###   to check whether a certain spot can be reached by sand, we check the previous layer of sand.
#### 
