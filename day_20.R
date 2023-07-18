data20 <- as.integer(readLines("Input/day20.txt"))
n <- length(data20)


undo <- function(pp1, x) {
  res <- x
  idx <- 1
  for (k in seq_along(pp1)[-1]) {
    idx <- pp1[idx]
    res[k] <- x[idx]
  }
  return(res)
} 

run_mix <- function(times = 1L, const = 1L) {
  pp1 <- c(seq_along(data20)[-1], 1L)
  
  for (.t in seq_len(times)) {
    for (k in seq_along(data20)) {
      if (data20[k] != 0L) {
        res <- k
        for (l in seq_len(((data20[k] * const) %% (n - 1L)))) res <- pp1[res]
        hlp <- pp1[res]
        pp1[pp1 == k] <- pp1[k] 
        pp1[pp1 == hlp] <- k
        pp1[k] <- hlp  
      }
    }
  }
  res <- undo(pp1, data20 * const)
  sum(res[(which(res == 0) + 1:3*1000 - 1L) %% n + 1L])
}


#part1-------
run_mix()

#part2---------
sprintf("%.f", run_mix(10, 811589153))
