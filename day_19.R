data19 <- readLines("Input/day19.txt")
cost <- sapply(regmatches(data19, gregexpr("\\d+ " , data19)), as.integer)

f <- function(mat = integer(4), rob = c(1L, 0L, 0L, 0L), cst, minute, mx_tm) {
  
  if (minute == 1L) cur_best <<- 1L
  
  if (minute == mx_tm) {
    res <- mat[4] + rob[4]
  } else if (mat[4] + (mx_tm + 1L - minute) * (rob[4] + (mx_tm - minute)/2L) < cur_best) {
    return(-1L)
  } else {
    tm <- ceiling(c(
      (cst[1:2] - mat[1]) / rob[1],
      max((cst[3:4] - mat[c(1, 2)]) / rob[c(1, 2)]),
      max((cst[5:6] - mat[c(1, 3)]) / rob[c(1, 3)])
    ))
    
    tm[1:3] <- ifelse(
      mat[1:3] >= (c(max(cst[c(1:3, 5)]), cst[4], cst[6]) - rob[1:3]) * (mx_tm + 1L - minute),
      99L, tm[1:3]
    )
    
    tm <- pmax(0L, as.integer(pmin(99L, tm))) + 1L
    nxt <- .Internal(which(tm < mx_tm + 1L - minute))
    
    if (length(nxt) > 0) { 
      hlp <- matrix(rep.int(0L, 16L), 4, 4)
      hlp[c(1L, 5L, 9L, 10L, 13L, 15L)] <- cst
      res <- max(sapply(nxt, \(k) f(mat + tm[k] * rob - hlp[, k], rob + diag(4)[, k], cst, minute + tm[k], mx_tm)))
    } else {
      res <- mat[4] + rob[4] * (mx_tm + 1L - minute)
    }
  }
  
  if (res > cur_best) cur_best <<- res
  return(res)
}

#part1---------
res <- apply(cost, 2, \(x) f(cst = x, minute = 1L, mx_tm = 24L))
sum(res * seq_along(res))

#part2-----------
prod(apply(cost[,1:3], 2, \(x) f(cst = x, minute = 1L, mx_tm = 32L)))

#theoretical best
#given that we are at time t and have g geodes and r geode robots, the upper bound for
#  geodes at time T is given by g + (r) + (r + 1) + (r + 2) +... + (r + T -t) =
#    = g + (T - t + 1) * (r + (T-t)/2)