data19 <- lapply(strsplit(gsub("\\D+", " ", readLines("Input/day19.txt")), " "), \(x) as.integer(x[-(1:2)]))

cost <- lapply(data19, \(x) t(matrix(c(x[1], 0L, 0L, 0L, x[2], 0L, 0L, 0L,
                                       x[3], x[4], 0L, 0L, x[5], 0L, x[6], 0L), byrow = TRUE, nrow = 4L)))

optimize <- function(mat, rob, minute, cst, lookup = rep(0L, 24)) {
  
  if (minute == 24L) return(mat[4] + rob[4])
  
  if (rob[4] < lookup[minute]) {return(-1L)}
  
  #theoratical best----
  if (mat[4] + sum((rob[4] + 0:(24L - minute)) * (25L - minute):1) <= cur_max) return(-Inf)
  
  #build robot phase------
  nxt <- .Internal(which(colSums(matrix(cst[rob == 0L, ], ncol = 4) == 0L) == sum(rob == 0L)))
  mat_mult <- pmax(apply(ceiling((cst[,nxt] - mat) / rob), 2L, max, na.rm = TRUE), 0L) + 1L
  
  res <- mat[4] + rob[4]
  if (4L %in% nxt & c(mat_mult[nxt == 4L], 0L)[1] == 1L) {
    lookup[minute:24] <- max(lookup)
    res <- optimize(mat + rob - cst[,4], rob + diag(4)[,4], minute + 1L, cst, lookup)
  } else {
    for (k in seq_along(nxt)) {
      if ((rob[nxt[k]] < max(cst[nxt[k],]) | nxt[k] == 4L) & minute + mat_mult[nxt[k]] <= 32) {
        res <- max(res, optimize(mat + rob*mat_mult[k] - cst[,nxt[k]], rob + diag(4)[,nxt[k]], minute + mat_mult[k], cst, lookup))
      }
    }
  }
  if (res  > cur_max) cur_max <<- res
  return(res)
}


#---------
res <- integer(length(data19))
for (k in seq_along(res)) {
  cur_max <- 0L
  res[k] <- optimize(c(0L, 0L, 0L, 0L), c(1L, 0L, 0L, 0L), 1L, cost[[k]])
  
}

sum(res*seq_along(res))


#part2----------
optimize2 <- function(mat, rob, minute, cst, lookup = rep(0L, 32)) {
  
  if (minute == 32L) return(mat[4] + rob[4])
  
  if (rob[4] < lookup[minute]) {return(-1L)}
  
  #theoratical best----
  if (mat[4] + sum((rob[4] + 0:(32L - minute)) * (33L - minute):1) <= cur_max) return(-Inf)
  
  #build robot phase------
  nxt <- .Internal(which(colSums(matrix(cst[rob == 0L, ], ncol = 4) == 0L) == sum(rob == 0L)))
  mat_mult <- pmax(apply(ceiling((cst[,nxt] - mat) / rob), 2L, max, na.rm = TRUE), 0L) + 1L
  
  res <- mat[4] + rob[4]
  if (4L %in% nxt & c(mat_mult[nxt == 4L], 0L)[1] == 1L) {
    lookup[minute:32] <- max(lookup)
    res <- optimize2(mat + rob - cst[,4], rob + diag(4)[,4], minute + 1L, cst, lookup)
  } else {
    for (k in seq_along(nxt)) {
      if ((rob[nxt[k]] < max(cst[nxt[k],]) | nxt[k] == 4L) & minute + mat_mult[nxt[k]] <= 32) {
        res <- max(res, optimize2(mat + rob*mat_mult[k] - cst[,nxt[k]], rob + diag(4)[,nxt[k]], minute + mat_mult[k], cst, lookup))
      }
    }
  }
  if (res  > cur_max) cur_max <<- res
  return(res)
}

res <- integer(3)
for (k in seq_along(res)) {
  cur_max <- 0L
  res[k] <- optimize2(c(0L, 0L, 0L, 0L), c(1L, 0L, 0L, 0L), 1L, cost[[k]])
  
}

prod(res)
