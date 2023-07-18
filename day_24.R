data24 <- read.fwf("Input/day24.txt", widths = rep(1, 102), comment.char = "")[-1, -1]
data24 <- unname(as.matrix(data24[-nrow(data24), -ncol(data24)]))
dm <- dim(data24)

rw <- as.integer(row(data24))
cl <- as.integer(col(data24))

lookup_n <- function(k) {
  m <- k %% dm[1]
  k + c(0L, if (m != 0L) 1L, if (m != 1L) -1L, if (k > dm[1]) -dm[1], if (k + dm[1] <= prod(dm)) dm[1])
}

check_blz <- function(k) { # function that returns times at which gr[k] is occupied
  
  ns <- c(
    -rw[k] + .Internal(which(data24[,cl[k]] == "^")),
     rw[k] - .Internal(which(data24[,cl[k]] == "v"))
  )
  ns <- rep.int(ns, 28L) + rep(dm[1]*(0:27), each = length(ns))
  
  ew <- c(
     cl[k] - .Internal(which(data24[rw[k],] == ">")),
    -cl[k] + .Internal(which(data24[rw[k],] == "<"))
  )
  ew <- rep.int(ew, 9L) + rep(dm[2]*(0:8), each = length(ew))
  unique(c(ns, ew))
}

lookup <- sapply(seq_along(data24), lookup_n)
occ    <- sapply(seq_along(data24), check_blz)

go_path <- function(cur, time, goal) {
  
  time <- min(occ[[cur]][occ[[cur]] >= time])

    while (all(cur != goal)) {
    time <- time + 1L
    nxt <- unique(unlist(lookup[cur]))
    cur <- nxt[sapply(occ[nxt], \(k) all(k != time))]
    }
  occ <<- lapply(occ, \(x) x[x > time])
  return(time + 1L)
  
}

#part1---------
p1 <- go_path(1L, 1L, length(data24))
p1

#part2------
go_path(1L, go_path(length(data24), p1, 1L), length(data24))
