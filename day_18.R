data18 <- unname(t(as.matrix(read.table("Input/day18.txt", sep = ","))))

#part1-----
sum(apply(data18, 2, \(x) 6L - sum(colSums(abs(data18 - x)) == 1L)))

#part2------
dm <- apply(data18, 1, \(x) max(x) - min(x)) + 3L
m <- dm[1]
n <- dm[1] * dm[2]

obj_idx <- apply(data18 - apply(data18, 1, min) + 1L, 2L, \(x) sum(x * c(1L, m, n))) + 1L

lookup_n <- function(k) {
  k + c(if (k %% m != 1L) -1L, if (k %% m != 0L) 1L,
        if (k %% n > m) -m, if (k %% n <= n - m) m,
        if (k > n) -n, if (k <= prod(dm) - n) n
  )
}

lookup <- lapply(seq_len(prod(dm)), lookup_n)
outside <- which(sapply(lookup, \(x) length(x) < 6))
cur <- outside

while (length(cur) > 0) {
  nxt <- unique(unlist(lookup[cur]))
  cur <- nxt[match(nxt, c(outside, obj_idx), 0L) == 0L]
  outside <- c(outside, cur)
}
sum(unlist(lookup[obj_idx]) %in% outside)

#explanation:
## we imagine that we put the object (obj) in a big cube that completely contains it.
### the dimensions of that cube are given by dm
###  we then fill the cube with water; on a technical level we are doing a bfs from outside
###   of the object.
### the only tricky part is to determine the indices of the cubes which belong to the object:
###  therefore, we just interpret the big cube as three dimensional array and 
###  shift the object such that bottom left corner of the cube is at 0,0,0.
###    This can be achieved by shifting the object such that min(x) = 1, min(y) = 1 and min(z) = 1