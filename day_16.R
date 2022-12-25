data16 <- readLines("Input/day16.txt")

nm <- gsub("Valve (..).*", "\\1", data16) 
vlv <- setNames(strsplit(gsub(".* valves? ", "", data16), ", "), nm)
flw <- setNames(as.integer(gsub("\\D", "", data16)), nm)[order(nm)]


bfs <- function(.from) {
  queue <- .from
  j <- 1L
  step <- 0L
  
  while (j <= length(queue)) {
    parent <- queue[j]
    new_edge <- setdiff(vlv[[queue[j]]], queue)
    queue <- c(queue, new_edge)
    step <- c(step, rep(step[j] + 1L, length(new_edge)))
    j <- j + 1L
  }
  return(setNames(step + c(0, rep(1, length(queue))[-1]), queue)[order(queue)])
}

bfs_mat <- unname(sapply(nm, bfs)[c("AA", names(flw[flw > 0])), c("AA", names(flw[flw > 0]))])
flw <- unname(flw[flw > 0 | names(flw) == "AA"])


sim_flw <- function(pos, visited = integer(), rem_time, cach = FALSE, cum_prs = 0L) {
  
  nxt <- seq_along(flw)[match(seq_along(flw), c(visited, pos), 0L) == 0L]
  nxt <- nxt[rem_time - bfs_mat[pos, nxt] > 0]
  
  if (length(nxt) == 0L) return(list(cum_prs + flw[pos] * rem_time, c(visited, pos)))
  
  nxt_full <- lapply(nxt, \(x) sim_flw(x, c(visited, pos), rem_time - bfs_mat[pos, x], cach, cum_prs + flw[pos] * rem_time))
  
  if (cach) if (length(visited) > 4) gl_cach <<- c(gl_cach, nxt_full)
  nxt_full[[which.max(sapply(nxt_full, \(x) x[[1]]))]]
}

#part 1------
sim_flw(1L, rem_time = 30L)[[1]] 

#part2-------
gl_cach <- list()
sim_flw(1L, rem_time = 26L, cach = TRUE)

l1 <- unique(gl_cach)[order(sapply(unique(gl_cach), \(x) x[[1]]), decreasing = TRUE)[1:100]]
res <- 0L

for (x in l1) {
  l2 <- l1[sapply(l1, \(y) !anyDuplicated(c(x[[2]][-1], y[[2]])))]
  if (length(l2) > 0) res <- max(res, x[[1]] + sapply(l2, \(y) y[[1]]))
}
res
