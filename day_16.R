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


sim_flw_bst <- function(vstd, rt) {
  
  nxt <- seq_along(flw)[-vstd][rt - bfs_mat[vstd[1], -vstd] > 0]
  if (length(nxt) == 0L) return(flw[vstd[1]] * rt)
  
  flw[vstd[1]] * rt + max(sapply(nxt, \(x) sim_flw_bst(c(x, vstd), rt - bfs_mat[vstd[1], x])))
}

#part 1------
sim_flw_bst(1L, rt = 30L)


#part2-------
sim_flw_all <- function(vstd, rt, cum_prs) {
  
  nxt <- seq_along(flw)[-vstd][rt - bfs_mat[vstd[1], -vstd] > 0]
  if (length(nxt) == 0L) return(list(cum_prs + flw[vstd[1]] * rt, vstd))
  
  nxt_full <- lapply(nxt, \(x) sim_flw_all(c(x, vstd), rt - bfs_mat[vstd[1], x], cum_prs + flw[vstd[1]] * rt))
  
  if (length(vstd) > 4) gl_cach <<- c(gl_cach, nxt_full)
  nxt_full[[which.max(sapply(nxt_full, \(x) x[[1]]))]]
}


gl_cach <- list()
sim_flw_all(1L, rt = 26L, 0L)

l1 <- unique(gl_cach)[order(sapply(unique(gl_cach), \(x) x[[1]]), decreasing = TRUE)[1:100]]
res <- 0L

for (x in l1) {
  l2 <- l1[sapply(l1, \(y) !anyDuplicated(c(head(x[[2]], -1), y[[2]])))]
  if (length(l2) > 0) res <- max(res, x[[1]] + sapply(l2, \(y) y[[1]]))
}
res

