data12 <- as.character(unlist(read.fwf("Input/day12.txt", rep(1, 162))))

map_k <- function(k) {
  m <- k %% 41
  k + c(if (k <= 6642L - 41L) 41 , if (k > 41) -41, if (m != 1L) -1L, if (m != 0L) 1L)
}

gr <- unname(setNames(c(1:26, 1, 26), c(letters, "S", "E"))[data12])
lookup <- lapply(seq_along(gr), map_k)

find_way <- function(tar) {
  q <- collections::priority_queue(which(data12 == "E"), priorities = 0L)
  dist <- c(rep.int(10000L, length(gr)))
  dist[which(data12 == "E")] <- 0L
  
  while (q$size() > 0) {
    cur <- q$pop()
    if (any(cur == tar)) return(dist[cur])
    cur_dist <- dist[cur]
    for (ne in lookup[[cur]][gr[lookup[[cur]]] + 1L >= gr[cur]]) {
      if (dist[ne] > cur_dist + 1L) {
        dist[ne] <- cur_dist + 1L
        q$push(ne, priority = -cur_dist - 1L)
      }
    }
  }
}

#part1----
find_way(which(data12 == "S"))  

#part2-----
find_way(which(gr == 1))  
