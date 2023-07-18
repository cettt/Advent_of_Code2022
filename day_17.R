data17 <- unname(c("<" = -1L, ">" = 1L)[strsplit(readLines("Input/day17.txt"), "")[[1]]])

tiles <- list(
  3:6 + 0i, c(4, 3:5 + 1i, 4 + 2i), c(3:5, 5 + 1:2*1i), 3 + 0:3*1i, c(3:4, 3:4 + 1i)
)

fallen <- 1:7 + 0i
idx <- 0
h_vec <- integer(2021)
rep_vec <- integer(2021)

for (r in 0:2021) {
  
  bot <- max(Im(fallen))
  x <- tiles[[r %% 5 + 1L]] + (bot + 4)*1i
  
  for (k in 1:(bot + 5)) {
    rep_vec[r + 1L] <- idx %% length(data17) + 1
    lr <- data17[idx %% length(data17) + 1]
    idx <- idx + 1L
    fln <- fallen[Im(fallen) >= (bot + 4 - k) & Im(fallen) <= (bot + 8 - k)]
    if (all(Re(x + lr) %in% 1:7)  & !any((x + lr) %in% fln)) x <- x + lr
    if (!any((x - 1i) %in% fln)) x <- x - 1i else break
  }
  
  fallen <- c(fallen, x)
  tbl <- table(Im(fallen[Im(fallen) %in% Im(x)]))
  if (any(tbl == 7L)) fallen <- fallen[Im(fallen) >= as.numeric(names(tbl[tbl == 7]))]
  h_vec[r + 1L] <- max(bot, Im(x))

}

#part1-----
h_vec[2022]

#part2----
reps <- 1000000000000
k2 <- Reduce(intersect, lapply(0:4, \(k) which(duplicated(rep_vec[seq_len(2022) %% 5 == k]))))[1]*5L
k1 <- which(rep_vec == rep_vec[k2])[1]


sprintf("%.f", h_vec[(reps - k1) %% (k2 - k1) + k1] + floor((reps - k1) / (k2 - k1)) * diff(h_vec[c(k1, k2)]))
