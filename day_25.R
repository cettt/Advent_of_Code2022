key <- c("2" = 2L, "1" = 1L, "0" = 0L, "-" = -1L, "=" = -2L) 
data25 <- lapply(strsplit(readLines("Input/day25.txt"), ""), \(x) rev(key[x]))

sm <- sapply(1:max(sapply(data25, length)), \(k) sum(sapply(data25, \(x) x[k]), na.rm = T))

for (k in seq_along(sm)) {
  rk <- (sm[k] + 2L) %% 5L - 2L
  sm[k + 1] <- sm[k + 1] + (sm[k] - rk) %/% 5L
  sm[k] <- rk
}

paste0(c("=", "-", "0", "1", "2")[rev(sm)[-1] + 3L], collapse = "")
