data04 <- as.list(read.fwf("Input/day05.txt", nrows = 8, widths = rep(4, 9)))

crates <- lapply(data04, \(y) gsub("\\[|\\]| ", "", y))
crates <- lapply(crates, \(y) y[y != ""])

mve <- strsplit(gsub("\\D+", " ", readLines("Input/day05.txt")[-(1:10)]), " ")
mve <- t(sapply(mve, \(x) as.integer(x[-1])))

rearrange <- function(crt, .f) {
  
  for (k in seq_along(mve[,1])) {
    crt[[mve[k, 3]]] <- c(.f(crt[[mve[k, 2]]][seq_len(mve[k,1])]), crt[[mve[k, 3]]])
    crt[[mve[k, 2]]] <- crt[[mve[k, 2]]][-seq_len(mve[k,1])]
  }

  paste(sapply(crt, \(x) x[1]), collapse = "")
}

#part1----
rearrange(crates, rev)

#part2----
rearrange(crates, \(x) x)
