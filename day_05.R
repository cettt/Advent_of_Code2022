data05 <- as.list(read.fwf("Input/day05.txt", nrows = 8, widths = rep(4, 9)))

crates <- lapply(data05, \(y) gsub("\\[|\\]| ", "", y))
crates <- lapply(crates, \(y) y[y != ""])

mve <- strsplit(gsub("\\D+", " ", readLines("Input/day05.txt")[-(1:10)]), " ")
mve <- lapply(mve, \(x) as.integer(x[-1]))

rearrange <- function(crt, .f) {
  
  for (m in mve) {
    crt[[m[3]]] <- c(.f(crt[[m[2]]][seq_len(m[1])]), crt[[m[3]]])
    crt[[m[2]]] <- crt[[m[2]]][-seq_len(m[1])]
  }
  
  paste(sapply(crt, \(x) x[1]), collapse = "")
}

#part1----
rearrange(crates, rev)

#part2----
rearrange(crates, \(x) x)
