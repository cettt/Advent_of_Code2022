data08 <- as.matrix(read.fwf("Input/day08.txt", widths = rep(1L, 99)))

#part 1------
f <- \(x) (x > cummax(c(-1L, x[-99]))) | (rev(x[99:1] > cummax(c(-1L, x[99:2])))) 
sum(t(apply(data08, 1, f)) | apply(data08, 2, f))


#part2-------
see <- function(x) { #look to the right
  sapply(seq_along(x), \(k) Position(\(y) y >= x[k], x[-(1:k)], nomatch = 99 - k))
}

max(t(apply(data08, 1, \(x) see(x) * rev(see(rev(x))))) * apply(data08, 2, \(x) see(x) * rev(see(rev(x)))))          

