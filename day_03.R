data03 <- readLines("Input/day03.txt")

sc <- setNames(1:52, c(letters, LETTERS))
x <- strsplit(data03, "")

#part1-------
sum(sapply(x, \(y) sc[intersect(y[seq_len(length(y) / 2)], y[-seq_len(length(y) / 2)])]))


###part2---------
gr <- ceiling(seq_along(data03) / 3)
sum(aggregate(data03, by = list(gr), \(x) sc[Reduce(intersect, strsplit(x, ""))])[,2])
