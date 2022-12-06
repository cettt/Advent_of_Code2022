data02 <- apply(read.table("Input/day02.txt"), 1, paste, collapse = "")

f <- \(x) sum(setNames(x, c("AX", "AY", "AZ", "BX", "BY", "BZ", "CX", "CY", "CZ"))[data02])
#part1------
f(c(4L, 8L, 3L, 1L, 5L, 9L, 7L, 2L, 6L))

#part2----------
f(c(3L, 4L, 8L, 1L, 5L, 9L, 2L, 6L, 7L))
