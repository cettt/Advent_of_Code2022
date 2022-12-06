data01 <- as.integer(readLines("Input/day01.txt"))
x <- -sort(-aggregate(data01, by = list(cumsum(is.na(data01))), sum, na.rm = T)[,2])[1:3]

#part1-------
max(x)

#part2------
sum(x)
