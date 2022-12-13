data10 <- gsub("\\w{4} ?", "", readLines("Input/day10.txt"))

hlp <- unlist(lapply(data10, \(x) c(0L, if (x != "") as.integer(x))))
x_reg <- cumsum(c(1L, hlp[-1]))

#part1------
sum((x_reg * (seq_along(x_reg) + 1L))[0:5*40L + 19L])

#part2-------
pxl <- ifelse(abs(0:239 %% 40 - c(1L, x_reg[-240])) <= 1L, 1L, 0L)
a <- which(matrix(pxl, ncol = 6)[,6:1] == 1L, arr.ind = TRUE)

plot(a, ylim = c(-8, 10), cex = 2, pch = 15, axes = F, bty = "n")
