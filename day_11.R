data11 <- readLines("Input/day11.txt")

item <- lapply(strsplit(gsub(".+: ", "", data11[0:7*7 + 2L]), ", "), as.integer)
op <- lapply(gsub(".+=", "\\\\(old)", data11[0:7*7 + 3L]), \(x) eval(parse(text = x)))
div <- as.integer(gsub("\\D", "", data11[0:7*7 + 4L]))
tru <- as.integer(gsub("\\D", "", data11[0:7*7 + 5L])) + 1L
fal <- as.integer(gsub("\\D", "", data11[0:7*7 + 6L])) + 1L


run_bamboozle <- function(rounds, part1 = TRUE) {
  
  score <- integer(8)
  worry_fun <- if (part1) \(x) as.integer(x / 3) else \(x) x %% prod(div)
  
  for (r in 1:rounds) {
    for (m in 1:8) {
      y <- worry_fun(op[[m]](item[[m]]))
      idx <- y %% div[m] == 0L
      item[[tru[m]]] <- c(item[[tru[m]]], y[idx])
      item[[fal[m]]] <- c(item[[fal[m]]], y[!idx])
      score[m] <- score[m] + length(y)
      item[[m]] <- integer()
    }
  }
  prod(sort(score, decreasing = T)[1:2])
}

#part1------
run_bamboozle(20, TRUE)

#part2---------
sprintf("%.f", run_bamboozle(10000, FALSE))
