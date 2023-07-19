data21 <- read.table("Input/day21.txt", sep = ":")
data21[,2] <- gsub(" +", "", data21[, 2])
data21[, 3:4] <- t(sapply(strsplit(data21[,2], "[\\+\\*-/]"), \(x) rep_len(x, 2)))

humn <- as.numeric(data21[data21[,1] == "humn", 2])
data21[data21[,1] == "humn", ] <- "humn"

check <- grep("[a-z]{2,}", data21[,2], invert = TRUE)

#first substitute known numbers and evaluate quantities...
while (length(check) > 0) {
  for (k in check) {
    evl <- eval(parse(text = data21[k, 2]))
    idx <- data21[,3] == data21[k, 1] | data21[,4] == data21[k, 1]
    data21[idx, 2] <- gsub(data21[k, 1], evl, data21[idx, 2])
  }
  data21 <- data21[-check, ]
  check <- grep("[a-z]{3,}", data21[,2], invert = TRUE)
}

#...and next substitute unknown quantities to generate one big expression
cur_eq <- data21[data21[, 1] == "root", 2]
tar <- as.numeric(gsub("\\D+", "", cur_eq))
data21[,2] <- ifelse(grepl("[\\+-]", data21[,2]), paste0("(", data21[,2], ")"), data21[,2])
cur_var <- gsub("[^a-z]+", "", cur_eq)
while (cur_var != "humn") {
  idx <- data21[, 1] == cur_var
  cur_eq <- gsub(cur_var, data21[idx, 2], cur_eq)
  cur_var <- gsub("[^a-z]+", "", data21[idx, 2])
}


## part1-------
sprintf("%.f", eval(parse(text = gsub("humn", humn, cur_eq))))

#part2-----
eq2 <- gsub(paste0(".", tar), "", cur_eq)
f <-  \(x) eval(parse(text = gsub("humn", x, eq2))) - tar

##binary search
x <- 1e12 * c(10, 1)
while (TRUE) if (f(mean(x)) != 0) x[(sign(f(mean(x))) + 3) / 2] <- mean(x) else break

sprintf("%.f", mean(x))
