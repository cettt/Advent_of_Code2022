data13 <- readLines("Input/day13.txt")
data13 <- data13[data13 != ""]

split_el <- function(z) {
  z1 <- strsplit(gsub("^\\[(.*)\\]$", "\\1", z), "")[[1]]
  z2 <- cumsum(z1 == "[") - cumsum(c("", head(z1, -1)) == "]")
  z1[z1 == "," & z2 == 0] <- ":"
  strsplit(paste(z1, collapse = ""), ":")[[1]]
}


compare <- function(x, y) {
  
  x1 <- split_el(x)
  y1 <- split_el(y)
  
  if (max(length(x1), length(y1)) > 0) {
    for (k in 1:max(length(x1), length(y1))) {
      if (length(x1) < k) return(TRUE)
      if (length(y1) < k) return(FALSE)
      if (!grepl("^\\[", x1[k]) & !grepl("^\\[", y1[k])) {
        if (as.integer(x1[k]) < as.integer(y1[k])) return(TRUE)
        if (as.integer(x1[k]) > as.integer(y1[k])) return(FALSE)
      } else if (grepl("^\\[", x1[k]) & grepl("^\\[", y1[k])) {
        com <- compare(x1[k], y1[k])
        if (!is.na(com)) return(com)
      } else if (grepl("^\\[", x1[k])) {
        com <- compare(x1[k], paste0("[", y1[k], "]"))
        if (!is.na(com)) return(com)
      } else if (grepl("^\\[", y1[k])) {
        com <- compare(paste0("[", x1[k], "]"), y1[k])
        if (!is.na(com)) return(com)
      }
    }
  }
  return(NA)
}

#part1-----
sum(which(sapply(1:150, \(k) compare(data13[k*2 - 1L], data13[k*2]))))

#part2---------
prod(sapply(c("[[2]]", "[[6]]"), \(y) sum(sapply(data13, compare, y = y))) + 1:2)
