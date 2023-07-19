data23 <- read.fwf("Input/day23.txt", widths = rep(1, 70), comment.char = ";")
elf <- apply(which(data23 == "#", arr.ind = TRUE), 1, \(x) x[2] - 1i*x[1]) 
m_list <- list(1i + c(0, -1, 1), -1i + c(0, -1, 1), -1 + c(0, 1i, -1i), 1 + c(0, 1i, -1i)) 


for (r in 1:1e4) {
  new_pos <- elf
  run <- FALSE
  for (i in seq_along(elf)) {
    e2 <- elf[seq.int(max(1L, i - 60L), min(length(elf), i + 60L))]
    e2 <- e2[abs(elf[i] - e2) < 2]
    if (length(e2) > 1)  {
      run <- TRUE
      for (k in (r + 0:3 - 1L) %% 4 + 1L) {
        if (!any(e2 %in% (elf[i] + m_list[[k]]))) {
          new_pos[i] <- elf[i] + m_list[[k]][1]
          break
        }
      }
    }
  }
  elf <- ifelse(new_pos %in% new_pos[duplicated(new_pos)], elf, new_pos)
  elf <- elf[.Internal(radixsort(TRUE, c(FALSE, FALSE), FALSE, TRUE, Re(elf), -Im(elf)))]
  if (r == 10) part1 <- (diff(range(Re(elf))) + 1) * (diff(range(Im(elf))) + 1) - length(elf)
  if (!run) break
}

#part 1-------
part1
#part2-------
r
