data23 <- read.fwf("Input/day23.txt", widths = rep(1, 70), comment.char = ";")
elf_m <- matrix(0L, 210, 210)
elf_m[71:140, 71:140] <- c("#" = 1L, "." = 0L)[as.matrix(data23)]

move_vec <- c(-1L, 1L, -210L, 210L) #North, South, East, West
check_vec <- c(-211L, -1L, 209L, -209L, 1L, 211L, -211L + 0:2, 209L + 0:2)

lookup <- matrix(rep(seq_along(elf_m), each = 12), nrow = 12) + check_vec

for (r in 1:1e3) {
  
  elf_int <- .Internal(which(elf_m == 1L))
  elf_n <- matrix(elf_m[as.integer(lookup[, elf_int])], nrow = 12)
  elf_n2 <- cbind(elf_int, sapply(0:3, \(k) colSums(elf_n[k * 3L + 1:3, ])))
  
  if (sum(elf_n2[, -1L]) == 0L) break
  elf_n2 <- elf_n2[rowSums(elf_n2[, -1L]) != 0L,]
  
  for (l in ((r + 0:3 - 1L) %% 4L + 1L)) {
    
    idx_l <- elf_n2[, l + 1L] == 0L
    new_pos <- elf_n2[idx_l, 1L] + move_vec[l]
    old_pos <- new_pos + move_vec[l]
    
    elf_m[old_pos] <- ifelse(elf_m[new_pos] == 1L, 1L, elf_m[old_pos])
    elf_m[elf_n2[idx_l, 1L]] <- ifelse(elf_m[new_pos] == 0L, 0L, 1L)
    elf_m[new_pos] <- 1L - elf_m[new_pos]
    
    elf_n2 <- matrix(elf_n2[!idx_l, ], ncol = 5)
  }
  
  if (r == 10L) part1 <- elf_m
}

#part 1-------
prod(apply(which(part1 == 1L, arr.ind = TRUE), 2, \(x) diff(range(x)) + 1L)) - length(elf_int)

#part2-------
r
