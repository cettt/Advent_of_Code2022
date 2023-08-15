data20 <- as.integer(readLines("Input/day20.txt"))
n <- length(data20)

run_mix <- function(times = 1L, const = 1L) {
  fwd <- c(seq_along(data20)[-1], 1L)
  bkw <- c(length(data20), seq_along(data20[-1]))
  x <- as.integer((data20 * (const %% (n - 1L)) + n/2) %% (n - 1L) - n/2)
  prev <- 1L
  
  for (.t in seq_len(times)) {
    for (k in seq_along(data20)) {
      if (data20[k] != 0L) {
        res <- k
        if (sign(x[k]) != prev) {
          tmp <- fwd
          fwd <- bkw
          bkw <- tmp
        }
        for (l in seq_len(abs(x[k]))) res <- fwd[res]
        
        fwd[c(bkw[k], k, res)] <- c(fwd[k], fwd[res], k) 
        bkw[c(fwd[k], fwd[bkw[k]], k)] <- c(k, bkw[k], res)
        
        prev <- sign(x[k])
      }
    }
  }
  
  if (prev == -1L) fwd <- bkw
  idx <- which(x == 0L)
  for (l in 1:3) idx <- c(Reduce(\(k, l) fwd[k], 1:1000, init = idx[1]), idx)
  
  return(sum(data20[idx]) * const)
}


#part1-------
run_mix()

#part2---------
sprintf("%.f", run_mix(10L, 811589153))


#explanation---
#the objects fwd (forwards) and bkw (backwards) are linked list
## fwd contains the index of the successor of any position, i.e.
##    fwd[k] = s means that after the number which was at position k in the original list, is the number at position s
## similarly, bkw contains the index of the predecessor of any position, i.e.
##    bkw[l] = t means that before number l there is number t in the list.
## Clearly, fwd can be derived directly from bkw and vice versa.
### One easy way to do that would be bkw <- order(fwd); or fwd <- order(bkw)
###  However this is computationally quite expensive, therefore we modify fwd and bkw separetly.

## The idea of the code is to work with fwd whenever we move a number to the right (x[k] > 0)
###   and to work with bwk whenever we shift a number to the left (x[k] < 0)
### Since every shift forward (i.e to the right, or clockwise) can be expressed as a shift backwards and vice versa,
#### we will work with the shorter of these two directions. 
#### Therefore, in the definition of x we shift by n/2.