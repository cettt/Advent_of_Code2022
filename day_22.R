data22 <- strsplit(gsub("(L|R)", "\\1 ", tail(readLines("Input/day22.txt"), 1)), " ")[[1]]
cube <- read.fwf("Input/day22.txt", widths = rep(1, 150), strip.white = F, comment.char = ";", n = 200)

co <-  apply(which(cube == ".", arr.ind = TRUE), 1, \(x) x[2] + x[1]*1i)

tm1 <- rbind(
  cbind(150 + 1:50    * 1i, 51 + 1:50    * 1i, 1, 1),
  cbind(100 + 51:100  * 1i, 51 + 51:100  * 1i, 1, 1),
  cbind(100 + 101:150 * 1i,  1 + 101:150 * 1i, 1, 1),
  cbind(50  + 151:200 * 1i,  1 + 151:200 * 1i, 1, 1),
  cbind(51:100  + 1i, 51:100  + 150i, -1i, -1i),
  cbind(101:150 + 1i, 101:150 +  50i, -1i, -1i),
  cbind(1:50  + 101i, 1:50  +   200i, -1i, -1i)
)


walk <- function(tm) {
  
  tm <- tm[tm[,1] %in% co & tm[,2] %in% co,]
  tm <- rbind(tm, cbind(tm[,2:1], tm[, 4:3]*(-1)))
  
  pos <- min(Re(co[Im(co) == min(Im(co))])) + min(Im(co))*1i
  dir <- 1
  
  for (cmd in data22) {
    n_stp <- as.integer(sub("\\D", "", cmd)) 
    while (n_stp > 0) {
      co2 <- if (Re(dir) != 0 ) co[Im(co) == Im(pos)] else co[Re(co) == Re(pos)]
      stp_tkn <- Position(\(x) !x %in% co2, pos + seq_len(n_stp)*dir, nomatch = n_stp + 1L)
      pos <- pos + (stp_tkn - 1L) * dir
      
      new_pos <- tm[tm[, 1] == pos & tm[, 3] == dir, ]
      if (length(new_pos) > 0 & stp_tkn <= n_stp) {
        pos <- new_pos[2]
        dir <- new_pos[4]
        n_stp <- n_stp - stp_tkn
      } else break
    }
    dir <- dir * if (grepl("R", cmd)) 1i else if (grepl("L", cmd)) -1i else 1
  }
  Im(pos)*1000 + Re(pos)*4 + ifelse(dir == 1, 0, ifelse(dir == -1, 2, ifelse(dir == 1i, 1, 3)))
}

#part 1-------
walk(tm1)

# part 2--------
tm2 <- rbind(
  cbind(150 + 1:50    * 1i, 100 + 150:101 * 1i, 1, -1), #from 1 to 5
  cbind(100 + 51:100  * 1i, 101:150 + 50 * 1i, 1, -1i),   #from 3 to 1
  cbind(50  + 151:200 * 1i,  51:100 + 150 * 1i, 1, -1i), #from 4 to 5
  cbind(51:100  + 1i, 1  + 151:200 * 1i, -1i, 1), # from 2 to 4
  cbind(101:150 + 1i, 1:50 +  200i, -1i, -1i), # from 1 to 4
  cbind(1:50  + 101i, 51  +   51:100 * 1i, -1i, 1), # from 6 to 3
  cbind(51  + 1:50 * 1i, 1  +   150:101 * 1i, -1, 1) # from 2 to 6
)

walk(tm2)

#explanation------
#the objects tm1 and tm2 are transportation matrices and show how the edges are connected
##  column one contains the starting coordinates
##  column two contains the target coordinates
##  column three contains the starting direction
##  column four contains the direction after reaching the target
##  for part 2 we enumerate the sides of the cube as follows

###  .21
###  .3.
###  65.
###  4..

## the comments in the definition of tm2 show which side is connected to which side