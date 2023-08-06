data22 <- readLines("Input/day22.txt")
fld <- read.fwf("Input/day22.txt", widths = rep(1, 150), strip.white = F, comment.char = ";", n = 200)
pos_str <- strsplit(gsub("(L|R)", "\\1 ", data22[length(data22)]), " ")[[1]]

co <-  apply(which(fld == ".", arr.ind = TRUE), 1, \(x) x[2] + x[1]*1i)
blk <- apply(which(fld == "#", arr.ind = TRUE), 1, \(x) x[2] + x[1]*1i)


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
  
  for (x in pos_str) {
    n <- as.integer(sub("\\D", "", x)) 
    while (n > 0L) {
      co2 <- if (Re(dir) != 0 ) co[Im(co) == Im(pos)] else co[Re(co) == Re(pos)]
      stp_tkn <- Position(\(x) !x %in% co2, pos + seq_len(n)*dir, nomatch = n + 1)
      pos <- pos + (stp_tkn - 1L) * dir
      
      if (((pos + dir) %in% blk) | (stp_tkn == n + 1L)) break else {
        new_pos <- tm[tm[, 1] == pos & tm[, 3] == dir, ]
        if (length(new_pos) > 0) {
          pos <- new_pos[2]
          dir <- new_pos[4]
          n <- n - stp_tkn
        } else break
      }
    }
    dir <- dir * if (grepl("R", x)) 1i else if (grepl("L", x)) -1i else 1
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
