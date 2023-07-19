data22 <- readLines("Input/day22.txt")
fld <- read.fwf("Input/day22.txt", widths = rep(1, 150), strip.white = F, comment.char = ";", n = 200)
pos_str <- strsplit(gsub("(L|R)", "\\1 ", data22[length(data22)]), " ")[[1]]


co <- apply(which(fld != " ", arr.ind = TRUE), 1, \(x) x[2] + x[1]*1i)
blk <- apply(which(fld == "#", arr.ind = TRUE), 1, \(x) x[2] + x[1]*1i)


pos <- min(Re(co[Im(co) == min(Im(co))])) + min(Im(co))*1i
dir <- 1

for (x in pos_str) {
  n <- as.integer(sub("\\D", "", x))
  for (k in seq_len(n)) {
    nxt_pos <- pos + dir
    if (!nxt_pos %in% co) {
      if (dir == 1)        nxt_pos <- min(Re(co[Im(co) == Im(pos)])) + Im(pos)*1i
      else if (dir == -1)  nxt_pos <- max(Re(co[Im(co) == Im(pos)])) + Im(pos)*1i
      else if (dir == 1i)  nxt_pos <- min(Im(co[Re(co) == Re(pos)]))*1i + Re(pos)
      else if (dir == -1i) nxt_pos <- max(Im(co[Re(co) == Re(pos)]))*1i + Re(pos)
    }
    if (nxt_pos %in% blk) break else pos <- nxt_pos
  }
  
  dir <- dir * if (grepl("R", x)) 1i else if (grepl("L", x)) -1i else 1
}


Im(pos)*1000 + Re(pos)*4 + ifelse(dir == 1, 0, ifelse(dir == -1, 2, ifelse(dir == 1i, 1, 3)))


#part2----

sides <- ifelse(
  Im(co) <= 50 & Re(co) <= 100, 2L, ifelse(
    Im(co) <= 50,  1L, ifelse(
      Im(co) <= 100, 3L, ifelse(
        Im(co) <= 150 & Re(co) <= 50, 6L, ifelse(
          Im(co) <= 150, 5L, 4L
        )
      )
    )
  )
)


pos <- min(Re(co[Im(co) == min(Im(co))])) + min(Im(co))*1i
cur_side <- sides[co == pos]
dir <- 1

for (x in pos_str) {
  n <- as.integer(sub("\\D", "", x))
  
  for (k in seq_len(n)) {
    cur_side <- sides[co == pos]
    # cat (pos, dir, cur_side, "\n")
    nxt_pos <- pos + dir
    new_dir <- dir
    if (!nxt_pos %in% co) {
      
      if (cur_side == 1) {
        if (dir == 1) { #switch to side 5
          new_dir <- -1
          nxt_pos <- 100 + (151 - Im(pos))*1i
        } else if (dir == 1i) { #switch to side 3
          new_dir <- -1
          nxt_pos <- 100 + (Re(pos) - 50)*1i
        } else if (dir == -1i) { #switch to side 4
          new_dir <- -1i
          nxt_pos <- Re(pos) - 100 + 200*1i
        }
      }
      else if (cur_side == 2) {
        if (dir == -1i) { #switch to side 4
          new_dir <- 1
          nxt_pos <- 1 + (100 + Re(pos))*1i
        } else if (dir == -1) { #switch to side 6
          new_dir <- 1
          nxt_pos <- 1 + (151 - Im(pos))*1i
        }
      } else if (cur_side == 3) { 
        if (dir == 1) { #switch to side 1
          new_dir <- -1i
          nxt_pos <- Im(pos) + 50 + 50*1i
        }  else if (dir == -1) { #switch to side 6
          new_dir <- 1i
          nxt_pos <- 101*1i + (Im(pos) - 50)
        } 
      } else if (cur_side == 5) {
        if (dir == 1)  { #switch to 1
          new_dir <- -1
          nxt_pos <- 150 + (151 - Im(pos))*1i
        } else if (dir == 1i)  { #switch to 4
          new_dir <- -1
          nxt_pos <- 50 + (Re(pos) + 100) * 1i
        }
      } else if (cur_side == 6) {
        if (dir == -1)  { #switch to 2
          new_dir <- 1
          nxt_pos <- 51 + (151 - Im(pos))*1i
        } else if (dir == -1i)  { #switch to 3
          new_dir <- 1
          nxt_pos <- 51 + (Re(pos) + 50)*1i
        }
      } else {
        if (dir == 1)  { #switch to 5
          new_dir <- -1i
          nxt_pos <- (Im(pos) - 100) + 150*1i
        } else if (dir == -1) { #switch to 2
          new_dir <- 1i
          nxt_pos <- Im(pos) - 100 + 1i
        } else if (dir == 1i) { #switch to 1
          new_dir <- 1i
          nxt_pos <- Re(pos) + 100 + 1i
        }
      }    
      
    }
    if (nxt_pos %in% blk) break else {
      pos <- nxt_pos
      dir <- new_dir
    }
  }
   
  dir <- dir * if (grepl("R", x)) 1i else if (grepl("L", x)) -1i else 1
}

Im(pos)*1000 + Re(pos)*4 + ifelse(dir == 1, 0, ifelse(dir == -1, 2, ifelse(dir == 1i, 1, 3)))
