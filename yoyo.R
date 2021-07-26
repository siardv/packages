# measure yo-yo effect (or weight cycling) with BMI from panel data

yoyo <- function(panel, pid, waves, bmi) {
  df <- na.omit(data.frame(panel[, c(pid, waves, bmi)], yoyo = 0))
  pid <- type.convert(names(which(table(df[, 1]) >= 3))) # select participants with minimum 3 measures
  w <- unique(df[, 2])
  if (length(w) <= 3)
    stop("Minimum 3 time points required.")
  w <- expand.grid(w, w, w, stringsAsFactors = FALSE) # all combinations of size 3 for set of waves where w1 < w2 < w3
  w <- subset(w, w[, 1] < w[, 2] & w[, 2] < w[, 3])
  cat("\n")
  for (p in seq_along(pid)) {
    cat("\r Progress :", format(round(p / length(pid) * 100, 2), nsmall = 2), "%") # progress
    sub <- df[df[1] == pid[p], ]
    wp <- w[which(apply(w, 1, function(x) all(x %in% sub[, 2]))), ] # get all valid combinations for participant p
    wp <- data.frame(cbind(wp[, 3], wp), row.names = NULL) # reference wave: highest of 3
    wp[, 2:4] <- sapply(2:4, function(i) sub[, 3][match(wp[, i], unique(wp[, i]))]) # add corresponding BMI
    yy <- which((wp[, 2] > wp[, 3] & wp[, 3] < wp[, 4]) | (wp[, 2] < wp[, 3] & wp[, 3] > wp[, 4])) # detect yo-yo patterns: down-up | up-down
    if (length(yy) > 0){
      wp[yy, 5] <- sapply(seq_along(yy), function(i) mean(abs(diff(as.numeric(wp[i, 2:4]))))) # yo-yo mean per wave
      sapply(unique(wp[, 1]), function(i) df[df[, 1] %in% pid[p] & df[, 2] == i, 4] <<- mean(wp[wp[, 1] == i, 5], na.rm = TRUE)) # add score to main data frame
    }
  }
  return(df)
}

# example
yy <- yoyo(panel = df, pid = 1, waves = 3, bmi = 4)
# panel = data structure containing repeated measures data
# pid = individual-level identifier (column index)
# waves = wave/time variable (column index)
# bmi = measured bmi (column index)
