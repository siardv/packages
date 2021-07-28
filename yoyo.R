
## Measure yo-yo effect (weight cycling) with panel data

yoyo <- function(panel, pid, waves, bmi, append = TRUE) {
  df <- na.omit(data.frame(panel[, c(pid, waves, bmi)], yoyo = 0))  # select id's that occur in at least 3 waves
  df <- df[order(df[, 2]), ]
  pid <- type.convert(names(which(table(df[, 1]) >= 3)))  # generate all combinations of size 3 for set of waves
  w <- unique(df[, 2])
  if (length(w) < 3) 
    stop("Minimum of 3 time points required.")
  w <- expand.grid(w, w, w, stringsAsFactors = FALSE)
  w <- subset(w, w[, 1] < w[, 2] & w[, 2] < w[, 3])
  cat("\n")
  for (p in seq_along(pid)) {
    cat("\r Progress :", format(round(p/length(pid) * 100, 2), nsmall = 2), "%")  # progress
    sub <- df[df[1] == pid[p], ]  # select valid wave combinations for participant p
    wp <- w[which(apply(w, 1, function(x) all(x %in% sub[, 2]))), ]  # get all wave combinations in which participant p participated
    wp <- data.frame(cbind(wp[, 3], wp), row.names = NULL)  # reference wave: highest of trio
    wp[, 2:4] <- sapply(2:4, function(i) sub[, 3][match(wp[, i], unique(wp[, i]))])  # add corresponding BMI score
    yy <- which((wp[, 2] > wp[, 3] & wp[, 3] < wp[, 4]) | (wp[, 2] < wp[, 3] & wp[, 3] > wp[, 4]))  # detect yo-yo patterns: down-up | up-down
    if (length(yy) > 0) {
      wp[yy, 5] <- sapply(seq_along(yy), function(i) mean(abs(diff(as.numeric(wp[i, 2:4])))))  # yo-yo mean per wave
      sapply(unique(wp[, 1]), function(i) wp[wp[, 1] == i, 5] <<- mean(wp[wp[, 1] == i, 5], na.rm = TRUE))
      wp <- unique(wp[, c(1, 5)])
      wp[, 2][!is.finite(wp[, 2])] <- 0
      df[which(df[, 1] == pid[p] & df[, 2] %in% wp[, 1]), 4] <- wp[, 2]
    }
  }
  if (append == TRUE)
    df <- merge(panel, df, by = names(panel)[c(pid, waves, bmi)])
  return(df)
}

## Arguments         
# panel     Data frame containing repeated measures data
# pid       Participant identifier (column-index)
# waves     Wave/time variable (column-index)
# bmi       Measured BMI (column-index)
# append    Logical, if TRUE (default) a data frame from panel-argument with added yo-yo column is returned; 
#                    if FALSE, a data frame with columns specified in arguments with added yo-yo column is returned.

## Example
yy <- yoyo(panel = df, pid = 1, waves = 3, bmi = 4)
       
