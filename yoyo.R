
## Measure yo-yo effect (weight cycling) with panel data

yoyo <- function(panel, id, waves, bmi, append = FALSE) {
  df <- data.frame(panel[, c(id, waves, bmi)], yoyo = 0)
  df <- df[order(df[, 2]), ]
  pid <- type.convert(names(which(table(df[which(!is.na(df[, 3])), 1]) >= 3)))   # select id's with non-missing BMI in 3 or more waves
  n <- unique(df[, 2])
  if (length(w) < 3) 
    stop("Minimum of 3 time points required.")
  w <- expand.grid(n, n, n, stringsAsFactors = FALSE)   # generate all partially ordered 3-element subsets from the set of n-waves
  w <- subset(w, w[, 1] < w[, 2] & w[, 2] < w[, 3])     # select subsets where w1 < w2 < w3
  for (p in seq_along(pid)) {
    cat("\r Progress :", format(round(p/length(pid) * 100, 2), nsmall = 2), "%\t")
    sub <- df[df[1] == pid[p], ]   # valid wave combinations for participant p
    wp <- w[which(apply(w, 1, function(x) all(x %in% sub[, 2]))), ]
    wp <- data.frame(cbind(wp[, 3], wp), row.names = NULL)   # add reference wave: highest of 3-element subset (1, 3, 4) -> (4, 1, 3, 4)
    wp[, 2:4] <- sapply(2:4, function(i) sub[, 3][match(wp[, i], unique(wp[, i]))]) # add corresponding BMI score (4, 21, 22, 21)
    yy <- which((wp[, 2] > wp[, 3] & wp[, 3] < wp[, 4]) | (wp[, 2] < wp[, 3] & wp[, 3] > wp[, 4]))   # detect yo-yo patterns: down-up | up-down
    if (length(yy) > 0) {
      wp[yy, 5] <- sapply(yy, function(i) mean(abs(diff(as.numeric(wp[i, 2:4])))))   # yo-yo mean per wave
      sapply(unique(wp[, 1]), function(i) wp[wp[, 1] == i, 5] <<- mean(wp[wp[, 1] == i, 5], na.rm = TRUE))
      wp <- unique(wp[, c(1, 5)])
      wp[, 2][!is.finite(wp[, 2])] <- 0
      df[which(df[, 1] == pid[p] & df[, 2] %in% wp[, 1]), 4] <- wp[, 2]
    }
  }
  if (append == TRUE)
    df <- merge(panel, df, by = names(panel)[c(id, waves, bmi)])
  return(df)
}

## Arguments         
# panel     Data frame containing repeated measures data
# id        Individual-level identifier (column-index)
# waves     Wave/time variable (column-index)
# bmi       Measured BMI (column-index)
# append    Logical, if FALSE (default) a data frame with columns specified in arguments with added yo-yo column is returned;
#                    if TRUE, a original data frame from panel-argument with added yo-yo column is returned.

## Example
yy <- yoyo(panel = df, id = 1, waves = 2, bmi = 3)
