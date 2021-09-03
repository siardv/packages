
## Measure yo-yo effect (weight cycling) with panel data
yoyo <- function(panel, id, waves, bmi, append = FALSE) {
  df <- data.frame(panel[, c(id, waves, bmi)], yoyo = 0)
  df <- df[order(df[, 2]), ]
  ids <- type.convert(names(which(table(df[which(!is.na(df[, 3])), 1]) >= 3)))   # select id's with non-missing BMI in 3 or more waves
  n <- unique(df[, 2])
  if (length(n) < 3) 
    stop("Minimum of 3 waves required.")
  w <- data.frame(t(utils::combn(n, 3))) # get all partially ordered 3-element subsets from the set of n-waves where w1 < w2 < w3
  for (i in seq_along(ids)) {
      cat("\r Progress: ", format(round(i/length(ids) * 100, 2), nsmall = 2), "%\t", sep = "")
      sub <- df[df[1] == ids[i], ]
      if (any(table(sub[2]) > 1))
        warn <- 1
      wp <- w[which(apply(w, 1, function(x) all(x %in% sub[, 2]))), ]   # subsets for i
      wp <- data.frame(cbind(wp[, 3], wp))   # add outcome/reference wave: (1, 3, 4) -> (4, 1, 3, 4)
      wp[, 2:4] <- sapply(2:4, function(x) sub[, 3][match(wp[, x], unique(wp[, x]))]) # add BMI: (4, 1, 3, 4) -> (4, 21.2, 22.8, 20.9)
      yy <- which((wp[, 2] > wp[, 3] & wp[, 3] < wp[, 4]) | (wp[, 2] < wp[, 3] & wp[, 3] > wp[, 4]))   # detect yo-yo patterns: down-up | up-down
      if (length(yy) > 0) {
        wp[yy, 5] <- sapply(yy, function(x) mean(abs(diff(as.numeric(wp[x, 2:4])))))   # yo-yo mean per reference wave
        sapply(unique(wp[, 1]), function(x) wp[wp[, 1] == x, 5] <<- ifelse(all(is.na(wp[wp[, 1] == x, 5])), 0, mean(wp[wp[, 1] == x, 5], na.rm = TRUE)))
        wp <- unique(wp[, c(1, 5)])
        apply(wp, 1, function(x) df[which(df[, 2] == x[1] & df[, 1] == ids[i]), 4] <<- x[2])
      }
  }
  if (append == TRUE)
    df <- merge(panel, df, by = names(panel)[c(id, waves, bmi)])
  if(exists("warn"))
    cat("\n Some ids have multiple measures in the same wave: only first values are used.\n")
  return(df)
}

## Arguments         
# panel     Data frame containing repeated measures data
# id        Individual-level identifier
# waves     Wave/time variable (positive integer)
# bmi       Measured BMI
# append    Logical, if FALSE (default) a four column data frame is returned containing the columns specified in the arguments plus a column containing the yo-yo score;
#                    if TRUE, the original data frame plus a column containing the yo-yo score is returned.

## Example
yoyo_score <- yoyo(panel = df, id = 1, waves = 2, bmi = 3)
