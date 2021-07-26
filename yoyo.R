
yoyo <- function(panel, pid, waves, bmi) {
  df <- data.frame(panel[, c(pid, waves, bmi)])
  df <- na.omit(df)
  df <- df[with(df, ave(df[, 1], df[, 1], FUN = length)) > 2, ]
  df[, ncol(df) + 1] <- as.numeric(NA)
  colnames(df)[ncol(df)] <- "yoyo"
  id <- unique(unlist(df[, 1]))
  # generate all combinations of size 3 for set of waves
  w <- c(unique(unlist(df[, 2])))
  if (length(w) < 3) {
    stop(paste("Minimum of 3 time points required."))
  }
  w <- expand.grid(w, w, w, stringsAsFactors = FALSE)
  w <- subset(w, (w[, 1] < w[, 2]) & (w[, 2] < w[, 3]))
  cat("\n")
  for (p in 1:length(id)) {
    cat("\r Progress :",format(round(p/length(id) * 100, 2), nsmall = 2), "%")  # progress
    # select valid combinations per id
    sub <- data.frame(df[df[, 1] == id[p], ])
    wp <- w
    for (c in 1:ncol(wp)) {
      x <- which(is.element(wp[, c], as.numeric(sub[, 2])) == FALSE)
      if (length(x) > 0) {
        wp <- wp[-x, ]
      }
      rownames(wp) <- NULL
      # add corresponding BMI scores
      wp[, 4] <- wp[, 3]
      for (r in 1:nrow(sub)) {
        j <- which(sub[r, 2] == wp[, c])
        wp[j, c] <- sub[r, 3]
      }
    }
    # detect yo-yo patterns
    du <- (wp[, 1] > wp[, 2] & wp[, 2] < wp[, 3])  # down-up pattern
    ud <- (wp[, 1] < wp[, 2] & wp[, 2] > wp[, 3])  # up-down patter
    yy <- c(which(du), which(ud))
    # measure the difference in BMI for the particular patterns observed
    if (length(yy) > 0) {
      for (y in 1:length(yy)) {
        wp[yy[y], 5] <- mean(abs(diff(as.numeric(wp[yy[y], 1:3]))))
      }
      # take the mean of patterns per wave
      for (l in 1:length(unique(wp[, 4]))) {
        x <- which(wp[, 4] == unique(wp[, 4])[l])
        wp[x[l], 6] <- round(mean(wp[x, 5], na.rm = TRUE), 3)
        if (is.na(wp[x[i], 6])) {
          wp[x[l], 6] <- 0
        }
      }
      # add mean per wave in last column of source data
      if (l == length(unique(wp[, 4]))) {
        yoyo <- na.omit(wp[, c(4, 6)])
        names(yoyo) <- c(names(sub[2]), "yoyo")
        m <- base::merge(sub[1:3], yoyo, by = names(sub[2]), all = TRUE)
        panel[rownames(sub), ncol(panel)] <- m[, 4]
      }
    }
  }
  return(panel)
}

new_df <- yoyo(panel = df, pid = 1, waves = 3, bmi = 4)
# panel = object with panel data (object)
# pid = individual-level identifier (column index)
# waves = wave variable (column index)
# bmi = measured bmi  (column index)

# NA indicates that the yo-yo score could not be measured because the minimum requirement of 3 valid BMI scores was not met.

