get_breaks_pp <- function(x) {
  
  lower <- min(x)
  upper <- max(x)
  
  if (lower < -50) {
    negative <- c(lower, -50, -30, -15)
  } else if (lower >= -50 & lower < -30) {
    negative <- c(lower, -30, -15)
  } else if (lower >= -30 & lower < -15) {
    negative <- c(lower, -15)
  } else if (lower >= -15 & lower < 0) {
    negative <- lower
  }
  
  if (upper >= 300) {
    positive <- c(0, 15, 30, 50, 100, 300, upper)
  } else if (upper < 300 & upper >= 100) {
    positive <- c(0, 15, 30, 50, 100, upper)
  } else if (upper < 100 & upper >= 50) {
    positive <- c(0, 15, 30, 50, upper)
  } else if (upper < 50 & upper >= 30) {
    positive <- c(0, 15, 30, upper)
  } else if (upper < 30 & upper >= 15) {
    positive <- c(0, 15, upper)
  } else if (upper < 15 & upper >= 0) {
    positive <- c(0, upper)
  }
  
  c(negative, positive)
  
}
