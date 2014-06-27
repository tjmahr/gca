#' Compute orthogonal times
#'
#' @param df a data-frame
#' @param degree degree of the desired polynomial
#' @param time_col the name of the column containing the time units
#' @return a data-frame with original time values and an ot column for
#'   each polynomial degree
orthogonal_time <- function(df, degree, time_col = "Time") {
  times <- df[[time_col]]
  clean_times <- sort(unique(times))
  time_df <- as.data.frame(poly(clean_times, degree))
  names(time_df) <- paste0("ot", names(time_df))
  time_df[[time_col]] <- clean_times
  time_df
}

#' Compute empirical logit
#'
#' @param x vector containing number of looks to target
#' @param y vector containing number of looks to distractors
#' @return empirical_logit(...) returns the empirical logit of looking to 
#'   target.empirical_logit_weight(...) returns weights for these values.
#' @references Dale Barr's Walkthrough of an "empirical logit" analysis in R 
#'   http://talklab.psy.gla.ac.uk/tvw/elogit-wt.html
empirical_logit <- function(x, y) {
  log((x + 0.5) / (y + 0.5))
}

empirical_logit_weight <- function(x, y) {
  var1 <- 1 / (x + 0.5)
  var2 <- 1 / (y + 0.5)
  var1 + var2
}

#' Inverse logit
inv_logit <- gtools::inv.logit

#' Use normal approximation for p-values in a lmer model
approx_p_values <- function(model) {
  p_values <- data.frame(coef(summary(model)))
  p_values$p <- 2 * (1 - pnorm(abs(p_values$t.value)))
  p_values
}

