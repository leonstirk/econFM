#' fmPlot
#'
#' @param dat econFM output object
#' @param g character, group variable name
#' @param t integer
#'
#' @return plot
#' @export
#'
#' @examples
fmPlot <- function(dat, g, t) {
  plot(dat[[g]][[t]]$didi$mi, dat[[g]][[t]]$didi$Ni, log = "y", col = "grey", main = "All hazards Frequency-Total Damages",
       ylab = "Cumulative disaster frequency",
       xlab =  expression("Log"[10]*"(Inflation-adjusted damages)"))
  graphics::points(dplyr::filter(dat[[g]][[t]]$didi, mi == as.character(dat[[g]][[t]]$mc))$mi, dplyr::filter(dat[[g]][[t]]$didi, mi == as.character(dat[[g]][[t]]$mc))$Ni, pch = 6)
  graphics::points(dat[[g]][[t]]$didi$mi, dat[[g]][[t]]$didi$ni)
  graphics::abline(v = c(dat[[g]][[t]]$mc), col = c("brown"), lty = c("dotdash"))
  graphics::abline(dat[[g]][[t]]$b_cum, col = "brown", lty = 'dotdash')
}

#' fmAdd
#'
#' @param dat econFM output object
#' @param g character, group variable name
#' @param t integer
#'
#' @return plot
#' @export
#'
#' @examples
fmAdd <- function(dat, g, t) {
  graphics::points(dat[[g]][[t]]$didi$mi, dat[[g]][[t]]$didi$Ni, col = 3)
  graphics::points(dat[[g]][[t]]$didi$mi, dat[[g]][[t]]$didi$ni, col = 4)
  graphics::points(dplyr::filter(dat[[g]][[t]]$didi, mi == as.character(dat[[g]][[t]]$mc))$mi, dplyr::filter(dat[[g]][[t]]$didi, mi == as.character(dat[[g]][[t]]$mc))$Ni, pch = 6, col = 3)
  graphics::abline(v = c(dat[[g]][[t]]$mc), col = 3, lty = "solid")
  graphics::abline(dat[[g]][[t]]$b_cum, col = 3, lty = 'solid')
}
