#' Title
#'
#' @param dat tibble
#' @param y character, variable of interest name
#' @param y_metric, integer, multiplier (e.g. 1000)
#' @param log_base 'natural' or 10
#' @param cpi_adjust logical
#' @param time_var character, time variable name
#' @param l label object (e.g. labs(title = 'hello'))
#'
#' @return ggplot
#' @export
#'
#' @examples
loessTM <- function(dat, y, y_metric = 1000, log_base = 10, cpi_adjust = TRUE, time_var = "date", l = labs(title = 'Pls add labels')) {
  ## input y complete cases
  dat <- dat[dat[y] %>% stats::complete.cases() %>% which(),]
  ## convert to units using y_metric
  dat[y] <- dat[y] * y_metric
  ## if cpi_adjust is TRUE adjust for inflation
  if(cpi_adjust) {
    ## make a new variable name
    real_y <- paste('real_', y, sep = '')
    ## inflation adjusted total damages
    dat[real_y] <- format(dat[[y]]/(dat$cpi/100), scientific = F, digits = 1) %>% as.numeric()
    ## relevant variable name changes to real_(y) if cpi_adjust is TRUE
    y <- real_y
  }

  ## Log the y variable (default = base 10)
  ## make a new variable name
  log_y <- paste('log_', y, sep = '')
  if(log_base == 'natural') {
    dat[log_y] <- log(dat[y])
  }
  if(log_base == '10') {
    ## log10 inflation adjusted total damages
    dat[log_y] <- log10(dat[y])
  }

  ggplot2::ggplot(dat, ggplot2::aes(x = dat[[time_var]], y = dat[[log_y]])) +
    ggplot2::geom_point(alpha = 1/10) +
    ggplot2::geom_smooth(method = 'loess', size = 0.5, color = 2) +
    ggplot2::theme_minimal() + l
}

#' fmPlot
#'
#' @param dat econFM output object
#' @param g character, group variable name
#' @param t integer
#' @param main character
#' @param ylab character
#' @param xlab character
#'
#' @return plot
#' @export
#'
#' @examples
fmPlot <- function(dat, g, t, main, ylab, xlab) {
  plot(dat[[g]][[t]]$didi$mi, dat[[g]][[t]]$didi$Ni, log = "y", col = 1, main = main,
       ylab = ylab,
       xlab = xlab)
  graphics::points(dplyr::filter(dat[[g]][[t]]$didi, mi == as.character(dat[[g]][[t]]$mc))$mi, dplyr::filter(dat[[g]][[t]]$didi, mi == as.character(dat[[g]][[t]]$mc))$Ni, pch = 25, col = 1, bg = 1)

  graphics::points(dat[[g]][[t]]$didi$mi, dat[[g]][[t]]$didi$ni, col = 'darkgray')

  graphics::abline(v = c(dat[[g]][[t]]$mc), col = 1, lty = c("dotdash"))
  graphics::abline(dat[[g]][[t]]$b_cum, col = 1, lty = 'solid')
  ## graphics::text(x=filter(dat[[g]][[t]]$didi, mi == as.character(dat[[g]][[t]]$mc))$mi, y=filter(dat[[g]][[t]]$didi, mi == as.character(dat[[g]][[t]]$mc))$Ni, labels=c(expression("M"["c"])), pos=2, col=2)
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
  graphics::points(dat[[g]][[t]]$didi$mi, dat[[g]][[t]]$didi$Ni, col = 'red')
  graphics::points(dplyr::filter(dat[[g]][[t]]$didi, mi == as.character(dat[[g]][[t]]$mc))$mi, dplyr::filter(dat[[g]][[t]]$didi, mi == as.character(dat[[g]][[t]]$mc))$Ni, pch = 25, col = 'red', bg = 'red')

  graphics::points(dat[[g]][[t]]$didi$mi, dat[[g]][[t]]$didi$ni, col = 2)

  graphics::abline(v = c(dat[[g]][[t]]$mc), col = 'red', lty = "dotdash")
  graphics::abline(dat[[g]][[t]]$b_cum, col = 'red', lty = 'solid')
}
