#' Title
#'
#' @param dat data.frame tibble
#' @param y character
#' @param y_metric numeric
#' @param cpi_adjust logical
#' @param time_var character
#' @param time_split 'median', 'half', 'none', or numeric quantile e.g. 0.2
#' @param time_split_cumulative logical
#' @param group_var character
#' @param group character vector
#' @param log_base 10 or 'natural'
#' @param mc_method 'gft', 'mode', or 'mbass'
#' @param mbin numeric
#'
#' @return list
#' @export
#'
#' @examples
## output <- econFM(dis, "total_damages", group = c("Geophysical", "!Geophysical"),
##                  time_split = 'median', time_split_cumulative = F, mc_method = 'gft')
## output <- econFM(dis, "total_affected", cpi_adjust = F, group_var = "disaster_type",
##                  group = c("Flood", "Storm"), mc_method = 'mode')
econFM <- function(dat, y, y_metric = 1000, cpi_adjust = TRUE, time_var = "date", time_split = "none", time_split_cumulative = F, group_var = "disaster_subgroup", group = "", log_base = 10, mc_method = 'gft', mbin = 0.1) {
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

  ## disaster groups
  l_dat_group <- lapply(group, function(g) {
    if(stringr::str_length(g) > 0) {
      if(stringr::str_detect(g, "!")) {
        g <- dat[which(dat[group_var] != stringr::str_remove_all(g, "!")),]
      } else {
        g <- dat[which(dat[group_var] == g),]
      }
    } else {
      g <- dat
    }
  })
  names(l_dat_group) <- group

  l_dat_time <- lapply(l_dat_group, function(dat) {
    ## time_split
    if(class(time_split) == 'numeric') {
      tmp <- dat[[time_var]] < as.Date(stats::quantile(as.numeric(dat[[time_var]]),probs = c(time_split)), origin = zoo::as.Date(0))
      if(time_split_cumulative) {
        list(dat[tmp,], dat)
      } else {
        list(dat[tmp,], dat[!tmp,])
      }
    }
    else if(time_split == 'median') {
      tmp <- dat[[time_var]] < as.Date(stats::quantile(as.numeric(dat[[time_var]]),probs = c(0.5)), origin = zoo::as.Date(0))
      if(time_split_cumulative) {
        list(dat[tmp,], dat)
      } else {
        list(dat[tmp,], dat[!tmp,])
      }
    } else if(time_split == 'half') {
      tmp <- dat[time_var] < max(dat[time_var]) - diff(range(dat[time_var]))/2
      if(time_split_cumulative) {
        list(dat[tmp,], dat)
      } else {
        list(dat[tmp,], dat[!tmp,])
      }
    } else if(time_split == 'random') {
      tmp <- stats::rbinom(nrow(dat), 1, 0.5) > 0
      if(time_split_cumulative) {
        list(dat[tmp,], dat)
      } else {
        list(dat[tmp,], dat[!tmp,])
      }
    } else if(time_split == 'none') {
      list(dat)
    }
  })

  l_didi <- lapply(l_dat_time, function(g) {
    lapply(g, function(i) {
      didi <- rseismNet::fmd(i[[log_y]], mbin = mbin)
      mc <- rseismNet::mc.val(i[[log_y]], mc_method, mbin = mbin)
      filt <- dplyr::filter(didi, mi > mc)
      filt <- filt[which(filt$Ni > 0),]
      b_cum <- if(log_base == 10){
        stats::lm(log10(Ni) ~ mi, data = filt)
      } else if (log_base == 'natural') {
        stats::lm(log(Ni) ~ mi, data = filt)
      }

      list('didi' = didi,
           'mc' = mc,
           'b' = rseismNet::beta.mle(i[[log_y]], mc),
           'b_cum' = b_cum,
           'date_range' = range(i[[time_var]]))
    })

  })
  return(l_didi)
}


