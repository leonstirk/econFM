# ##library(rvest)
# ##library(lubridate)
# ##library(zoo)
#
# dis <- read.csv('../../earthquakes/emdat_allnaturaldisasters_1900_2021.csv') %>% as_tibble()
# names(dis) <- names(dis) %>% tolower() %>% str_replace_all('\\.','_') %>% str_replace_all('___000_us__', '')
#
# ## set null start days to 1
# dis[which(is.na(dis$start_day)),'start_day'] <- 1
# ## set null start months to 1
# dis[which(is.na(dis$start_month)), 'start_month'] <- 1
# ## correction for afghanistan flash flood 1992
# dis[3693,c("start_day", "end_day")] <- 3
# ## gen date variable
# dis$date <- dis %>% select(start_year, start_month, start_day) %>% unite("date", start_year, start_month, start_day) %>% .$date %>% as.Date("%Y_%m_%d")
#
# ## fill in recent cpi values with value 100
# dis[which(is.na(dis$cpi) & dis$start_year > 2019),'cpi'] <- 100
#
# ## only post-1965
# dis <- dis[which(dis$date > as.Date('1964-01-01')),]
#
# #############################################################
#
# ## tmp <- something(dis, "total_damages")
# ## tmp <- something(dis, "total_damages", group = c("Geophysical", "!Geophysical"))
# ## tmp <- something(dis, "total_affected", y_metric = 1, cpi_adjust = F, group = c("!Geophysical", "Geophysical"))
#
# par(mfrow = c(2,2))
#
# tmp <- econFM(dis, "total_damages", group = c("Geophysical", "!Geophysical"), time_split = 'median', time_split_cumulative = F, mc_method = 'mode')
# fmPlot(tmp,1,2)
# fmAdd(tmp,1,1)
# fmPlot(tmp,2,2)
# fmAdd(tmp,2,1)
#
# as.Date(unlist(lapply(tmp, function(i) { unlist(lapply(i, function(x) { x[['date_range']]}))})), origin = as.Date(0))
# lapply(tmp, function(i) { unlist(lapply(i, function(x) { x[['mc']]}))})
# lapply(tmp, function(i) { unlist(lapply(i, function(x) { x[['b_cum']]$coefficients[2]}))})
#
# tmp <- econFM(dis, "total_damages", group = c("Geophysical", "!Geophysical"), time_split = 'median', time_split_cumulative = F, mc_method = 'gft')
# fmPlot(tmp,1,2)
# fmAdd(tmp,1,1)
# fmPlot(tmp,2,2)
# fmAdd(tmp,2,1)
#
# as.Date(unlist(lapply(tmp, function(i) { unlist(lapply(i, function(x) { x[['date_range']]}))})), origin = as.Date(0))
# lapply(tmp, function(i) { unlist(lapply(i, function(x) { x[['mc']]}))})
# lapply(tmp, function(i) { unlist(lapply(i, function(x) { x[['b_cum']]$coefficients[2]}))})
#
#
# ## text(x=filter(didi, mi == as.character(mc_gft))$mi, y=filter(didi, mi == as.character(mc_gft))$Ni, labels=c(expression("M"["c"])), pos=2, col="blue")
#
#
#
#
#
#
