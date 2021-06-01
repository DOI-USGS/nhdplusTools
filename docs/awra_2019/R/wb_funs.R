get_wb <- function(catchment, flowData, sseb_data, gridmet_data) {
  sec_per_day <- 24 * 60^2
  q <- dplyr::filter(flowData, Date >= as.Date("2009-10-01") & Date <= as.Date("2010-10-01"))
  q <- dplyr::select(q, t = Date, x = X_00060_00003) 
  q$x <- units::set_units(q$x * sec_per_day, "ft^3")
  q$cumsum <- cumsum(q$x)
  
  e <- sseb_data$intersection
  
  area_df <- data.frame(ID = as.integer(names(e)[2:ncol(e)]))
  area_df <- dplyr::left_join(area_df, sf::st_set_geometry(catchment, NULL), by = "ID")
  
  e$sum <- rowSums(e[2:ncol(e)] * area_df$a, na.rm = TRUE) / sum(area_df$a)
  e <- dplyr::select(e, t = time_stamp, x = sum)
  e <- units::set_units(approx(as.Date(e$t), cumsum(e$x), q$t)$y, "mm")
  e <- data.frame(t = q$t, cumsum = e)
  
  p <- gridmet_data$intersection
  p$sum <- units::set_units(rowSums(p[2:ncol(p)] * area_df$a, na.rm = TRUE), "mm") / sum(area_df$a)
  p <- dplyr::select(p, t = time_stamp, x = sum)
  p$cumsum <- cumsum(p$x)
  
  wb_summary <- data.frame(t = q$t, q = q$cumsum, e = e$cumsum, p = p$cumsum)
  a <- units::set_units(sf::st_area(sf::st_geometry(readd(boundary))), "ft^2")
  wb_summary$q <- wb_summary$q / a
  wb_summary$x <- wb_summary$p - wb_summary$q - wb_summary$e
  return(wb_summary)
}

get_plot_data <- function(x) {
  plot_data <- colSums(x[2:ncol(x)], na.rm = TRUE)
  data.frame(ID = as.integer(names(plot_data)), sum = as.numeric(plot_data), 
             stringsAsFactors = FALSE)
}