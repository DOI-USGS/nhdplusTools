prep_layer <- function(x) st_geometry(st_transform(x, 3857))

hexSticker::sticker(expression({
  plot(prep_layer(basin), lwd = 2)
  plot(prep_layer(flowline), lwd = 1, col = "blue", add = TRUE)
  plot(prep_layer(dplyr::filter(flowline, streamorde > 2)),
       lwd = 2, col = "darkblue", add = TRUE)
}), package = "nhdplusTools",
s_width = 2, s_height = 2, s_y = .5, p_size = 6,
p_color = "#000000", h_fill = "#057519", h_color = "#000000")
