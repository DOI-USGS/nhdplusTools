# Hex sticker for hydrogeofetch.
# Uses the New Hope sample data shipped with the package.
# Iterate by tweaking colors, line weights, fade band placement, and the
# hexSticker p_*/s_*/h_* args at the bottom.
#
# Source from an R session that has hexSticker, sf, and dplyr installed:
#   source("inst/extdata/hex.R")

source(system.file("extdata", "new_hope_data.R", package = "nhdplusTools"))

# Use Univers Condensed Bold if installed — the OS-known family name
# "Univers 67 Condensed" *is* the bold weight, so fontface stays "plain".
# Falls back to "sans" on systems without it. Registering an "Univers" alias
# via systemfonts::register_font did not survive ggplot2/grid metric
# resolution; passing the real family name does.
univers_family <- if (file.exists("C:/WINDOWS/Fonts/Univers-CondensedBold.otf")) {
  "Univers 67 Condensed"
} else {
  "sans"
}

prep_layer <- function(x) sf::st_geometry(sf::st_transform(x, 3857))

# Smoothstep taper. Returns 1 on a central plateau, smooth ramp to 0 at |t|=1.
fade_alpha <- function(t, plateau = 0.4) {
  abs_t <- abs(t)
  ramp  <- pmin(1, pmax(0, (1 - abs_t) / (1 - plateau)))
  ramp * ramp * (3 - 2 * ramp)
}

# Horizontal band whose opacity peaks in the middle and fades to 0 at top/bottom.
# Drawn as N stacked thin rectangles so the gradient works in base graphics.
draw_fade_band <- function(xlim, y_center, half_height,
                           fill = "#fbf6e9", max_alpha = 0.85,
                           plateau = 0.4, n = 80) {
  edges <- seq(y_center - half_height, y_center + half_height,
               length.out = n + 1)
  mids  <- (edges[-1] + edges[-length(edges)]) / 2
  t     <- (mids - y_center) / half_height
  alpha <- max_alpha * fade_alpha(t, plateau = plateau)
  rgb_fill <- grDevices::col2rgb(fill) / 255
  cols <- grDevices::rgb(rgb_fill[1], rgb_fill[2], rgb_fill[3], alpha = alpha)
  rect(xlim[1], edges[-length(edges)], xlim[2], edges[-1],
       col = cols, border = NA)
}

subplot_expr <- expression({
  fl  <- prep_layer(new_hope_flowline)
  big <- prep_layer(dplyr::filter(new_hope_flowline, StreamOrde > 2))
  bb  <- sf::st_bbox(fl)

  old_par <- par(mar = c(0, 0, 0, 0))
  on.exit(par(old_par), add = TRUE)

  plot.new()
  plot.window(xlim = c(bb["xmin"], bb["xmax"]),
              ylim = c(bb["ymin"], bb["ymax"]),
              asp = 1, xaxs = "i", yaxs = "i")

  plot(fl,  add = TRUE, lwd = 0.6, col = "#5aa0c8")
  plot(big, add = TRUE, lwd = 1.6, col = "#1d4f7a")

  # Fade band sits at the bottom of the hex's vertical sides. In hexSticker
  # coords with s_y=1, s_height=2 the lower flat-side region is at hex y ~0.5,
  # which maps to data y = ymin + span_y * 0.25. Adjust y_center / half_height
  # together with p_y below if the band drifts off the text.
  span_y <- bb["ymax"] - bb["ymin"]
  draw_fade_band(
    xlim        = c(bb["xmin"], bb["xmax"]),
    y_center    = bb["ymin"] + span_y * 0.27,
    half_height = span_y * 0.12,
    fill        = "#fbf6e9",
    max_alpha   = 0.85,
    plateau     = 0.4
  )
})

out <- file.path("hydrogeofetch_hex.png")

hexSticker::sticker(
  subplot    = subplot_expr,
  package    = "hydrogeofetch",
  s_x        = 1,    s_y      = 1,
  s_width    = 2,    s_height = 2,
  p_x        = 1,    p_y      = 0.55,
  p_size     = 11,
  p_color    = "#0e2a44",
  p_family   = univers_family,
  p_fontface = "plain",
  h_fill     = "#e8f2f7",
  h_color    = "#0e2a44",
  h_size     = 1.4,
  filename   = out
)

message("Wrote: ", out)
