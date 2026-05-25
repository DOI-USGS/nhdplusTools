# Hex sticker for hydrogeofetch.
# Uses the New Hope sample data shipped with the package.
# Iterate by tweaking colors, line weights, fade band placement, and the
# hexSticker p_*/s_*/h_* args at the bottom.
#
# Source from an R session that has hexSticker, sf, and dplyr installed:
#   source("inst/extdata/hex.R")

source(system.file("extdata", "new_hope_data.R", package = "nhdplusTools"))

zone_outlets <- c(8895440, 8893396, 8894326, 8894344, 8894356, 8897784)
ibm_colors <- c("#FFB000", "#785EF0", "#DC267F", "#FE6100", "#648FFF", "#009D9A")

seen_comids <- integer(0)
zone_catchments <- list()
for (i in seq_along(zone_outlets)) {
  ut <- nhdplusTools::get_UT(new_hope_flowline, zone_outlets[i])
  ut <- setdiff(ut, seen_comids)
  zone_catchments[[i]] <- new_hope_catchment[new_hope_catchment$FEATUREID %in% ut, ]
  seen_comids <- c(seen_comids, ut)
}

pkg_family   <- "mono"
pkg_fontface <- "plain"

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
                           plateau = 0.4, n = 80,
                           hex_clip = NULL) {
  edges <- seq(y_center - half_height, y_center + half_height,
               length.out = n + 1)
  mids  <- (edges[-1] + edges[-length(edges)]) / 2
  t     <- (mids - y_center) / half_height
  alpha <- max_alpha * fade_alpha(t, plateau = plateau)
  rgb_fill <- grDevices::col2rgb(fill) / 255
  cols <- grDevices::rgb(rgb_fill[1], rgb_fill[2], rgb_fill[3], alpha = alpha)

  if (!is.null(hex_clip)) {
    cx <- hex_clip$cx; cy <- hex_clip$cy
    R  <- hex_clip$R;  r  <- hex_clip$r
    dy <- abs(mids - cy)
    hw <- ifelse(dy <= R / 2, r,
                 ifelse(dy <= R, 2 * r * (R - dy) / R, 0))
    x_left  <- cx - hw
    x_right <- cx + hw
  } else {
    x_left  <- rep(xlim[1], length(mids))
    x_right <- rep(xlim[2], length(mids))
  }

  rect(x_left, edges[-length(edges)], x_right, edges[-1],
       col = cols, border = NA)
}

subplot_expr <- expression({
  fl  <- prep_layer(new_hope_flowline)
  big <- prep_layer(dplyr::filter(new_hope_flowline, StreamOrde > 2 & StreamOrde == StreamCalc))
  bb  <- sf::st_bbox(fl)
  pad_x  <- (bb["xmax"] - bb["xmin"]) * 0.25
  pad_yb <- (bb["ymax"] - bb["ymin"]) * 0.10
  pad_yt <- (bb["ymax"] - bb["ymin"]) * 0.25

  old_par <- par(mar = c(0, 0, 0, 0))
  on.exit(par(old_par), add = TRUE)

  plot.new()
  plot.window(xlim = c(bb["xmin"] - pad_x, bb["xmax"] + pad_x),
              ylim = c(bb["ymin"] - pad_yb, bb["ymax"] + pad_yt),
              asp = 1, xaxs = "i", yaxs = "i")

  for (i in seq_along(zone_catchments)) {
    if (nrow(zone_catchments[[i]]) > 0) {
      plot(prep_layer(zone_catchments[[i]]), add = TRUE,
           col = ibm_colors[i], border = NA)
    }
  }

  plot(fl,  add = TRUE, lwd = 0.6, col = "#a8d8ea")
  plot(big, add = TRUE, lwd = 1.2, col = "#1d4f7a")

  # Fade band sits at the top of the hex's vertical sides. In hexSticker
  # coords with s_y=1, s_height=2 the lower flat-side region is at hex y ~0.5,
  # which maps to data y = ymin + span_y * 0.25. Adjust y_center / half_height
  # together with p_y below if the band drifts off the text.
  xlim_span <- (bb["xmax"] + pad_x) - (bb["xmin"] - pad_x)
  ylim_span <- (bb["ymax"] + pad_yt) - (bb["ymin"] - pad_yb)
  S     <- max(xlim_span, ylim_span)
  hx_R  <- S / 2
  hx_r  <- S * sqrt(3) / 4
  hx_cx <- (bb["xmin"] - pad_x + bb["xmax"] + pad_x) / 2
  hx_cy <- (bb["ymin"] - pad_yb + bb["ymax"] + pad_yt) / 2

  draw_fade_band(
    xlim        = c(bb["xmin"] - pad_x, bb["xmax"] + pad_x),
    y_center    = (bb["ymin"] - pad_yb) + ylim_span * 0.78,
    half_height = ylim_span * 0.12,
    fill        = "#fbf6e9",
    max_alpha   = 0.6,
    plateau     = 0,
    hex_clip    = list(cx = hx_cx, cy = hx_cy, R = hx_R, r = hx_r)
  )
})

out <- file.path("hydrogeofetch_hex.png")

hexSticker::sticker(
  subplot    = subplot_expr,
  package    = "hydrogeofetch",
  s_x        = 1,    s_y      = 1,
  s_width    = 2,    s_height = 2,
  p_x        = 1,    p_y      = 1.45,
  p_size     = 15,
  p_color    = "#0e2a44",
  p_family   = pkg_family,
  p_fontface = pkg_fontface,
  h_fill     = "#E8E0DB",
  h_color    = "#0e2a44",
  h_size     = 1.4,
  filename   = out
)

message("Wrote: ", out)
