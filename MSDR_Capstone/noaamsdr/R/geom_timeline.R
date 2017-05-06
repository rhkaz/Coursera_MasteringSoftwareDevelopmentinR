#' Draw a timeline
#'
#' @inheritParams ggplot2::geom_point
#' @param na.rm boolean. Not implemented
#'
#' @return layer. Called by its side effects adding a layer to a ggplot.
#' @examples
#' library(magrittr)
#' library(ggplot2)
#'
#' eq_get_data() %>%
#'   eq_clean_data() %>%
#'   dplyr::filter(!is.na(EQ_PRIMARY), !is.na(DEATHS)) %>%
#'   ggplot() +
#'   aes(
#'     x = DATE,
#'     size = EQ_PRIMARY,
#'     colour = DEATHS
#'   ) +
#'   geom_timeline()
#'
#' @importFrom ggplot2 layer ggproto
#' @export
geom_timeline <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  na.rm = FALSE,  show.legend = NA, inherit.aes = TRUE, ...
) {
  layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' Helper function for geom_timeline
#'
#' Internal usage.
#'
#' @param data  Dataframe. A dataframe with at least x, y and group aesthetics
#' @param panel_params List. Contains information about the scales in the current panel
#' @param coord Environment. Coordinate specification
#' @param na.rm boolean. Not implemented
#'
#' @return A grob to draw in the panel
#'
#' @references \url{https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html}
#'
#' @return grob points. A grob object with the desired layer
#' @importFrom grid polylineGrob gpar pointsGrob nullGrob
#' @importFrom ggplot2 .pt .stroke alpha
draw_timeline <- function (data, panel_params, coord, na.rm = FALSE) {
  # if not enough points return a nullGrob
  if (nrow(data) == 0) return(nullGrob())

  # Transform data to panel coordinate system
  coords <- coord$transform(data, panel_params)
  line_coords <- expand.grid(y = unique(coords$y), x = c(0, 1))
  line_coords$id = rep(1:(nrow(line_coords) / 2), 2)

  timeline_points <- pointsGrob(
    x = coords$x,
    y = coords$y,
    pch = coords$shape,
    gp = gpar(
      col = alpha(coords$colour, coords$alpha),
      fill = alpha(coords$fill, coords$alpha),
      fontsize = coords$size * .pt + coords$stroke * .stroke / 2
    )
  )
  timeline_lines <- polylineGrob(
    x = line_coords$x,
    y = line_coords$y,
    id = line_coords$id,
    gp = gpar(col = alpha("gray20", 0.2), lwd = 0.5 * .pt)
  )

  grobTree(timeline_lines, timeline_points)
}

#' ggproto class for geom_timeline
#'
#' Internal usage.
#'
#' @importFrom ggplot2 ggproto aes draw_key_point Geom
GeomTimeline = ggproto(
  "GeomTimeline",
  ggplot2::Geom,
  required_aes = "x",
  non_missing_aes = c("size", "shape", "colour"),
  default_aes = aes(
    y = 0,
    shape = 19, size = 1.5, stroke = 0.5,
    colour = "black", fill = NA, alpha = 0.2
  ),
  draw_panel = draw_timeline,
  draw_key = draw_key_point
)
