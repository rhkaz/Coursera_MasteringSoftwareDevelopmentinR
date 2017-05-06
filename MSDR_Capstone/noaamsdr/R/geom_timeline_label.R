#' Adds an annotation to the geom_timeline layer
#'
#' You must provide the "by" aesthetics in order to filter the dataset with this
#' variable.
#'
#' @inheritParams ggplot2::geom_text
#' @param n_max numeric. Positive integer. The maximum number of labels to draw
#' by group
#'
#' @return layer. A ggplot2 layer that draws the labels for a geom_timeline layer
#' @examples
#' library(ggplot2)
#' library(magrittr)
#'
#' eq_get_data() %>%
#'   eq_clean_data() %>%
#'   ggplot() +
#'   aes(
#'     x = DATE,
#'     label = LOCATION_NAME,
#'     by = EQ_PRIMARY
#'   ) +
#'   geom_timeline_label()
#'
#' @importFrom ggplot2 layer
#' @export
geom_timeline_label <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ..., show.legend = NA, inherit.aes = TRUE,
  n_max = 5, check_overlap = FALSE, na.rm = FALSE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatTimelineLabel,
    geom = GeomTimelineLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      check_overlap = check_overlap,
      n_max = n_max,
      na.rm = na.rm,
      ...
    )
  )
}

#' Helper function for geom_timeline_label base class
#'
#' Internal usage.
#'
#' @param data  data.frame. A dataframe with at least x, y and group aesthetics
#' @param panel_params list. Contains information about the scales in the current panel
#' @param coord environment. Coordinate specification
#' @param check_overlap boolean. Omit overlaped text.
#'
#' @return grobTree. The text labels and corresponding guide lines.
#'
#' @importFrom grid textGrob polylineGrob grobTree
draw_timeline_label <- function (data, panel_params, coord, check_overlap) {
  coords <- coord$transform(data, panel_params)
  basis <- unique(coords$y)
  occurrences <- unique(coords$x)
  line_coords = expand.grid(x = occurrences, y = c(basis, basis + 0.1))
  line_coords$id = rep(1:(nrow(line_coords) / 2), 2)

  text <- textGrob(
    label = coords$label,
    x = coords$x,
    y = coords$y + 0.1,
    rot = 45,
    just = c("left", "center"),
    gp = gpar(
      col = "black",
      fontsize = 3.25 * .pt
    ),
    check.overlap = check_overlap
  )
  lines <- polylineGrob(
    x = line_coords$x,
    y = line_coords$y,
    id = line_coords$id,
    gp = gpar(
      col = alpha("gray20", 0.25),
      lwd = 0.5 * .pt
    )
  )

  grobTree(lines, text)
}

#' Base class for geom_timeline_label
GeomTimelineLabel <- ggproto(
  "GeomTimelineLabel",
  ggplot2::Geom,
  required_aes = c("x", "label", "by"),
  default_aes = aes(y = 0, angle = 45),
  draw_group = draw_timeline_label
)

#' Base class for stat_timelinelabel
#'
#' Internal usage. This class computes the labels to be drawn in the
#' geom_timeline_label.
#' @importFrom dplyr slice arrange
StatTimelineLabel <- ggproto(
  "StatTimelineLabel",
  ggplot2::Stat,
  compute_group = function (data, scales, params, n_max = 5) {
    if (n_max < 0) stop("n_max param must be a positive integer")

    data %>%
      arrange(desc(by)) %>%
      slice(1:n_max) %>%
      return
  }
)
