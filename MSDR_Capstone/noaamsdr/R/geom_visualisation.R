#' NOAA timeline ggplot theme
#'
#' A simple theme fot the NOAACourser package
#'
#' @importFrom ggplot2 theme element_line element_blank element_rect
#' @export
theme_timeline <- theme(
  legend.position = "bottom",
  axis.line.x = element_line(),
  axis.ticks.x = element_line(),
  panel.background = element_rect(fill = "white"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill = "white")
)
