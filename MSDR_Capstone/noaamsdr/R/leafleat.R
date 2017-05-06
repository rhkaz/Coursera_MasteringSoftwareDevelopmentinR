#' Draws a map to locate the epicenters
#'
#' This function will create a leaflet map with points in the earthquakes
#' epicenters. Also you can pass a \code{annot_col} param to create a popup
#' in each marker
#'
#' @param dataframe data.frame. The filtered dataset containing the points to draw
#' @param annot_col character. A column name to create the popup labels.
#'
#' @return leaflet. A leaflet map
#' @examples
#' library(dplyr)
#' library(lubridate)
#'
#' eq_get_data() %>%
#'   eq_clean_data %>%
#'   filter(COUNTRY == "MEXICO", year(DATE) >= 2000) %>%
#'   eq_map(annot_col = "DATE")
#'
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#' @importFrom magrittr "%>%" extract2
#' @export
eq_map <- function (dataframe, annot_col= NULL) {
  if (is.null(annot_col))
    stop("You must provide a colume to anotate the points")

  annotations = dataframe %>% extract2(annot_col) %>% as.character
  leaflet(data = dataframe) %>%
    addTiles() %>%
    addCircleMarkers(lng = ~ LONGITUDE, lat = ~ LATITUDE, popup = annotations)
}

#' Create annotation HTML text for leaflet
#'
#' This function helps to create a custon HTML template for to annotate the leaflet.
#' The data to show is:
#' - Location
#' - Magnitude
#' - Total number of deaths
#'
#' @param dataframe data.frame. The data to create labels.
#'
#' @return character. A custom HTML template
#' @examples
#' library(lubridate)
#' library(dplyr)
#'
#' eq_get_data() %>%
#'   eq_clean_data %>%
#'   filter(COUNTRY == "MEXICO", year(DATE) >= 2000) %>%
#'   mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = "popup_text")
#'
#' @importFrom stats na.omit
#' @importFrom purrr by_row
#' @importFrom dplyr select_
#' @export
eq_create_label <- function (dataframe) {
  dataframe %>%
    by_row(function (row) {
      text_location = ifelse(
        !is.na(row$LOCATION_NAME),
        sprintf("<b>Location: </b>%s<br>", row$LOCATION_NAME),
        NA
      )
      text_magnitude = ifelse(
        !is.na(row$EQ_PRIMARY),
        sprintf("<b>Magnitude: </b>%s<br>", row$EQ_PRIMARY),
        NA
      )
      text_deaths = ifelse(
        !is.na(row$TOTAL_DEATHS),
        sprintf("<b>Total deaths: </b>%s<br>", row$TOTAL_DEATHS),
        NA
      )
      c(text_location, text_magnitude, text_deaths) %>%
        na.omit() %>%
        paste(collapse = "") %>%
        return
    }, .to = "template") %>%
    select_("template") %>%
    unlist %>%
    return
}
