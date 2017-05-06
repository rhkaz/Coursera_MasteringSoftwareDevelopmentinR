#' Tests if a vector can be safely converted to a numeric type
#'
#' This test will not take into account NA_character_ in the test. Only character
#' values will be tested.
#'
#' @param test_vector character. A vector to test
#'
#' @return Boolean. TRUE if the vector can be safely converted to numeric.
#'
#' @importFrom utils download.file
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_detect str_trim
numeric_convertible <- function (test_vector) {
  test_vector %>%
    str_trim %>%
    replace(. == "", NA) %>%
    str_detect("\\d+(\\.\\d+)?") %>%
    all(na.rm = T) %>%
    return
}

#' Cleans the NOAA dataset
#'
#' A simple wrapper for the main cleaning functions. This function will
#' clean dates, coordintes and location names.
#'
#' @param dataframe A tibble. The raw NOAA dataset
#' @return A tibble
#'
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
#' eq_get_data() %>% eq_clean_data
eq_clean_data <- function (dataframe) {
  dataframe %>%
    eq_clean_wrong_parsed_numeric_vectors %>%
    eq_clean_dates %>%
    eq_clean_coordinates %>%
    eq_clean_location %>%
    return
}

#' Clean magnitude variables
#'
#' Magnitude variables are parsed as character vectors. This functions mutate
#' them into numeric.
#'
#' @param dataframe A tibble. The raw NOAA dataset
#'
#' @return tibble. A data frame with magnitudes cleaned
#' @examples
#' \dontrun{
#'   eq_get_data() %>% eq_clean_worng_parsed_numeric_vectors
#' }
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate_if funs
eq_clean_wrong_parsed_numeric_vectors <- function (dataframe) {
  dataframe %>%
    mutate_if(numeric_convertible, funs(as.numeric)) %>%
    return
}

#' Cleaning NOAA dates
#'
#' The BCE dates where removed from the dataset because R cannot handle
#' this dates
#'
#' @param dataframe A tibble with the NOAA raw dataframe
#'
#' @return tibble. A data frame with the parsed dates and removed BCE dates
#' @examples
#' \dontrun{eq_get_data() %>% eq_clean_dates}
#'
#' @importFrom dplyr filter_ mutate_
#' @importFrom tidyr unite_
#' @importFrom lubridate ymd
eq_clean_dates <- function (dataframe) {
  dataframe %>%
    filter_(~ YEAR > 0) %>%
    mutate_(YEAR = ~ sprintf("%+05d", as.integer(YEAR))) %>%
    mutate_(MONTH = ~ ifelse(
      is.na(MONTH),
      "01",
      sprintf("%02d", as.integer(MONTH))
    )) %>%
    mutate_(DAY = ~ ifelse(is.na(DAY), "01", sprintf("%02d", as.integer(DAY)))) %>%
    unite_("DATE", c("YEAR", "MONTH", "DAY")) %>%
    mutate_(DATE = ~ ymd(DATE)) %>%
    return
}

#' Clean coordinates in the NOAA dataset
#'
#' Coherce coordinates to numeric and removes non valid rows from the
#' NOAA raw dataset
#'
#' @param dataframe A tibble with the NOAA dataset
#' @return A tibble
#' @examples
#' \dontrun{eq_get_data() %>% eq_clean_coordinates}
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter_
eq_clean_coordinates <- function (dataframe) {
  dataframe %>%
    filter_(~ !is.na(LATITUDE), ~ !is.na(LONGITUDE)) %>%
    return
}

#' Clean names for the location in the NOAA dataset
#'
#' This function removes the country name and converts location to titlecase
#' for later usage in labels
#'
#' @param dataframe A tibble. The NOAA raw dataset
#' @return A tibble
#' @examples \dontrun{eq_get_data() %>% eq_clean_location()}
#'
#' @importFrom stringr str_replace str_to_title
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate_
eq_clean_location <- function (dataframe) {
  dataframe %>%
    mutate_(
      LOCATION_NAME = ~ LOCATION_NAME %>%
        str_replace(".*:\\s+(.*)", "\\1") %>%
        str_to_title
    ) %>%
    return
}
