#' Tests if a vector can be safely converted to a numeric type
#'
#' This test will not take into account NA_character_ in the test. Only character
#' values will be tested.
#'
#' @param test_vector character. A vector to test
#'
#' @return Boolean. TRUE if the vector can be safely converted to numeric.
#'
#' @examples
#' c("1.5", "1.2", "2.0") %>% numeric_convertible # Returns TRUE
#' c("a", "b", "c") %>% numeric_convertible # Returns FALSE
#'
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
#' @param dataframe
#'
#' @return tibble. A data frame with magnitudes cleaned
#' @examples
#' \dontrun{
#'   eq_get_data() %>% eq_clean_worng_parsed_numeric_vectors
#' }
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate_if starts_with funs
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
#' @param dataframe. A tibble with the NOAA raw dataframe
#' @return tibble. A data frame with the parsed dates and removed BCE dates
#' @examples
#' \dontrun{eq_get_data() %>% eq_clean_dates}
#'
#' @importFrom dplyr filter mutate
#' @importFrom tidyr unite
#' @importFrom lubridate ymd
eq_clean_dates <- function (dataframe) {
  dataframe %>%
    filter(YEAR > 0) %>%
    mutate(YEAR = sprintf("%+05d", as.integer(YEAR))) %>%
    mutate(MONTH = ifelse(is.na(MONTH), "01", sprintf("%02d", as.integer(MONTH)))) %>%
    mutate(DAY = ifelse(is.na(DAY), "01", sprintf("%02d", as.integer(DAY)))) %>%
    unite(date, YEAR, MONTH, DAY) %>%
    mutate(date = ymd(date)) %>%
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
#' @importFrom dplyr mutate filter
eq_clean_coordinates <- function (dataframe) {
  dataframe %>%
    filter(!is.na(LATITUDE), !is.na(LONGITUDE)) %>%
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
#' @importFrom dplyr mutate
#' @importFrom magrittr "%>%"
eq_clean_location <- function (dataframe) {
  dataframe %>%
    mutate(
      LOCATION_NAME = LOCATION_NAME %>%
        str_replace(".*:\\s+(.*)", "\\1") %>%
        str_to_title
    ) %>%
    return
}
