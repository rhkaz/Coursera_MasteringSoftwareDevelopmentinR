#'@title Reading FARS csv data files
#'
#'@description  The function will read tablular data, 
#' most notably, comma-separated-value (CSV) files.
#'The function will check if file exisits, if not it will throw error
#'
#'@param filename A character string of the path and the file name. For example
#'"./A/B/C/xyz.csv"
#'
#'@details Converts a FARS csv file into a dataframe
#'
#'@return This function returns a data frame.
#'
#'@importFrom readr read_csv
#'@importFrom dplyr tbl_df
#' 
#'@examples
#'fars_read("accident_2013.csv")


fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#'@title Create canonical filenmae
#'
#'@description  make_filename is used to create a FARS character string filename.
#'
#'@param year A character or integer variable of the year
#'
#'@details The function creates a character string with a filename of the year say 2006
#'in the format of accident_2006.csv.bz2
#'
#'@return This function returns a filename in the form of a character string.
#'
#'@examples
#'make_filename("2016")
#'make_filename(2016)
#'
#'@importFrom dplyr mutate select
#'
#'@import magrittr
#'
#'@export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#'@title Make list of data.frames for each year
#'
#'@description  fars_read_years is used to create a datafrane of selected year from FARS dataset
#'
#'@import dplyr
#'@param years A character or integer vector of years
#'
#'@details The function creates a dataframe of Month and year from the FARS base dataset
#'
#'@return This function returns a dataframe.
#'
#'@examples
#'fars_read_years(2013)
#'fars_read_years(c(2013, 2014, 2015)
#'
#'@export
fars_read_years <- function(years) {
   lapply(years, function(year) {
     file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
   })
}

#'@title Summarize FARS data set by year and each month in year
#'
#'@description  fars_summarize_years is used to create a summary of the number of data points per
#'year and each month in the year.
#'
#'@import dplyr tidyr
#'@param years A character or integer vector of years
#'
#'@details The function creates a summary dataframe of number of data points in the FARS data set
#'summarized per year and each month
#'
#'@return This function returns a dataframe.
#'
#'@import magrittr
#'@importFrom dplyr bind_rows group_by summarise n
#'@importFrom tidyr spread
#'
#'@examples
#'fars_summarize_years(2013)
#'fars_summarize_years(c(2013,2014))
#'
#'@export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#'@title Graphical map of fatalities in FARS data STATE and year
#'
#'@description  fars_map_state is used to create a graphical map of number of fatalities per
#'STATE and year.
#'
#'@import dplyr tidyr maps
#'@param state.num Integer of state
#'@param year A character or integer of year
#'
#'@param state.num number of state
#'@param year specific year
#'
#'@return plot with map or message
#'
#'@importFrom dplyr filter
#'@importFrom maps map
#'@importFrom graphics points
#'
#'@examples
#'fars_map_state(1,2013)
#'
#'@export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
