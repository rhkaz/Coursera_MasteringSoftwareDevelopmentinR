#' Download the NOAA dataset
#'
#' Get the NOAA dataset in TXT file format. If the destination directory
#' don't exist, will be created.
#'
#' @param destdir character. Destination directory
#'
#' @return NULL. Impure function, called by its side effects
#'
#' @examples
#' \dontrun{download_noaa()}
#'
#' @importFrom utils download.file
#' @importFrom magrittr "%>%"
#' @export
eq_download_data = function (destdir) {
  # Constants
  dataset_url <- paste0(
    "https://www.ngdc.noaa.gov/nndc/struts/",
    "results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt"
  )
  destfile <- file.path(destdir, "NOAA.txt", fsep = "\\")

  # First time? Lets fix that
  if (!dir.exists(destdir)) {
    "Creating directory (%s)..." %>% sprintf(destdir) %>% message
    dir.create(destdir, recursive = TRUE)
  }

  # Download file
  "Downloading NOAA dataset to %s ...\n" %>% sprintf(destfile) %>% message
  download.file(dataset_url, destfile, quiet = TRUE)

  return(NULL)
}

#' Get the path to the NOAA dataset
#'
#' Returns the full path to the NOAA dataset for a given directory. If no
#' path is provided, a persistent path to the file will be created. If the
#' file do not exists, it will be retrieved from the NOAA website.
#'
#' @param destdir character. A character vector with path to the working directory.
#'                If NULL a persistet folder will be created (default and prefered).
#' @param update boolean. Delete the current file and download the dataset again?
#'
#' @return character. Absolute path to the NOAA dataset
#'
#' @importFrom magrittr "%>%"
#' @importFrom rappdirs user_data_dir
#' @export
#'
#' @examples
#' \dontrun{noaa_file_path}
eq_file_path <- function (destdir = NULL, update = FALSE) {
  # destdir type check
  if (!is.null(destdir) && !is.character(destdir)) {
    stop("destdir must be either NULL or character")
  }

  ## destdir fallback
  destdir <- if (is.null(destdir)) {
    user_data_dir("NOAA", "Coursera")
  } else {
    destdir
  }
  destfile <- file.path(destdir, "NOAA.txt", fsep = "\\")

  if (!file.exists(destfile) | update) {
    eq_download_data(destdir)
  }

  return(destfile)
}

#' Get NOAA dataframe
#'
#' Securely give you the NOAA dataset. If you do not have the dataset it will
#' download the data in a persistent folder and give you the full data.frame.
#' The column type is character by default and coherent transformations can be
#' performed with the cleaning data functions of this same package.
#'
#' @inheritParams eq_file_path
#' @return tibble. A tibble containing the full NOAA dataset
#'
#' @examples
#' eq_get_data()
#' eq_get_data %>% class
#'
#' @importFrom  readr read_delim cols
#' @export
eq_get_data <- function (destdir = NULL, update = FALSE) {
  eq_file_path(destdir, update) %>%
    read_delim(delim = "\t", col_types = cols(.default = "c")) %>%
    return
}
