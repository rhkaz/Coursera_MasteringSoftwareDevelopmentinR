% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_data.R
\name{eq_file_path}
\alias{eq_file_path}
\title{Get the path to the NOAA dataset}
\usage{
eq_file_path(destdir = NULL, update = FALSE)
}
\arguments{
\item{destdir}{character. A character vector with path to the working directory.
If NULL a persistet folder will be created (default and prefered).}

\item{update}{boolean. Delete the current file and download the dataset again?}
}
\value{
character. Absolute path to the NOAA dataset
}
\description{
Returns the full path to the NOAA dataset for a given directory. If no
path is provided, a persistent path to the file will be created. If the
file do not exists, it will be retrieved from the NOAA website.
}
\examples{
\dontrun{noaa_file_path}
}
