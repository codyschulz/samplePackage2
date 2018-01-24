#' Read FARS data
#'
#' This function reads in a given file and turns into a table.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @param filename A character string providing the filename for the function
#' to read in.
#'
#' @return A dplyr tbl_df
#'
#' @examples
#' \dontrun{
#' fars_read("~/inputs/fars1.csv")
#' fars_read("~/in/fars2.csv.bz2")
#' }
#'
#' @note the function will thrown an error if the filename provided as input
#' does not exist
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}
#'
#' Make Filename
#'
#' This function takes a year as input and returns a
#' string filename with the format "accident_y.csv.bz2" with y as the
#' input converted to an integer.
#'
#' @param year A numeric vector of length 1
#'
#' @return A character string with format "accident_year.csv.bz2"
#'
#' @examples
#' \dontrun{
#' make_filename(2000)
#' make_filename(2012)
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  system.file("extdata", sprintf("accident_%d.csv.bz2", year), package = "samplePackage2")

}
#'
#' Read Multiple Years of FARS Data
#'
#' This function takes a vector of years as input and returns a
#' list of tables containing the month and year variables from the FARS data
#' tables. The years returned correspond to the years in the input vector.
#'
#' @importFrom dplyr mutate select
#'
#' @param years a numeric vector of length >= 1
#'
#' @return A list of tables
#'
#' @examples
#' \dontrun{
#' fars_read_years(c(2000, 2002))
#' fars_read-years(2013:2015)
#' }
#'
#' @note The function will throw an 'invalid year' error if it encounters any
#' error.
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dat1 <- dplyr::mutate_(dat, year = ~ year)
      dat2 <- dplyr::select_(dat1, .dots = c('MONTH', 'year'))
      dat2
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}
#'
#' Summarize Multiple Years of FARS data
#'
#' This function takes a list of years and returns a table with a count
#' of the number of accidents per month for each year
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @inheritParams fars_read_years
#'
#' @return a table with size 12 x (length(years)) + 1
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2000, 2002))
#' fars_summarize_years(2012:2015)
#' }
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  a <- dplyr::bind_rows(dat_list)
  b <- dplyr::group_by_(a, ~ year, ~ MONTH)
  c <- dplyr::summarize_(b, n = ~ n())
  d <- tidyr::spread_(c, key_col = 'year', value_col = 'n')
  d
}
#'
#' Visualize FARS Accidents by State and Year
#'
#' This function takes year and state.num inputs and plots a map of the
#' selected state with all accidents from that state and year.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @param state.num a numeric vector of length 1
#' @inheritParams make_filename
#'
#' @return a plot object
#'
#' @examples
#' \dontrun{
#' fars_map_state(1, 2013)
#' fars_map_state(30, 2015)
#' }
#'
#' @note This function will throw an error if the state number given is not an
#' actual state number.
#'
#' @note This function will return a NULL object if there are no accidents to
#' plot.
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter_(data, ~ STATE == state.num)
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
