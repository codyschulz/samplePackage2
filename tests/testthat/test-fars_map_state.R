# Test fars_map_state

expect_that(fars_map_state(3, 2012), throws_error())

test_that("latlon_adj", {
  state.num = 37
  year = 2015

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

  expect_that(max(data.sub$LATITUDE) > 40, is_true())

  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 40

  expect_that(is.na(max(data.sub$LATITUDE)), is_true())
  expect_that(max(data.sub$LATITUDE, na.rm = TRUE) > 40, is_false())
})
