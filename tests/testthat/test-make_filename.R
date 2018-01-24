# Test make_filename

test_that("year and character", {
  year <- as.integer(2012)
  expect_that(year,
              is_a("integer"))
  expect_that(system.file("extdata", sprintf("accident_%d.csv.bz2", year), package = "samplePackage2"),
              is_a("character"))
})
