# Test fars_summarize_years

expect_that(fars_summarize_years(2013:2014),
            is_a("data.frame"))

expect_that(nrow(fars_summarize_years(2013)),
            equals(12))

expect_that(ncol(fars_summarize_years(2013)),
            equals(2))

expect_that(ncol(fars_summarize_years(2013:2015)),
            equals(4))
