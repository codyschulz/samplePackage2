# Test fars_read

expect_that(fars_read("joe.csv"), throws_error())

expect_that(fars_read(system.file("extdata", sprintf("accident_%d.csv.bz2", 2013), package = "samplePackage2")),
            is_a("tbl_df"))
