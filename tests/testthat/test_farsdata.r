
setwd("C:/Users/marco/OneDrive/061 Coursera/spec_MasteringSoftwareDevInR/03_BuildingRPackages/w2_assignment/data")


# -------------------------------------------------------------------
context('Errors')
test_that('Throws errors', {
        throws_error(fars_read_years(years = 2000))
        throws_error(fars_summarize_years(years = 2000))
        throws_error(make_filename(year = 'two thousand thirteen'))

        library(mapdata)
        throws_error(fars_map_state(3, 2014))
        throws_error(fars_map_state(36, 2000))
})


# -------------------------------------------------------------------
context('File load and summary are correct')
test_that('Loads multiple years correctly', {
        dfs <- fars_read_years(2013:2015)
        expect_that(dfs, is_a('list'))
        expect_that(dfs[[1]], is_a('tbl_df'))
        expect_equal(length(dfs), 3)
})

test_that('Reads one file correctly', {
        fn <- make_filename(2013)
        df <- fars_read(fn)
        expect_that(df, is_a('tbl_df'))
        expect_that(nrow(df), is_more_than(0))
})

