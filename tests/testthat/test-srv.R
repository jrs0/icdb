### Tests for invalid database connection attempts

test_that("server throws an error when no connection information is provided", {
    expect_error(server())
})

test_that("server throws error when an non-existent config file is used", {
    expect_error(server(config="does_not_exist.yaml"))
})

test_that("server throws error when an invalid config file is used", {
    expect_error(server(config=system.file("extdata", "invalid.yaml",package="icdb")))
})

test_that("server throws error when a config file contains an invalid driver name", {
    expect_error(server(config=system.file("extdata", "bad_driver.yaml",package="icdb")))
})

### Tests for valid connections via config files

test_that("server connects to sqlite database without errors", {

    ## Generate test data (put the result in "gendata/test.db")
    gen_clean_apc("apc.db")
    
    srv <- server(config=system.file("extdata", "sqlite.yaml", package="icdb"))
    expect_true(DBI::dbIsValid(srv@con))
})

