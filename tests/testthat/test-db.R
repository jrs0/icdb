### Tests for invalid database connection attempts

test_that("Databases throws an error when no connection information is provided", {
    expect_error(Databases())
})

test_that("Databases throws error when an non-existent config file is used", {
    expect_error(Databases(config="does_not_exist.json"))
})

test_that("Databases throws error when an invalid config file is used", {
    expect_error(Databases(config=system.file("extdata", "invalid.json",package="icdb")))
})

test_that("Databases throws error when a config file contains an invalid driver name", {
    expect_error(Databases(config=system.file("extdata", "bad_driver.json",package="icdb")))
})

### Tests for valid connections via config files

test_that("Databases connects to sqlite database without errors", {
    expect_error(Databases(config=system.file("extdata", "sqlite.json",package="icdb")))
})
