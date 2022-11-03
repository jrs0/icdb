### Tests for invalid database connection attempts

test_that("Server throws an error when no connection information is provided", {
    expect_error(Server())
})

test_that("Server throws error when an non-existent config file is used", {
    expect_error(Server(config="does_not_exist.json"))
})

test_that("Server throws error when an invalid config file is used", {
    expect_error(Server(config=system.file("extdata", "invalid.json",package="icdb")))
})

test_that("Server throws error when a config file contains an invalid driver name", {
    expect_error(Server(config=system.file("extdata", "bad_driver.json",package="icdb")))
})

### Tests for valid connections via config files

test_that("Server connects to sqlite database without errors", {
    srv <- Server(config=system.file("extdata", "sqlite.json", package="icdb"))
    expect_true(DBI::dbIsValid(srv@con))
})
