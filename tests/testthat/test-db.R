test_that("Databases throws an error when no connection information is provided", {
  expect_error(Databases())
})

test_that("Databases throws error when an non-existent config file is used", {
  expect_error(Databases(config="does_not_exist.json"))
})

test_that("Databases throws error when an invalid config file is used", {
  expect_error(Databases(config=system.file("extdata", "invalid.json",package="icdb")))
})
