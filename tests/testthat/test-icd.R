test_that("a selection of ICD-10 codes that have previously failed to parse", {
  expect_equal(format(icd10("A000")), "A00.0")
  expect_equal(format(icd10("Z380")), "Z38.0")

})

test_that("a selection of invalid ICD-10 codes throw errors", {
  expect_error(icd10("I222")) ## There is no I22.2
})
