test_that("A selection of ICD-10 codes that have previously failed to parse", {
  expect_equal(format(icd10("A000")), "A00.0")
})
