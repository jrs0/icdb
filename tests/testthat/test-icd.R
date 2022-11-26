test_that("a selection of ICD-10 codes that have previously failed to parse", {
    ## This worked OK from the beginning
    expect_equal(format(icd10("A000")), "[C] A00.0")

    ## This tests the out-of-order location of
    ## Z near the end of the codes
    expect_equal(format(icd10("Z380")), "[C] Z38.0")

    ## This tests a case where the code (B34.9)
    ## is lexicographically outside the stated
    ## range of its class (B25-B34)
    expect_equal(format(icd10("B349")), "[C] B34.9")

})

test_that("a selection of invalid ICD-10 codes throw errors", {
  expect_false(is_valid(icd10("I222"))) ## There is no I22.2
})
