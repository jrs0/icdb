### Tests for randomly generated NHS numbers (for use in artificial data)

test_that("Random NHS numbers are always invalid", {
    for (n in 1:500)
    {
        expect_equal(nhs_number_valid(random_nhs_number()), FALSE)
    }
})
