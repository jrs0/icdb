### Tests for randomly generated NHS numbers (for use in artificial data)

## TODO: find a better way of implementing this to test more numbers.
## The random_nhs_number function is very slow currently
test_that("Random NHS numbers are always invalid", {
    for (n in 1:50)
    {
        expect_false(nhs_number_valid(random_nhs_number()))
    }
})
