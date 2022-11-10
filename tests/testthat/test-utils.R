test_that("Random NHS numbers are always invalid", {
    for (n in 1:1000)
    {
        expect_equal(nhs_number_valid(random_nhs_number()), FALSE)
    }
})
