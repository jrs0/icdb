### Tests using the mapped for the APC CDS v6.3 database

test_that("correct columns are returned in mapped APC", {

    ## Generate test data (put the result in "gendata/test.db")
    gen_clean_apc("apc.db")

    ## Connect to the database
    msrv <- mapped_server(
        config = system.file("extdata", "sqlite.yaml", package="icdb"),
        mapping = system.file("extdata", "clean_apc.yaml", package="icdb"))

    ## Expected column names (the test will check that at least
    ## these are present in the srv table
    nn <- c("nhs_number", "primary_diagnosis_icd",
            "hospital_provider_spell_identifier", 
            "start_time_hospital_provider_spell")
    expect_true(all(nn %in% colnames(msrv$apc)))
})

## test_that("all NHS numbers in the artificial database are invalid", {

##     ## Generate test data (put the result in "gendata/test.db")
##     gen_clean_apc("apc.db")

##     ## Connect to the database
##     srv <- server(config=system.file("extdata", "sqlite.yaml", package="icdb"))

##     ## Check validity (expect all invalid)
##     num_valid <- srv$APC_SYNTH %>%
##         run() %>%
##         dplyr::filter(nhs_number_valid(NHSNumber) == TRUE) %>%
##         nrow()
##     expect_equal(num_valid, 0)
## })

