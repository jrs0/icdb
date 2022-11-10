### Tests for invalid database connection attempts

test_that("server throws an error when no connection information is provided", {
    expect_error(server())
})

test_that("server throws error when an non-existent config file is used", {
    expect_error(server(config="does_not_exist.yaml"))
})

test_that("server throws error when an invalid config file is used", {
    expect_error(server(config=system.file("extdata", "invalid.yaml",package="icdb")))
})

test_that("server throws error when a config file contains an invalid driver name", {
    expect_error(server(config=system.file("extdata", "bad_driver.yaml",package="icdb")))
})

### Tests for valid connections via config files

test_that("server connects to sqlite database without errors", {

    ## Generate test data (put the result in "gendata/test.db")
    gen_clean_apc("apc.db")
    
    srv <- server(config=system.file("extdata", "sqlite.yaml", package="icdb"))
    expect_true(DBI::dbIsValid(srv@con))
})

### Tests using the artificial clean APC (CDS v6.3) database

test_that("correct columns are returned in artificial APC database", {

    ## Generate test data (put the result in "gendata/test.db")
    gen_clean_apc("apc.db")

    ## Connect to the database
    srv <- server(config=system.file("extdata", "sqlite.yaml", package="icdb"))

    ## Expected column names (the test will check that at least
    ## these are present in the srv table
    nn <- c("NHSNumber", "PrimaryDiagnosis_ICD",
            "HospitalProviderSpellIdentifier", 
            "StartTime_HospitalProviderSpell")
    expect_true(all(nn %in% colnames(srv$APC_SYNTH)))
})

test_that("all NHS numbers in the artificial database are invalid", {

    ## Generate test data (put the result in "gendata/test.db")
    gen_clean_apc("apc.db")

    ## Connect to the database
    srv <- server(config=system.file("extdata", "sqlite.yaml", package="icdb"))

    ## Check validity (expect all invalid)
    num_valid <- srv$APC_SYNTH %>%
        run() %>%
        dplyr::filter(nhs_number_valid(NHSNumber) == TRUE) %>%
        nrow()
    expect_equal(num_valid, 0)
})

### Test raw SQL queries

test_that("check raw SQL string", {

    ## Generate test data (put the result in "gendata/test.db")
    gen_clean_apc("apc.db")

    ## Connect to the database
    srv <- server(config = system.file("extdata", "sqlite.yaml", package="icdb"))

    ## Perform a raw SQL query
    xx <- srv %>% sql_query("SELECT NHSNumber FROM APC_SYNTH
                             WHERE PrimaryDiagnosis_ICD LIKE 'K25%'")
    xx_true <- c("8490675123", "8653479103", "0465972383", "0465972383")
    
    expect_equal(xx$NHSNumber, xx_true)
    
    
})
