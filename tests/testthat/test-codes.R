### Check that parsing a codes definition file works

test_that("parsing a codes definition file works", {

    ## Load the codes file
    codes <- yaml::read_yaml(system.file("testdata", "test_codes.yaml",
                                         package = "icdb"))

    ## The correct result of this parse is as follows. You can execute
    ## this command and manually inspect the printed "correct" variable
    ## to see that the result corresponds to the definition file.
    correct_codes <-
        list(`acs.stemi.myocardial-infarction.anterior-transmural<something>.` = "I210", `acs.stemi.myocardial-infarction.inferior-transmural<another><yet-another>.` = "I211", 
             `acs.stemi.myocardial-infarction.other-transmural.` = "I212", 
             `acs.stemi.myocardial-infarction.unspecified-transmural.` = "I213", 
             `acs.stemi.myocardial-infarction.subsequent<catch-all-tags>.anterior-transmural.` = "I220", 
             `acs.stemi.myocardial-infarction.subsequent<catch-all-tags>.inferior-transmural.` = "I221", 
             `acs.stemi.myocardial-infarction.subsequent<catch-all-tags>.other-site.` = "I228", 
             `acs.nstemi.myocardial-infarction.subendocardial.` = "I214", 
             `acs.nstemi.myocardial-infarction.unspecified.` = c("I219", 
                                                                 "I21.9"), `acs.nstemi.myocardial-infarction.subsequent.generic.` = "I222", 
             `acs.nstemi.myocardial-infarction.subsequent.other-site.` = "I229", 
             `acs.nstemi.non-myocardial-infarction.coronary-thrombosis.` = "I240", 
             `acs.nstemi.non-myocardial-infarction.other.` = "I248", `acs.nstemi.non-myocardial-infarction.unspecified.` = "I249", 
             `acs.unstable-angina.` = "I200")
    
    ## Parse the file
    parsed_codes <- parse_codes(codes)
    
    ## Check that the cache is still empty
    expect_equal(parsed_codes, correct_codes)
})

## Check that parsing a directory of codes works

test_that("parsing a codes definition file works", {

    ## Load the codes file
    codes <- get_codes(system.file("testdata", "codes_dir", package = "icdb"))

    ## The correct result of this parse is as follows. You can execute
    ## this command and manually inspect the printed "correct" variable
    ## to see that the result corresponds to the definition file.
    ##
    ## The important feature of this parse is that the Catch All codes
    ## for foo come after the more specific codes .bar and .some
    correct_codes <- list(foo.bar. = c("B", "BB"),
                          foo.some. = "S",
                          foo. = c("Catch", "All"),
                          next. = "N")

    ## Check the parse is correct
    expect_equal(codes, correct_codes)
})

### Testing the code map

test_that("isolated test of the code map", {

    ## Example data for input to code map
    data <- list(foo = c("b", "c"),
                 bar = c("d"),
                 some = c("e"))
    map <- gen_code_map(data)

    ## The correct result flattens and reverses the
    ## list so that values in data point to keys in
    ## data.
    correct_map <- c(b = "foo",
                     c = "foo",
                     d = "bar",
                     e = "some")
    
    ## Check the parse is correct
    expect_equal(map, correct_map)
})

