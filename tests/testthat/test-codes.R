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
