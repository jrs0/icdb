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

test_that("all valid Xnnn.n ICD-10 codes parse", {
    codes <- read.csv(system.file("testdata",
                                  "icd10/icd10_code_list.txt",
                                  package="icdb"))
    raw <- codes %>% unlist()
    parsed <- raw %>% icd10()
    expect_equal(format(parsed), paste("[C]", raw))
})

test_that("a selection of invalid ICD-10 codes throw errors", {
    expect_false(is_valid(icd10("I222"))) ## There is no I22.2
})

test_that("various types of whitespace in ICD-10 codes work", {

    ## Whitespace in codes
    expect_equal(format(icd10("A000  ")), "[C] A00.0")
    expect_equal(format(icd10("  A000")), "[C] A00.0")
    expect_equal(format(icd10(" A000 ")), "[C] A00.0")
    expect_equal(format(icd10("\tA000\n")), "[C] A00.0")

    ## Whitespace only
    expect_false(is_valid(icd10("")))
    expect_false(is_valid(icd10(" ")))
    expect_false(is_valid(icd10("\t")))
    expect_false(is_valid(icd10("  \t\n ")))
})

mock_icd_api_request <- function(token, endpoint, data = list())
{
    if (endpoint == "https://id.who.int/icd/release/10/2016")
    {
        list(title = list(
                 `@language` = "en",
                 `@value` = "Example ICD API return (top level)"
             ),
             child = list(
                 "https://id.who.int/icd/release/10/2016/I", 
                 "https://id.who.int/icd/release/10/2016/V"
             ))
    }
    else if (endpoint == "https://id.who.int/icd/release/10/2016/I")
    {
        list(child = list(
                 "https://id.who.int/icd/release/10/2016/A15-A19"
             ),
             code = "I",
             title = list(
                 `@value` = "Chapter I") 
             )
    }
    else if (endpoint == "https://id.who.int/icd/release/10/2016/A15-A19")
    {
        list(child = list(
                 "https://id.who.int/icd/release/10/2016/A15"
             ),
             code = "A15-A19",
             title = list(
                 `@value` = "Range 1") 
             )
    }
    else if (endpoint == "https://id.who.int/icd/release/10/2016/A15")
    {
        list(child = list(
                 "https://id.who.int/icd/release/10/2016/A15.2"
             ),
             code = "A15",
             title = list(
                 `@value` = "Category 1") 
             )
    }
    else if (endpoint == "https://id.who.int/icd/release/10/2016/A15.2")
    {
        list(code = "A15.2",
             title = list(
                 `@value` = "Code 1") 
             )
    }
    else if (endpoint == "https://id.who.int/icd/release/10/2016/V")
    {
        list(child = list(
                 "https://id.who.int/icd/release/10/2016/F00-F09"
             ),
             code = "V",
             title = list(
                 `@value` = "Chapter V") 
             )
    }
    else if (endpoint == "https://id.who.int/icd/release/10/2016/F00-F09")
    {
        list(child = list(
                 "https://id.who.int/icd/release/10/2016/F01"
             ),
             code = "F00-F09",
             title = list(
                 `@value` = "Range 2") 
             )
    }
    else if (endpoint == "https://id.who.int/icd/release/10/2016/F01")
    {
        list(child = list(
                 "https://id.who.int/icd/release/10/2016/F01.9"
             ),
             code = "F01",
             title = list(
                 `@value` = "Category 2") 
             )
    }
    else if (endpoint == "https://id.who.int/icd/release/10/2016/F01.9")
    {
        list(code = "F01.9",
             title = list(
                 `@value` = "Code 2") 
             )
    }               
}


test_that("various types of whitespace in ICD", {

    ## Get the data and store chapter by chapter in files
    mockthat::with_mock(icd_api_request = mock_icd_api_request,   
                        icd_api_fetch_all("fake_token", dir = "gendata/"))

    ## Combine the files

    ## Check the validity of the result
    
    
})
