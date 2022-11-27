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

test_that("the format of invalid codes is correct (including trailing matter)", {
    expect_equal(format(icd10("I222")), "[X] (I222)")
    expect_equal(format(icd10("abcde")), "[X] (abcde)")
    expect_equal(format(icd10("  ")), "[E] ()")
    expect_equal(format(icd10("")), "[E] ()")
    expect_equal(format(icd10("A0000")), "[T] A00.0(0)")
    expect_equal(format(icd10("A000abc")), "[T] A00.0(abc)")
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

## A mock function used for the next test
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


test_that("the ICD-10 code file generation process works", {

    ## Get the data and store chapter by chapter in files
    ## Use a mock for the api (defined by mock_icd_api_request above)
    ## to simulate the data returned by the real API.
    mockthat::with_mock(icd_api_request = mock_icd_api_request,   
                        icd_api_fetch_all("fake_token", dir = "gendata/"))

    ## Combine the files
    icd_combine_files("gendata/")

    ## Read and check the file
    res <- yaml::read_yaml("gendata/icd10.yaml")

    correct <- list(
        list(
            category = "ICD-10",
            docs = "ICD-10 codes, 2016 release", 
            child = list(
                list(
                    category = "I",
                    docs = "Chapter I",
                    child = list(
                        list(
                            category = "A15-A19",
                            docs = "Range 1",
                            child = list(
                                list(
                                    category = "A15",
                                    docs = "Category 1",
                                    child = list(
                                        list(code = "A15.2",
                                             docs = "Code 1"))))))), 
                list(
                    category = "V",
                    docs = "Chapter V",
                    child = list(
                        list(
                            category = "F00-F09",
                            docs = "Range 2",
                            child = list(
                                list(category = "F01",
                                     docs = "Category 2",
                                     child = list(
                                         list(
                                             code = "F01.9",
                                             docs = "Code 2"))))))))))

    ## Compare the correct structure with the generated file
    expect_equal(res, correct)
    
})

