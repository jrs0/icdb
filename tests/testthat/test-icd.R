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

    ## There is no I22.2
    expect_false(is_valid(icd10("I222")))

    ## This code is an example where the binary search will
    ## fail at the start of the category -- tests that edge
    ## case (previous version of function crashed program)
    expect_false(is_valid(icd10("N180")))
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

test_that("ICD codes are placed in the correct groups", {
    x <- icd10(c("A010", "D021X", "D81.1"), codes_file = system.file("testdata",
                                                                     "icd10/icd10_test.yaml",
                                                                     package="icdb")) 
    expect_equal(group_string(x), c("group1,group2", "group2,group3", ""))
    expect_equal(in_any_group(x), c(TRUE, TRUE, FALSE))
})

test_that("ICD code conversion to character works", {
    x <- icd10(c("A010", "D021X", "D81.1", "Z233", "A227", "I222")) 
    expect_equal(as.character(x), c("A01.0", "D02.1", "D81.1", "Z23.3", "A22.7", NA))    
})
