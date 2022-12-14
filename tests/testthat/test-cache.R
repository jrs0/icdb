### Check that the cache is disabled by default

test_that("cache is disabled by default", {

    ## Generate artificial data
    gen_clean_apc("apc.db")
    
    ## Connect to the database
    srv <- server(config=system.file("extdata", "sqlite.yaml", package="icdb"))

    ## Get the current size of the cache
    size <- nrow(show_cache())
    
    ## Do a query that would get cached if the cache were enabled
    srv$APC_SYNTH %>% run()

    ## Check that the cache is still empty
    expect_equal(nrow(show_cache()), size)
})

### Test that run() caches queries and check hits

test_that("repeat query increments hits", {

    ## Generate artificial data
    gen_clean_apc("apc.db")

    ## Connect to the database
    srv <- server(config=system.file("extdata", "sqlite.yaml", package="icdb"))

    ## Enable the cache
    use_cache(TRUE)

    ## Clear the cache (in case cache/ is present)
    clear_cache()
    
    ## Check that the cache is empty
    expect_equal(nrow(show_cache()), 0)
    
    ## Do a query that will be cached
    srv$APC_SYNTH %>% run()

    ## Check the cache contains one item with one hit
    tbl <- show_cache()
    expect_equal(tbl %>% nrow(), 1)
    expect_equal(tbl$hits[[1]], 1)

    ## Check that repeated queries increment hits
    for (n in 2:4)
    {
        srv$APC_SYNTH %>% run()
        tbl <- show_cache()
        expect_equal(tbl %>% nrow(), 1)
        expect_equal(tbl$hits[[1]], n)
    }    
})

### Test that the level 2 cache is functional

test_that("queries are flushed to the level 2 cache", {

    ## Generate artificial data
    gen_clean_apc("apc.db")

    ## Connect to the database
    srv <- server(config=system.file("extdata", "sqlite.yaml", package="icdb"))

    ## Enable the cache, manually set a level 1 size
    use_cache(TRUE, size = 3)

    ## Clear the cache (in case cache/ is present)
    clear_cache()
    
    ## Check that the cache is empty
    expect_equal(nrow(show_cache()), 0)
    
    ## Do three queries (this will exactly fill the level 1 cache)
    for (n in 1:3)
    {
        srv$APC_SYNTH %>% head(n) %>% run()
    }

    ## Check that the cache is size 3, and all objects are
    ## in the level 1 cache
    tbl1 <- show_cache()
    expect_equal(tbl1 %>% nrow(), 3)
    expect_equal(tbl1 %>% dplyr::filter(!in_memory) %>% nrow(), 0)

    ## Perform more queries
    for (n in 4:6)
    {
        srv$APC_SYNTH %>% head(n) %>% run()
    }

    ## Check that the previous queries are now in the level 2
    ## cache
    tbl2 <- show_cache()
    expect_equal(tbl2 %>% nrow(), 6)
    expect_equal(tbl2 %>%
                 dplyr::filter(hash %in% !!tbl1$hash, !in_memory) %>%
                 nrow(),
                 3)
})

### Check the cache lifetime mechanism

test_that("check level 1 cache objects expires", {

    ## Generate artificial data
    gen_clean_apc("apc.db")

    ## Connect to the database
    srv <- server(config=system.file("extdata", "sqlite.yaml", package="icdb"))

    ## Enable the cache, manually set a level 1 size
    use_cache(TRUE, size = 2, lifetime = lubridate::dseconds(2))

    ## Clear the cache (in case cache/ is present)
    clear_cache()
    expect_equal(nrow(show_cache()), 0)
    
    ## Run the same query three times and check the hits is four
    for (n in 1:3)
    {
        srv$APC_SYNTH %>% head(1) %>% run()
    }
    tbl <- show_cache()
    expect_equal(tbl$hits[[1]], 3)

    ## Check that the cache object was last accessed at a
    ## different time to when it was first written
    expect_true(tbl$write_time[[1]] != tbl$last_access[[1]])
    
    ## Wait for 10 seconds so that the object expires
    Sys.sleep(5)

    ## Perform the same query again, expect hits to be equal to 1,
    ## 
    srv$APC_SYNTH %>% head(1) %>% run()
    tbl <- show_cache()
    expect_equal(tbl$hits[[1]], 1)

    ## Check that the write time and last access are equal
    expect_equal(tbl$write_time[[1]], tbl$last_access[[1]])

    
})

test_that("check level 2 cache objects expires", {

    ## Generate artificial data
    gen_clean_apc("apc.db")

    ## Connect to the database
    srv <- server(config=system.file("extdata", "sqlite.yaml", package="icdb"))

    ## Enable the cache, manually set a level 1 size
    use_cache(TRUE, size = 2, lifetime = lubridate::dseconds(2))

    ## Clear the cache (in case cache/ is present)
    clear_cache()
    expect_equal(nrow(show_cache()), 0)
    
    ## Run multiple queries multiple times (the first
    ## query will end up in the level 1 cache)
    srv$APC_SYNTH %>% head(1) %>% run()
    srv$APC_SYNTH %>% head(1) %>% run()
    srv$APC_SYNTH %>% head(2) %>% run()
    srv$APC_SYNTH %>% head(2) %>% run()
    srv$APC_SYNTH %>% head(3) %>% run()
    srv$APC_SYNTH %>% head(3) %>% run()
    tbl <- show_cache()
    expect_equal(tbl$hits[[1]], 2)

    ## Check that the cache object was last accessed at a
    ## different time to when it was first written
    expect_true(tbl$write_time[[1]] != tbl$last_access[[1]])
    
    ## Wait for 10 seconds so that the object expires
    Sys.sleep(5)

    ## Perform the same query again, expect hits to be equal to 1,
    ## 
    srv$APC_SYNTH %>% head(1) %>% run()
    tbl <- show_cache()
    expect_equal(tbl$hits[[1]], 1)

    ## Check that the write time and last access are equal
    expect_equal(tbl$write_time[[1]], tbl$last_access[[1]])

    
})



