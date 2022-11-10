##'
##'
NULL

##' Generate a synthetic table of admitted patient care
##' data and save it to an SQLite database file.
##'
##' @title Generate a synthetic admitted patient care database
##' @param filename The name of the sqlite database file in which
##' to store the resulting database. The file will be placed in a
##' folder gendata/ in the working directory, which will be
##' created if it does not exist. Any files in gendata/ with the
##' same name will be overwritten. The filename can be anything,
##' but you may wish to give it an extension .db or .sqlite for
##' clarity
##' @param seed The seed to use for data generation. If all other
##' parameters are equal (excluding filename), then using the
##' same seed is guaranteed to produce the same dataset.
##' @param nspells The number of spells to include in the dataset
##'
##' @export
gen_clean_apc <- function(filename, seed = 1, nspells = 10)
{
    stopifnot(nspells >= 1)
    
    ## Create the gendata/ folder if it does not exist
    if (!dir.exists("gendata"))
    {
        dir.create("gendata")
    }

    ## Set the seed so that subsequent operations are repeatable
    set.seed(seed)

    ## Generate the number of episodes for each spell
    ## Take abs to make positive and add 1 so that >= 1
    neps <- abs(as.integer(rnorm(nspells, sd=5))) + 1

    ## Store the length of the table
    N <- sum(neps)
    
    ## Generate a random NHS number (not the real format)
    nhs_numbers <- as.character(as.integer(900000000 + runif(nspells, min=0, max=100000)))

    ## Create the NHS number columns
    nhs_num_col <- rep(nhs_numbers, neps)

    ## Create the spell ID column
    spell_id_col <- rep(1:nspells, neps)
    
    ## Generate the ICD codes (treating each episode as independent)
    icd10 <- c("I210", "I211", "I212", "I213", "I220", "I221", "I228", "I214", 
               "I219", "I222", "I229", "I240", "I248", "I249", "I200",
               "S066", "I600", "I601", "I602", "I603", "I604", "I605", "I606", 
               "I607", "I608", "I609", "I60", "I60-", "S063", "I610", "I611", 
               "I612", "I613", "I614", "I615", "I616", "I618", "I619", "I62", 
               "I62-", "I61", "I61-", "I629", "I850", "I983", "K226", "K250", 
               "K252", "K254", "K256", "K290", "K2901", "K2921", "K2931", "K2941", 
               "K2951", "K2961", "K2971", "K2981", "K2991", "K260", "K262", 
               "K264", "K266", "K270", "K272", "K274", "K276", "K280", "K282", 
               "K284", "K286", "K5701", "K5711", "K5713", "K5731", "K5733", 
               "K5741", "K5751", "K5753", "K5781", "K5791", "K5793", "K920", 
               "K921", "K922")
    diagnosis_col <- sample(icd10, N, replace=TRUE)

    ## Generate random start and end times for each episode. Episodes
    ## do not overlap in this example, and each one starts when the previous
    ## one ends
    start_date <- lubridate::ymd("2018-1-1")
    end_date <- lubridate::ymd("2020-1-1")
    spell_starts <- lubridate::as_datetime(
                                   runif(nspells,
                                         as.numeric(as.POSIXct(start_date)),
                                         as.numeric(as.POSIXct(end_date)))) %>%
        as.character()
    
    ## ep_durations <- abs(20 + rnorm(5, sd = 300)) %>% lubridate::make_difftime(units="minutes")
    ## ep_ends <- ep_starts + ep_durations

    spell_starts_col <- rep(spell_starts, neps)
    
    ## Make the data frame with the episode data
    tbl <- tibble::tibble(NHSNumber = nhs_num_col,
                          PrimaryDiagnosis_ICD = diagnosis_col,
                          HospitalProviderSpellIdentifier = spell_id_col,
                          StartTime_HospitalProviderSpell = spell_starts_col)
    
    ## Create the database
    path <- paste0("gendata/", filename)
    con <- DBI::dbConnect(RSQLite::SQLite(), path)

    ## In case the file already exists and the table exists, remove the table
    if (DBI::dbExistsTable(con, "APC_SYNTH")) {
        DBI::dbRemoveTable(con, "APC_SYNTH")
    }

    ## Make a new table from the data frame
    DBI::dbWriteTable(con, name="APC_SYNTH",
                       value = tbl, overwrite = TRUE)

    
    DBI::dbDisconnect(con)
    message("Written generated APC data to '", path, "'")
}
