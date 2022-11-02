##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
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
gen_apc <- function(filename, seed = 1, nspells = 10)
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
               "I219", "I222", "I229", "I240", "I248", "I249", "I200")
    diagnosis_col <- sample(icd10, N, replace=TRUE)

    ## Generate random start and end times for each episode, in sequence
    
    
    ## Make the data frame with the episode data
    df <- tibble::tibble(NHSNnmber = nhs_num_col,
                         DiagnosisICD = diagnosis_col,
                         SpellID = spell_id_col)
    
    ## Create the database
    con <- DBI::dbConnect(RSQLite::SQLite(), paste0("gendata/",filename))

    ## In case the file already exists and the table exists, remove the table
    if (DBI::dbExistsTable(con, "APC_SYNTH")) {
        DBI::dbRemoveTable(con, "APC_SYNTH")
    }

    ## Make a new table from the data frame
    DBI::dbWriteTable(con, name="APC_SYNTH",
                       value = df, overwrite = TRUE)

    
    DBI::dbDisconnect(con)
}
