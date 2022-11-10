check_digit <- function(digits)
{
    ## Weights used in algorithm
    weights <- c(10,9,8,7,6,5,4,3,2)

    ## Scale the digits by the weights and add up
    total <- sum(weights * digits)

    ## Divide by 11 to get check digit
    11 - (total %% 11)
}

##' This function makes a random, invalid, 10-digit NHS number
##' and returns it as a character.
##'
##' An NHS number is a 10-digit numeric identifier that
##' satisfies a checksum condition.
##'
##' @title Make a random NHS number
##' @return The NHS number as a character vector
##'
##' @export
random_nhs_number <- function()
{
    ## Generate 9 random digits to use
    digits <- sample(10,9) - 1

    check <- check_digit(digits)

    ## If the check digit is three, use four. Otherwise
    ## use three. This will always result in an invalid
    ## NHS number (do not use zero, which will be valid
    ## if the check comes out 11). The choice here is
    ## arbitrary -- there are many ways to make an invalid
    ## NHS number
    if (check == 3)
    {
        check <- 4
    }
    else
    {
        check <- 3
    }

    ## Generate and return the number
    paste0(c(digits, as.character(check)), collapse = "")
}

nhs_number_valid <- function(nhs_number)
{
    digits <- as.numeric(strsplit(as.character(nhs_number), "")[[1]])

    check <- check_digit(digits[-10])

    if (check == 10)
    {
        ## If the check digit is 10, the NHS number is invalid
        FALSE
    }
    else if (check == 11)
    {
        ## THe NHS number is valid if the check digit in the
        ## number is 0
        digits[[10]] == 0
    }
    else
    {
        ## The NHS number is valid if the check digit agrees
        ## with the NHS number
        digits[[10]] == check
    }
    
}
