## Load icdb before running this script
library(lubridate)
library(tidyverse)

use_cache(TRUE, lifetime = ddays(30))

msrv <- mapped_server("xsw")

msrv$swd$swd_attribute %>%
    select(nhs_number) %>%
    run() %>%
    drop_na() %>%
    nrow()

msrv$swd$attr_h %>%
    select(nhs_number) %>%
    run() %>%
    nrow()
    
