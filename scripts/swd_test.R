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
    

msrv$sus$outpatient %>%
    mutate(attendance_status = case_when(
               attendance_status == 5 ~ "attended_on_time",
               attendance_status == 6 ~ "attended_late_was_seen",
               attendance_status == 7 ~ "attended_late_not_seen",
               attendance_status == 2 ~ "canceled_by_patient",
               attendance_status == 3 ~ "did_not_attend",
               attendance_status == 4 ~ "canceled_by_provider",
               TRUE ~ "unknown"
           ))
