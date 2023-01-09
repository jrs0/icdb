##' This script creates a simulated dataset of high bleeding risk
##' based on the numbers in the paper "2020 Cao et al. - Validation of the
##' Academic Research Consortium High Bleeding Risk Definition in
##' Contemporary PCI Patients". The purpose is to demonstrate the difference
##' between simulating bleeding risk (an event probability that depends
##' on certain bleeding risk factors) and simulating bleeding events
##' themselves. It is not possible to simulate bleeding events under
##' this event model (where bleeding event probability is equal to
##' bleeding risk) because the occurance of bleeding does not depend
##' deterministically (or nearly-deterministically) on the input
##' categories
##' 

library(tidyverse)

## Total number of patients
n <- 10000

## Proportion of population at high bleeding risk (HBR)
p_hbr <- 0.444

## Proportion of various major criteria, given HBR
p_anemia_hbr <- 0.332
p_oac_hbr <- 0.185
p_malignancy_hbr <- 0.168
p_ckd_hbr <- 0.137
p_surgery_hbr <- 0.082

set.seed(1023)

## Randomly choose the rows at HBR
hbr <- tibble(hbr = rbinom(n = n, size = 1, prob = p_hbr))

## Of the HBR rows, randomly generate major criteria according
## to the proportions above. For non-HBR rows, give no major
## critera
hbr <- hbr %>%
    mutate(anemia = rbinom(n=n, size=hbr, p=p_anemia_hbr)) %>%
    mutate(oac = rbinom(n=n, size=hbr, p=p_oac_hbr)) %>%
    mutate(malignancy = rbinom(n=n, size=hbr, p=p_malignancy_hbr)) %>%
    mutate(ckd = rbinom(n=n, size=hbr, p=p_ckd_hbr)) %>%
    mutate(surgery = rbinom(n=n, size=hbr, p=p_surgery_hbr))





























