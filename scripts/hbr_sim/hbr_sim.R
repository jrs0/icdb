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
library(corrplot)
library(caret)
library(pROC)
library(ggplot2)

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

## Probability of bleed occurance for the baseline
## population (baseline bleeding risk), vs probability
## of bleed occurance for HBR
p_bleed_base <- 0.032
p_bleed_hbr <- 0.091

set.seed(1023)

## Generate normally distributed ages
hbr <- tibble(age = rnorm(n = n, mean = 75, sd = 7))

## Generate HBR based deterministically on age
hbr <- hbr %>%
    mutate(hbr = if_else(age >= 75, 1, 0))

## Add noise, so that HBR does not depend completely
## deterministically on age. This models a more
## realistic scenario, where there are other hidden
## variables (determinsitically) affecting bleeding
## risk
random_hbr <- rbinom(n = n, size = 1, prob = 0.05)
hbr <- hbr %>% mutate(hbr = xor(hbr, random_hbr))

## Convert to factor
hbr$hbr <- factor(hbr$hbr)
levels(hbr$hbr) <- c("not_hbr", "is_hbr")

## Plot age distribution
ggplot(hbr, aes(x=age, fill=hbr)) + geom_density(alpha = 0.3)

## Generate bleed occurance based on bleeding risk
hbr <- hbr %>%
    mutate(bleeding_risk = if_else(hbr == "not_hbr", p_bleed_base, p_bleed_hbr)) %>%
    mutate(bleed_occured = rbinom(n = n, size = 1, p = bleeding_risk))

## Convert columns to factors
hbr$bleed_occured <- factor(hbr$bleed_occured)
levels(hbr$bleed_occured) <- c("no_bleed", "bleed_occured")

## In this dataset, The HBR column is assumed to reflect the truth
## about who is at high bleeding risk. Being at high bleeding
## risk is then modelled as having a raised probability of bleeding
## from the baseline 3.2% to the higher risk 9.1%. Conceptually,
## bleeding risk depends deterministically on patient characteristics,
## if enough characteristics were accounted for. Here, this is modelled
## by having bleeding risk depend exactly on age -- if age >= 75, then
## the is_hbr flag is set.

## First, use logistic regression to predict high-bleeding risk
## from the input predictors.

## Use 10-fold cross validation
ctrl <- trainControl(summaryFunction = twoClassSummary,
                     method = "cv",
                     number = 10,
                     classProbs = TRUE,
                     savePredictions = TRUE)
lr_hbr <- train(hbr[,1], y = hbr$hbr,
                method = "glm",
                metric = "ROC",
                trControl = ctrl)

## View the summary, look for ROC area, which is the average
## over the n folds of the cross validation. The paper states
## that 
lr_hbr

## Predict the class probabilities and add the probability of
lr_hbr_pred_prob <- predict(lr_hbr, new_data = hbr, type = "prob")
hbr$lr_hbr_prob <- lr_hbr_pred_prob[,"is_hbr"]

## Plot the ROC curve for the 
roc_curve <- roc(response = hbr$hbr,
                 predictor = hbr$lr_hbr_prob)
auc(roc_curve)
ci(roc_curve)
plot(roc_curve, legacy.axes = TRUE)

## Predict the bleeding risk (HBR or not-HBR) from the
## lr_hbr model, and plot the confusion matrix
hbr$lr_hbr_pred <- predict(lr_hbr, new_data = hbr)
confusionMatrix(hbr$lr_hbr_pred, hbr$hbr)

## Now repeat the analysis attempting to predict bleeding. It
## is not possible to predict bleeding, because it is a random
## event (the probability ranges from 3% - 10%). It is also
## not possible to use bleeding events as a proxy for bleeding
## risk, because the difference between the probabilities for
## HBR and non-HBR is too low to distinguish in this sample size
lr_bleed <- train(hbr[,1], y = hbr$bleed_occured,
                method = "glm",
                metric = "ROC",
                trControl = ctrl)

## View the summary, look for ROC area, which is the average
## over the n folds of the cross validation. The paper states
## that 
lr_bleed

## Predict the class probabilities and add the probability of
lr_bleed_pred_prob <- predict(lr_bleed, new_data = hbr, type = "prob")
hbr$lr_bleed_prob <- lr_bleed_pred_prob[,"bleed_occured"]

## Plot the ROC curve for the 
roc_curve <- roc(response = hbr$bleed_occured,
                 predictor = hbr$lr_bleed_prob)
auc(roc_curve)
ci(roc_curve)
plot(roc_curve, legacy.axes = TRUE)

## Attempt to predict bleeding events from the
## model. This model predicts all events as non
## bleeds.
hbr$lr_bleed_pred <- predict(lr_bleed, new_data = hbr)
confusionMatrix(hbr$lr_bleed_pred, hbr$bleed_occured)

## The real question is what is the value of predicting
## bleeding as a proxy for predicting HBR, the real
## quantity of interest
roc_curve <- roc(response = hbr$hbr,
                 predictor = hbr$lr_bleed_prob)
auc(roc_curve)
ci(roc_curve)
plot(roc_curve, legacy.axes = TRUE)

table(hbr$hbr, hbr$bleed_occured)

























