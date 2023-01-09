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

## Randomly choose the rows at HBR
hbr <- tibble(hbr = rbinom(n = n, size = 1, prob = p_hbr))

## Of the HBR rows, randomly generate major criteria according
## to the proportions above. For non-HBR rows, give no major
## critera
hbr <- hbr %>%
    mutate(anemia = rbinom(n=n, size=1, p=hbr*p_anemia_hbr)) %>%
    mutate(oac = rbinom(n=n, size=1, p=hbr*p_oac_hbr)) %>%
    mutate(malignancy = rbinom(n=n, size=1, p=hbr*p_malignancy_hbr)) %>%
    mutate(ckd = rbinom(n=n, size=1, p=hbr*p_ckd_hbr)) %>%
    mutate(surgery = rbinom(n=n, size=1, p=hbr*p_surgery_hbr))

## Generate bleed occurance based on bleeding risk
hbr <- hbr %>%
    mutate(bleeding_risk = if_else(hbr == 0, p_bleed_base, p_bleed_hbr)) %>%
    mutate(bleed_occured = rbinom(n = n, size = 1, p = bleeding_risk))

## Show correlations. Note the low correlation between bleeding risk and
## whether a bleed occured, due purely to the proportions involved.
cc <-cor(hbr)
corrplot(cc)

## Check the proportions of major criteria in the HBR group
## (should agree with the numbers above)
print("In the HBR group")
hbr %>% filter(hbr == 1) %>% summary()
print("In the non-HBR group")
hbr %>% filter(hbr == 0) %>% summary()

## Convert columns to factors
hbr$bleed_occured <- factor(hbr$bleed_occured)
levels(hbr$bleed_occured) <- c("no_bleed", "bleed_occured")
hbr$hbr <- factor(hbr$hbr)
levels(hbr$hbr) <- c("not_hbr", "is_hbr")


hbr_fact$bleed_occured <- factor(hbr$bleed_occured, levels=c("no_bleed", "bleed_occured"))

mutate(bleed_occured = )
levels(hbr_fact$bleed_occured) <- c("no_bleed", "bleed_occured")
hbr_fact <- hbr %>%
    mutate(bleed_occured = factor(bleed_occured))
levels(hbr_fact$bleed_occured) <- c("no_bleed", "bleed_occured")


## In this dataset, The HBR column is assumed to reflect the truth
## about who is at high bleeding risk. Being at high bleeding
## risk is then modelled as having a raised probability of bleeding
## from the baseline 3.2% to the higher risk 9.1%. Conceptually,
## bleeding risk depends deterministically on patient characteristics,
## if enough characteristics were accounted for. In reality, it is not
## possible to realise the determinsitic mapping between patient
## characteristics and bleeding risk. By choosing a restricted set
## of input predictors, risk will no longer depend deterministically
## on the predictors. Depending on the quality of the predictors, an
## how well they really map to the causes of bleeding risk, the
## dependence of bleeding risk on the predictors will be more or
## less deterministic

## In this simulated dataset, the assumed true bleeding risk is
## contained in the bleeding_risk column (or the HBR column). It
## takes only two values: an HBR value; and a non-HBR value. These
## values may be predictable from patient characteristics, if they
## depend sufficiently determinstically on them.

## First, use logistic regression to predict high-bleeding risk
## from the input predictors.

## Use 10-fold cross validation
ctrl <- trainControl(summaryFunction = twoClassSummary,
                     method = "cv",
                     number = 10,
                     classProbs = TRUE,
                     savePredictions = TRUE)
lr_hbr <- train(hbr[,2:6], y = hbr$hbr,
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

## Show the area under the ROC curve with a confidence interval
auc(roc_curve)
ci(roc_curve)

## Plot the ROC curve
plot(roc_curve, legacy.axes = TRUE)

## Predict the class from the LR model
dataset$lr_bleed <- predict(lr_full, new_data = dataset)

## Plot the confusion matrix 
confusionMatrix(prediction, dataset$bleed)


























