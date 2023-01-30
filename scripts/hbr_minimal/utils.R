##' @title Get ROC curves for the cross-validation folds
##' @return A tibble containing columns for the sensitivities,
##' specificities, and cross-validation fold in the label column
get_cv_roc <- function(fit)
{
    ## From "https://stackoverflow.com/questions/69246553/
    ## plot-the-average-cross-validated-auc-from-caret-package"
    ##
    ## sapply roc(), bind as tibble with Resample as .id
    tbl <- sapply(X = unique(fit$pred$Resample),
           FUN = function(x) {
               r <- fit$pred[fit$pred$Resample == x,]
               R <- roc(response = r$obs, predictor = r$bleed_occured)
               data.frame(sensitivities = R$sensitivities,
                          specificities = R$specificities)
           }, simplify = F) %>%
        bind_rows(.id = "Resample") %>%
        as_tibble() %>%
        rename(label = Resample)
}

##' @title Add predicted class probabilities to the data
##' 
##' @param data A tibble containing the predictors used in fit
##' and the response column 
##' @param fit The model to use
##' @param response_name The string name of the column to use as
##' the response (a factor)
##' @param positive_event Which level of response to use as the
##' positive event for the purpose of probabilities
##' @param response The response column to predict. The argument is
##' a string. 
##' @return A tibble containing the new columns response_prob
##' for the prediction probabilities
##' 
add_prediction_probs <- function(data, fit, response, positive_event)
{

    ## What is going on here? Apparently putting paste0 into the mutate
    ## does not work
    response_prob <- paste0(response, "_prob") 
    data[[response_prob]] = predict(fit, newdata = data,
                                    type = "prob")[,positive_event]
    data
}

##' @title Get ROC curve data from predictions and prediction probabilities
##' @param data The tibble containing the response and response_prob columns
##' @param response The name of the response column
##' @param label What label to assign to the ROC data
##' @return A tibble containing sensitivies, specificities, and a label for
##' binding to other ROC data.
get_roc <- function(data, response, label)
{
    response_prob <- paste0(response, "_prob")
    roc <- roc(response = data[[response]],
               predictor = data[[response_prob]])
    message("AUC (ROC) for '", response, "': ", auc(roc), " in '", label, "'")
    print(ci(roc))
    roc_test_tbl <- tibble(label = label,
                           sensitivities = roc$sensitivities,
                           specificities = roc$specificities)
}
