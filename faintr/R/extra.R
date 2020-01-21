library(brms)
library(tidyverse)

get_variables <- function(model) {
    # list all independent variables by parsing the brms formula

    # extract formula from fit
    formula <- model[["formula"]]

    # extract all variables from fit and remove tilde
    vars <- parse_bf(formula)[["allvars"]] %>%
        stringr::str_split(" ~ ", simplify = T)

    # split predictors and remove pluses
    predictors <- stringr::str_split(vars[[3, 1]], " \\+ ")[[1]] %>%
        str_split("\\:")
    print(predictors)
    predictors <- predictors[2:length(predictors)] %>%
        unlist() %>%
        unique()
    
    return(list(predicted = vars[[2, 1]],
                predictors = predictors))
}

get_factor_information <- function(model) {
    # return independent variables that are factors and their levels

    variables <- get_variables(model)

    predictors <- variables[["predictors"]]

    # data that the model was fit on
    data <- stats::model.frame(model)

    # get vector of names of factor predictors
    factor_predictors <- data %>%
        select(predictors) %>%
        select_if(is.factor) %>%
        names()

    # output levels for each of the predictor factors
    factor_info <- list()
    for (fac in factor_predictors) {
        lvls <- levels(data[[fac]])
        ref_lvl <- lvls[1]
        factor_levels <- list(levels = lvls,
                              reference = ref_lvl)
        
        factor_info[[fac]] <- factor_levels
    }

    return(factor_info)
}

compare_cells <- function(model, higher, lower, alpha = 0.05) {

    factor_info <- get_factor_information(model)

    higher_str <- ""
    higher_ref_count = 0
    
    for (fct in names(higher)) {
        # check for reference level
        if (factor_info[[fct]]$reference == higher[[fct]]) {
            higher_ref_count  <- higher_ref_count + 1
            factor_str <- ""
        } else {
            factor_str <- paste0(fct, higher[[fct]])
        }
        
        if (length(higher) > 1) {
            higher_str <- paste(factor_str, higher_str, sep = ":")
        } else {
            higher_str <- paste0(factor_str, higher_str)
        }
    }

    
    if (higher_ref_count == length(higher)) {
        higher_str <- "Intercept"
    } else if (length(higher) > 1) {
        # remove the trailing colon
        higher_str  <- higher_str %>%
            str_replace(":*$", "")

    }


    lower_str <- ""
    lower_ref_count = 0
    
    for (fct in names(lower)) {
        # check for reference level
        if (factor_info[[fct]]$reference == lower[[fct]]) {
            lower_ref_count  <- lower_ref_count + 1
            factor_str <- ""
        } else {
            factor_str <- paste0(fct, lower[[fct]])
        }
        
        if (length(lower) > 1) {
            lower_str <- paste(factor_str, lower_str, sep = ":")
        } else {
            lower_str <- paste0(factor_str, lower_str)
        }
    }

    
    if (lower_ref_count == length(lower)) {
        lower_str <- "Intercept"
    } else if (length(lower) > 1) {
        # remove the trailing colon
        lower_str  <- lower_str %>%
            str_replace(":*$", "")

    }

    brms::hypothesis(model, paste(higher_str, ">", lower_str), alpha = alpha)
}
