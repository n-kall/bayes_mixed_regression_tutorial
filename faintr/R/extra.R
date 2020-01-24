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


make_combination <- function(factor_values, factor_info) {
    # create a string for the combination of factor levels
    combination <- ""
    ref_count = 0    
    for (fct in names(factor_values)) {
        # check for reference level
        if (factor_info[[fct]]$reference == factor_values[[fct]]) {
            ref_count  <- ref_count + 1
            factor_str <- ""
        } else {
            factor_str <- paste0(fct, factor_values[[fct]])
        }
        
        if (length(factor_values) > 1) {
            combination <- paste(factor_str, combination, sep = ":")
        } else {
            combination <- paste0(factor_str, combination)
        }
    }
    
    if (ref_count == length(factor_values)) {
        parameter_str <- "Intercept"
    } else if (length(factor_values) > 1) {
       # remove the trailing colon
        combination  <- combination %>%
            str_remove(":*$") %>%
            str_remove("^:*")       
    }
}


compare_cells <- function(model, higher, lower, alpha = 0.05) {
    # create factor combination strings and run hypothesis

    factor_info <- get_factor_information(model)

    higher_str <- make_combination(higher, factor_info)
    
    lower_str <- make_combination(lower, factor_info)

    brms::hypothesis(model, paste(higher_str, ">", lower_str), alpha = alpha)
}
